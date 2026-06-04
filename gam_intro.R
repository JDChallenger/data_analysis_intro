#useful reference: https://noamross.github.io/gams-in-r-course/

library(ggplot2)
library(lme4)
library(mgcv)

#The data here is simulated data for mosquito 
# collections, to estimate the Human Biting Rate (HBR)
df <- read.csv('data/HBR_data.csv')

head(df)

table(df$Village)

#notice 'day of year' resets for each year, so we might want another time variable 
# here

df$day <- df$day_of_year + 365 * (df$year - 2021)

#For now, let's look at data from one village (Village_5)

df2 <- df[df$Village=='Village_5',]

#Here, 'y' is the number of mosquitoes caught per trap per night
table(df2$y)

ggplot(df2) + geom_point(aes(x = day, y = y)) + theme_classic()

#Try a linear model. (we could also write this as a poisson model, for count data, as a glm...)
m1 <- lm(y ~ day, data = df2)
summary(m1)

#Let's add the model to the plot
ggplot(df2) + geom_point(aes(x = day, y = y)) + theme_classic() + 
  geom_function(fun = function(x) m1$coefficients[[1]] + m1$coefficients[[2]] * x,
                color = 'magenta')

#Let's try a gam model
mod_g <- gam(y ~ s(day, k = 10), data = df2, family = poisson, method = "REML")
summary(mod_g)

#Things to look for in the summary: the edf for each spline (here we just have one spline, for 'day')
# if edf is equal to 1 (or close to 1), this indicates that the spline is (more-or-less) linear.
# This probably indicates that we can include it in the model as a linear term,
# and we don't need to use the spline. 

#We can also plot the spline (note: this is on the model scale )
plot(mod_g, rug = T)

# The function gam.check() can help us to see if k is too low
# if the k-index is below 1 and the edf is close to k', then 
# it could be useful to increase the value of k, and re-fit the model
# Note: inside gam.check, a significant p value indicates k is too low,
# it doesn't tell us about whether a term should be included in the model...
gam.check(mod_g)

#Now, we try to use the predict function to extract model predictions
min(df2$day)
max(df2$day)
new_dat <- data.frame('day' = seq(99,1025))
yy <- predict(mod_g, newdata = new_dat, type = 'response')

new_dat$pred <- yy

ggplot(df2) + geom_point(aes(x = day, y = y)) + theme_classic() + 
  geom_line(data = new_dat, aes(x = day, y = pred), color = 'magenta')

#For the 95% CIs, we'll need the predictions on the 'link' (model) scale
yy2 <- predict(mod_g, newdata = new_dat, type = 'link', se.fit = T)

new_dat$pred_ci1 <- exp(yy2$fit - 1.96*yy2$se.fit)
new_dat$pred_ci2 <- exp(yy2$fit + 1.96*yy2$se.fit)

ggplot(df2) + geom_point(aes(x = day, y = y)) + theme_classic() + 
  geom_line(data = new_dat, aes(x = day, y = pred), color = 'magenta') + 
  geom_ribbon(data = new_dat, aes(x = day, ymin = pred_ci1, ymax = pred_ci2), 
              fill = 'magenta', alpha = .3) + xlab('Day') + 
  ylab('Mosquitoes caught per trap')



## Utilising information on seasonality

#So far, we are not utilising seasonality in the model- the gam() does not know 
# that we have data from 3 years. We can change this, using a periodic spline,
# by setting bs = 'cc', like this: 

#Notice that I am using 'day_in_year' now!
mod_g2 <- gam(y ~ s(day_of_year, k = 11, bs = 'cc'), data = df2, family = poisson, method = "REML")
summary(mod_g2)

#Now let's use predict(). It'll be useful to have 'day' here too, for plotting
new_dat2 <- data.frame('day' = seq(99,1025))
new_dat2$day_of_year <- new_dat2$day %% 365

#View the data. What does the '%%' operator do?
#View(new_dat2)

yy2 <- predict(mod_g2, newdata = new_dat2, type = 'response')

new_dat2$pred <- yy2

# Now plot: 
ggplot(df2) + geom_point(aes(x = day, y = y)) + theme_classic() + 
  geom_line(data = new_dat2, aes(x = day, y = pred), color = 'magenta')

# How strange! This is not what we want: because we are missing data from the 
# dry season, the model has not correctly inferred the periodicity.

#Fortunately, we can provide the information to the model
#Period is from 0 to 365
mod_g3 <- gam(y ~ s(day_of_year, k = 7, bs = 'cc'), data = df2, 
              family = poisson, knots = list(day_of_year = c(0,365)),
              method = "REML")
summary(mod_g3)

yy2 <- predict(mod_g3, newdata = new_dat2, type = 'response')

new_dat2$pred <- yy2

# Now plot: 
ggplot(df2) + geom_point(aes(x = day, y = y)) + theme_classic() + 
  geom_line(data = new_dat2, aes(x = day, y = pred), color = 'magenta')


#Ok, now the model knows about the seasonality.
# However, it is not necessarily the case that we expect the same numbers of 
# mosquitoes each year. Let's add that information to the model:

mod_g4 <- gam(y ~ factor(year) + s(day_of_year, k = 7, bs = 'cc'), data = df2, 
              family = poisson, knots = list(day_of_year = c(0,365)), method = "REML")
summary(mod_g4)

#We see that there are some significant differences!
# Let's make some predictions
# It'll be useful to have 'day' here too, for plotting
new_dat3 <- data.frame('day' = seq(99,1025))
new_dat3$day_of_year <- new_dat3$day %% 365

new_dat3$year <- 2021
new_dat3[new_dat3$day > 365 & new_dat3$day <= 730 ,]$year <- 2022
new_dat3[new_dat3$day > 730,]$year <- 2023
new_dat3$year <- as.factor(new_dat3$year)

yy3 <- predict(mod_g4, newdata = new_dat3, type = 'response')

new_dat3$pred <- yy3

# Now plot: 
# Now we see the model expects different magnitudes of mosquito population
# each year.
ggplot(df2) + geom_point(aes(x = day, y = y)) + theme_classic() + 
  geom_line(data = new_dat3, aes(x = day, y = pred), color = 'magenta')

ggplot(df) + geom_point(aes(x = day, y = y, color = District)) + theme_classic() + 
  geom_line(data = new_dat3, aes(x = day, y = pred), color = 'magenta')

