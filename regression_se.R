library(lme4)
library(MASS)
library(ggplot2)

################################################################################
################# 1 . Linear regression models ####################
################################################################################

# These models take the form y ~ x, i.e. we want to use information about x ('the independent variable') to
# explain the variation we see in y ('the dependent variable').
# For a linear model, y must take continuous values, whilst x can be continuous or discrete.

Howell1 <- readRDS('data/training_data.rds') #load data
head(Howell1)

#Let's look at data from adults only ( >=18 years)
d <- Howell1[Howell1$age>=18,]

#Let's plot the heights & weights of the people in the study
ggplot(d) + geom_point(aes(x=height, y=weight),color= 'red', shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)')

# From the data, we can see a relationship between height and weight.
# Can we fit a linear model to the data?

#This model seeks to explain variation in weight in terms of individual's height
mod_l1 <- lm(weight ~ height, data = d)
summary(mod_l1)
#Here are the coefficients for the model:
mod_l1$coefficients

# Note: the interpretation of the intercept here is the expected weight of someone 0cm tall!
# This is not very intuitive.
# Instead, we will create a new variable 'height2', which is the deviation from the average height

Hbar = mean(d$height)
d$height2 <- d$height - Hbar

mod_l2 <- lm(weight ~ height2, data = d)
summary(mod_l2)

#Let's add the model to the plot.
#To do this, we will use geom_function() to plot the line y = a + b*x
# This has the form: geom_function(fun = function(x) a + b*x)

#Note: it doesn't matter which model we use - mod_l1 or mod_l2
#Here is the plot with model_l1
ggplot(d) + geom_point(aes(x=height, y=weight),color= 'red', shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_function(fun = function(x) mod_l1$coefficients[[1]] + mod_l1$coefficients[[2]] * x)

#Here is the plot with model_l2. Notice we have to include Hbar inside geom_function()
ggplot(d) + geom_point(aes(x=height, y=weight),color= 'red', shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_function(fun = function(x) mod_l2$coefficients[[1]] + mod_l2$coefficients[[2]] * (x - Hbar) )

#But remember, the data also give us information on biological sex (`male = 0`, or male = `1`)
ggplot(d) + geom_point(aes(x=height, y=weight,color= factor(male)), shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)')

#Perhaps the relationship between weight & height is different for men and women?
# In this model, we allow the intercept to change according to sex
mod_l3 <- lm(weight ~ height2 + male, data = d)
summary(mod_l3)

# Question: Notice that the coefficient for male is negative. What does this mean???

# Let's plot the models. Now we need to plot two lines, one for men and one for women
# Remember: because we wrote the model in terms of 'height2', we have to adjust the code for the slope inside geom_function()
# Note: the two lines are very similar
ggplot(d) + geom_point(aes(x=height, y=weight,color= factor(male)), shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_function(fun = function(x) mod_l3$coefficients[[1]] + mod_l3$coefficients[[2]] * (x - Hbar), color = 'red') + # for women
  geom_function(fun = function(x) mod_l3$coefficients[[1]] +
                  mod_l3$coefficients[[3]] + mod_l3$coefficients[[2]] * (x - Hbar), color = 'turquoise') # for men

#Let's tidy the labels
ggplot(d) + geom_point(aes(x=height, y=weight,color= factor(male)), shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_function(fun = function(x) mod_l3$coefficients[[1]] + mod_l3$coefficients[[2]] * (x - Hbar), color = 'red') + # for women
  geom_function(fun = function(x) mod_l3$coefficients[[1]] +
                  mod_l3$coefficients[[3]] + mod_l3$coefficients[[2]] * (x - Hbar), color = 'turquoise') + # for men
  scale_color_manual(values = c('red','turquoise'), name = '', labels = c('Women','Men'))

# However, it is possible that both the intercept and the slope are different for men & women
# To model this, we use an interaction between height & sex. In the
# lme4 package, an interaction term is indicated using the symbol '*'

mod_l4 <- lm(weight ~ height2 * male, data = d)
summary(mod_l4)

# here are the model coefficients
mod_l4$coefficients

# Now the model is a bit more complicated. Here is how to interpret the results.
# What is the model intercept for women? (`male = 0`)
mod_l4$coefficients[[1]]

# What is the model slope for women? (`male = 0`)
mod_l4$coefficients[[2]]

# What is the model intercept for men? (`male = 1`)
mod_l4$coefficients[[1]] + mod_l4$coefficients[[3]]

# What is the model slope for men? (`male = 1`)
mod_l4$coefficients[[2]] + mod_l4$coefficients[[4]]

# Question: can you plot the 2 relationships between weight & height (with the data), one for men, and one for women?

ggplot(d) + geom_point(aes(x=height, y=weight,color= factor(male)),
                       shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_function(fun = function(x) mod_l4$coefficients[[1]] + 
                  mod_l4$coefficients[[2]] * (x - Hbar), color = 'red') + # for women
  geom_function(fun = function(x) mod_l4$coefficients[[1]] +
                  mod_l4$coefficients[[3]] + 
          (mod_l4$coefficients[[2]] + mod_l4$coefficients[[4]]) * (x - Hbar),
        color = 'turquoise') # for men

#Let's tidy the labels
ggplot(d) + geom_point(aes(x=height, y=weight,color= factor(male)), shape = 1) +
  theme_classic() + xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_function(fun = function(x) mod_l4$coefficients[[1]] + mod_l4$coefficients[[2]] * (x - Hbar), color = 'red') + # for women
  geom_function(fun = function(x) mod_l4$coefficients[[1]] +
                  mod_l4$coefficients[[3]] + (mod_l4$coefficients[[2]] + mod_l4$coefficients[[4]]) * (x - Hbar),
                color = 'turquoise') + # for men
  scale_color_manual(values = c('red','turquoise'), name = '', labels = c('Women','Men'))

# Question: What weight does the model predict for a woman of height 150cm?
mod_l4$coefficients[[1]] + mod_l4$coefficients[[2]]*(150 - Hbar)

# Question: What weight does the model predict for a man of height 150cm?
mod_l4$coefficients[[1]] + mod_l4$coefficients[[3]] +
  (mod_l4$coefficients[[2]] + mod_l4$coefficients[[4]])*(150 - Hbar)

# Question: What weight does the model predict for a man of height 170cm?
mod_l4$coefficients[[1]] + mod_l4$coefficients[[3]] +
  (mod_l4$coefficients[[2]] + mod_l4$coefficients[[4]])*(170 - Hbar)

# In the code above, we have generated the model predictions manually.
# However, there is a simpler way! We can use the predict() function.

# We use the argument 'newdata' to describe the type of prediction we want to make.
# As an example, we repeat the question above ("What weight does the model predict for a woman of height 150cm?")

predict(mod_l4, newdata = data.frame('height2' = 150 - Hbar,
                                     'male' = 0))
# Does this match the answer we obtained manually?

# Remember: the regression model also provides us with an estimate of the
# uncertainty in the fitted parameters. We can use this to calculate the
# uncertainty present in our model prediction, using the argument 'se.fit = TRUE'

predict(mod_l4, newdata = data.frame('height2' = 150 - Hbar, 'male' = 0), se.fit = T)
predict(mod_l4, newdata = data.frame('height2' = 150 - Hbar, 'male' = 0), 
        interval = 'confidence', level = 0.95) # For 95% CI

# Compare the above with this:
predict(mod_l4, newdata = data.frame('height2' = 150 - Hbar, 'male' = 0),
    interval = 'prediction', level = 0.95)

# we can use the predict function to generate the predicted relationship between weight and height
# Remember, we have already plotted this above! But now we can include in the uncertainty in the
# prediction

#These are the predictions for women
pred_women <- predict(mod_l4, 
              newdata = data.frame('height2' = seq(135,170) - Hbar,
                                'male' = rep(0,36)),
                      interval = 'confidence', level = 0.95)
#Let's examine the contents of 'pred_women'
head(pred_women)
# When making the plot, it will be easier if 'pred_women' is a data.frame
pred_women <- as.data.frame(pred_women)
# Notice that we are missing a column for height here. Let's add it ourselves
pred_women$height <- seq(135,170)

#Let's plot the data for women, and the model prediction
# To show the model uncertainty, we will use 'geom_ribbon()'
ggplot(data = d[d$male==0,]) + theme_classic() + geom_point(aes(x=height, y=weight)) +
  geom_line(data = pred_women, aes(x = height, y = fit), color = 'purple3') +
  xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_ribbon(data = pred_women, 
              aes(x = height, ymin = lwr, ymax = upr),
              fill = 'purple3', alpha = .3) +
  ggtitle('Data and model predictions [Women only]')

#Question: can you generate the equivalent plot for men?

#These are the predictions for women
pred_men <- predict(mod_l4, 
                      newdata = data.frame('height2' = seq(135,180) - Hbar,
                                           'male' = rep(1,46)),
                      interval = 'confidence', level = 0.95)
#Let's examine the contents of 'pred_women'
head(pred_men)
# When making the plot, it will be easier if 'pred_women' is a data.frame
pred_men <- as.data.frame(pred_men)
# Notice that we are missing a column for height here. Let's add it ourselves
pred_men$height <- seq(135,180)

ggplot(data = d[d$male==1,]) + theme_classic() + 
  geom_point(aes(x=height, y=weight)) +
  geom_line(data = pred_men, aes(x = height, y = fit), color = 'orange') +
  xlab('Height (cm)') + ylab('Weight (kg)') +
  geom_ribbon(data = pred_men, 
              aes(x = height, ymin = lwr, ymax = upr),
              fill = 'orange', alpha = .3) +
  ggtitle('Data and model predictions [Men only]')




################################################################################
################ 2. Generalised linear regression models (GLMs) ################
################################################################################


# For other types of data (e.g. count data, data for proportions), we cannot use a
# linear model directly. Instead, we define a linear model on an appropriate 'model scale'.
# Then we make a non-linear transformation to the linear model. This is why these
# models are called 'Generalised Linear Models' (GLMs).



################################################################################
#############################  Poisson model   #################################
################################################################################

# As an example, let's fit a Poisson model to some synthetic data. We imagine
# an experiment to test the repellancy of mosquito nets. For three types of net
# (A control net with no insecticide, ITN1 and ITN2), we record how often mosquitoes
# touch the net in a 5-minute period. For each net we perform 25 replicates
# (i.e. we repeat the experiment using 25 mosquitoes).

count_data <- readRDS('ITN_count_data.rds')
head(count_data)

#Let's plot the data
ggplot(count_data) + theme_bw() +
  geom_jitter(aes(x = Net, y = Contacts, color = Net), width = 0.1, height = 0) +
  ylab('Number of contacts with the net') + theme(legend.position = 'none')

# Remember, the poisson distribution uses only one parameter (lambda). This parameter
# must be positive (lambda > 0 ). For our linear model, we need to use a model scale
# in which all values (positive or negative) are permissible. Therefore, we will use
# the log() function to provide our non-linear transformation between the data and the model.

#To fit the model, we use the glm() function. We write
# " family = poisson(link = "log") " to indicate that we use a log link function here
glm1 <- glm(Contacts ~ Net , data = count_data, family = poisson(link = "log") )
summary(glm1)

# Now we can extract some predictions for the average counts observed for each net.
# We have a choice here- do we want predictions on the model ("link") scale,
# or the data scale ("response")?

#predicitons on the model scale
predict(glm1, newdata = data.frame('Net' = c('Control','ITN1','ITN2')), type = 'link')

#Predictions on the data scale (in terms of mosquito counts)
predict(glm1, newdata = data.frame('Net' = c('Control','ITN1','ITN2')), type = 'response')

#Let's extract the predictions on the data ('response') scale and add them to the plot.
# do they make sense?
store_pred <- data.frame('Net' = c('Control','ITN1','ITN2'))
store_pred$values <- predict(glm1, newdata = data.frame('Net' = c('Control','ITN1','ITN2')), type = 'response')
store_pred

#now add the predictions to the plot
ggplot(count_data) + theme_bw() +
  geom_jitter(aes(x = Net, y = Contacts, color = Net), width = 0.1, height = 0) +
  ylab('Number of contacts with the net') + theme(legend.position = 'none') +
  geom_point(data = store_pred, aes(x = Net, y = values),size = 4, shape =2, color = 'black')

#New: now let's calculate the uncertainty in this prediction.
# On the model scale (selecting `type = 'link'`), we add the argument se.fit = T
# This gives us the standard error associated with each prediction, on the model scale
predict(glm1, newdata = data.frame('Net' = c('Control','ITN1','ITN2')), type = 'link', se.fit = T)
# save this output:
predict_with_se <- predict(glm1, newdata = data.frame('Net' = c('Control','ITN1','ITN2')),
                           type = 'link', se.fit = T)

#If we assume that the uncertainty in the predictions is normally distributed on the
# model scale, we can estimate the 95% confidence interval (CI) for the prediction.
# We note that, for normally distributed data, 95% of the data are found within
# 1.96 standard deviations of the mean

#Let's make a data frame to hold our predictions
store_pred <- data.frame('Net' = c('Control','ITN1','ITN2'))
# Here are the central estimates (on the model scale)
store_pred$values_model_scale <- predict_with_se$fit

#Now, using the normality assumption, we calculate the CI (again, on the model scale):
store_pred$values_model_scale_ci1 <- predict_with_se$fit - 1.96 * predict_with_se$se.fit
store_pred$values_model_scale_ci2 <- predict_with_se$fit + 1.96 * predict_with_se$se.fit

#now, we transform the predictions, using the inverse link function. This allows
# us to make the comparison with the data:
store_pred$values <- exp(store_pred$values_model_scale)
store_pred$values_ci1 <- exp(store_pred$values_model_scale_ci1)
store_pred$values_ci2 <- exp(store_pred$values_model_scale_ci2)

#now add the predictions to the plot.
# We use geom_errorbar() to show the uncertainty
ggplot(count_data) + theme_bw() +
  geom_jitter(aes(x = Net, y = Contacts, color = Net), width = 0.1, height = 0) +
  ylab('Number of contacts with the net') + theme(legend.position = 'none') +
  geom_point(data = store_pred, aes(x = Net, y = values), size = 4, shape =2, color = 'black') +
  geom_errorbar(data = store_pred, aes(x = Net, ymin = values_ci1, ymax = values_ci2),
                color = 'black', width = .2)

#Note: this model is quite simple. In particular, each experiment was the same duration (5 minutes).
# If there was variation in the duration of each experiment, we would have to make an
# adjustment for this (this is called an 'offset'.)


################################################################################
################### A logistic (binomial) model ##################
################################################################################

# This type of model is used when the outcome of events is recorded, and each
# event has only two possible outcomes (success/fail, yes/no, alive/dead, etc.)

# Again, we need a non-linear function to provide a link between the model and the data.
# We will use the logit transform- which we will define here in a function:

#This function converts a probability into a log-odds
Logit <- function(prob){
  log(prob/(1 - prob))
}
# Here we see a probability of 0.5 equates to a log-odds of 0
Logit(prob = 0.5)

#It can also be useful to have the inverse function
#This function converts a log-odds into a probability
InvLogit <- function(X){
  exp(X)/(exp(X) + 1)
}
InvLogit(0)

# The simulated data we will use here is motivated by Experimental Hut Trials,
# which are used to evaluate indoor vector control products, such as bednets (ITNs).

#Each morning, mosquitoes are collected, and their status (alive/dead) is recorded
# In this example, three different Nets are used (two ITNs and an untreated net),
# and three huts are used.

EHT_data <- readRDS('EHT_data.rds')
head(EHT_data)

#Let's write the regression model, using a logit link.
# On the left hand side of the model, we group the mosquitoes as:
# cbind('Number of mosquitoes dead','Number of mosquitoes alive').
# We are interested in how the mosquito mortality varies according to the Net used.
# We include Hut in the model, in case mortality varies across the huts.
# We want Hut to be a factor variable here, not a numeric variable:
EHT_data$Hut <- as.factor(EHT_data$Hut)

glm_bin1 <-
  glm(
    cbind(dead, N_mosq - dead) ~
      Net + Hut,
    family = binomial(link = "logit"), data = EHT_data)
summary(glm_bin1)

# here are the model coefficients
glm_bin1$coefficients

#Let's estimate the mortality according to each Net. First, we will do it
# manually, using the coefficients & the inverse link function (InvLogit)

#Mortality in the Control arm (untreated net)
InvLogit(glm_bin1$coefficients[1][[1]])



#Mortality in the INT1 arm
InvLogit(glm_bin1$coefficients[1][[1]] + glm_bin1$coefficients[2][[1]])



#Mortality in the ITN2 arm
InvLogit(glm_bin1$coefficients[1][[1]] + glm_bin1$coefficients[3][[1]])

# The other method to obtain these estimates is to use the predict function.
#Let's predict the mortality for each net in Hut 1
store_pred2 <- data.frame('Net' = c('Control','ITN1','ITN2'))
store_pred2$mortality <- predict(glm_bin1, newdata = data.frame('Net' = c('Control','ITN1','ITN2'),
                                                                'Hut' = as.factor(rep(1,3))), type = 'response')
store_pred2

#Now, let's add the uncertainty. We follow the same progress as we did for the
# Poisson model: we calculate the confidence interval on the model scale, and then
# we make the transformation, using the inverse link function

store_pred2 <- data.frame('Net' = c('Control','ITN1','ITN2'))

#Again, we make the prediction for Hut 1
predict(glm_bin1, newdata = data.frame('Net' = c('Control','ITN1','ITN2'),
                                       'Hut' = as.factor(rep(1,3))), type = 'link', se.fit = T)
#Let's store these values somewhere:
predict_with_se2 <- predict(glm_bin1, newdata = data.frame('Net' = c('Control','ITN1','ITN2'),
                                                           'Hut' = as.factor(rep(1,3))), type = 'link', se.fit = T)

store_pred2$mortality_model_scale <- predict_with_se2$fit

#Now, using the normality assumption, we calculate the CI (again, on the model scale):
store_pred2$mortality_model_scale_ci1 <- predict_with_se2$fit - 1.96 * predict_with_se2$se.fit
store_pred2$mortality_model_scale_ci2 <- predict_with_se2$fit + 1.96 * predict_with_se2$se.fit

#now, we transform the predictions, using the inverse link function. This allows
# us to make the comparison with the data:
store_pred2$mortality <- InvLogit(store_pred2$mortality_model_scale)
store_pred2$mortality_ci1 <- InvLogit(store_pred2$mortality_model_scale_ci1)
store_pred2$mortality_ci2 <- InvLogit(store_pred2$mortality_model_scale_ci2)

#Let's look at the predictions
store_pred2

#And plot them
ggplot(store_pred2) + geom_point(aes(x = Net, y = mortality)) +
  geom_errorbar(aes(x = Net, ymin = mortality_ci1, ymax = mortality_ci2)) +
  theme_bw() + ylab('Mortality')

#Let's add some data to the plot
ggplot(store_pred2) + geom_point(aes(x = Net, y = mortality)) +
  geom_errorbar(aes(x = Net, ymin = mortality_ci1, ymax = mortality_ci2)) +
  theme_bw() + ylab('Mortality') +
  geom_jitter(data = EHT_data, aes(x = Net, y = dead/N_mosq, color = Net),
              alpha = .5, width = 0.2, height = 0)

#Note: in each data point, we have a variable number of mosquitoes 'N_mosq'.
# We could adjust the size of the data points on the graph, to reflect this:
ggplot(store_pred2) + geom_point(aes(x = Net, y = mortality)) +
  geom_errorbar(aes(x = Net, ymin = mortality_ci1, ymax = mortality_ci2)) +
  theme_bw() + ylab('Mortality') +
  geom_jitter(data = EHT_data, aes(x = Net, y = dead/N_mosq, color = Net, size = N_mosq),
              alpha = .5, width = 0.15, height = 0) + labs(size = 'Number of \nmosquitoes')

#Remember, we have generated predictions here that use the coefficient for Hut 1.
# So we could generate predictions for Hut 2 and Hut 3 here, too