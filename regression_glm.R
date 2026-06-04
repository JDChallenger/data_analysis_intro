#library(lme4)
#library(MASS)
library(ggplot2)

################################################################################
###############  2 . Generalised linear regression models (GLMs)  ##############
################################################################################

# For other types of data (e.g. count data, data for proportions), we cannot use a 
# linear model directly. Instead, we define a linear model on an appropriate 'model scale'. 
# Then we make a non-linear transformation to the linear model. This is why these 
# models are called 'Generalised Linear Models' (GLMs). 

################################################################################
###################              Poisson model                ##################
################################################################################

# As an example, let's fit a Poisson model to some synthetic data. We imagine 
# an experiment to test the repellancy of mosquito nets. For three types of net 
# (A control net with no insecticide, ITN1, and ITN2), we record how often mosquitoes
# touch the net in a 5-minute period. For each net we perform 25 replicates
# (i.e. we repeat the experiment using 25 mosquitoes).

count_data <- readRDS('data/ITN_count_data.rds')
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

#Note: this model is quite simple. In particular, each experiment was the same duration (5 minutes).
# If there was variation in the duration of each experiment, we would have to make an
# adjustment for this (this is called an 'offset'.)


################################################################################
###################       A logistic (binomial) model         ##################
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

EHT_data <- readRDS('data/EHT_data.rds')
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
# To obtain estimates on the mortality scale, we use the argument type = 'response'
store_pred2 <- data.frame('Net' = c('Control','ITN1','ITN2'))
store_pred2$mortality <- predict(glm_bin1, newdata = data.frame('Net' = c('Control','ITN1','ITN2'),
                                                                'Hut' = as.factor(rep(1,3))), type = 'response')
store_pred2



