library(ggplot2)

# synthetic (simulated) data.
# Let's imagine we toss a coin 60 times. We don't know if the coin is fair
# (unbiased) or not

#'Heads' = 1, 'Tails' = 0
d <- rbinom(n = 60, size = 1, prob = 0.4)
table(d)

#Definition of likelihood: What's the probability of the data, for a given model? 
# Here, our model is very simple: we wish to infer the probability (p) of tossing 
# the coin and obtaining 'Heads'. 

# Let's calculate the likelihood for 200 values of p. Remember p is a probability,
# so it's value must lie between 0 and 1
p_list <- seq(0.0025,1,0.005)

#To calculate the likelihood, we need the probability distrbution for the Binomial 
# Distribution (technically, for a discrete varaible, this is the 'probability mass function', or PMF)

#What is the likelihood of the data for a model with p=0.1?
number_of_heads <- sum(d)
number_of_trials <- length(d)
dbinom(x = number_of_heads, size = number_of_trials, prob = 0.1)

#These numbers are often very small! Often, it is easier to work with the log of the 
# likelihood (the "log-likelihood"), like this: 

dbinom(x = number_of_heads, size = number_of_trials, prob = 0.1, log = T)

# We want to find the model with the maximum value of the likelihood
# This is the same as the model with the maximum log-likelihood

#Let's vary p, calculating the likelihood each time
# (for more complicated models, it is easier to use the log-likelihood,
# but we won't do this for this simple model)

LL <- rep(0,length(p_list)) # a vector to store the likelihood values
for(j in 1:length(p_list)){
  LL[j] <- dbinom(x = number_of_heads, size = number_of_trials, prob = p_list[j], log = F)
}

#Now we can plot the relationship between p and the log-likelihood
plot1 <- ggplot(data = data.frame('p' = p_list, 'LL' = LL)) + 
  geom_point(aes(x = p, y = LL)) + theme_classic() + ylab('Likelihood') +
  geom_line(aes(x = p, y = LL)) + ggtitle('60 data points')
plot1

#for which value of p do we find the maximum likelihood? 
which.max(LL) # this tells us which element in the vector LL has the max value
p_list[which.max(LL)] # This is the Maximum Likelihood estimate for p

#Let's repeat the analysis. This time we have more data- we toss the same coin 1000 times

d2 <- rbinom(n = 1000, size = 1, prob = 0.4)
table(d2)

number_of_heads2 <- sum(d2)
number_of_trials2 <- length(d2)

LL2 <- rep(0,length(p_list)) # a vector to store the likelihood values
for(j in 1:length(p_list)){
  LL2[j] <- dbinom(x = number_of_heads2, size = number_of_trials2, prob = p_list[j], log = F)
}

#Now we can plot the relationship between p and the log-likelihood for both 
# cases (60 coin tosses, and 1000 coin tosses)
plot2 <- ggplot() + 
  geom_point(data = data.frame('p' = p_list, 'LL' = LL2), aes(x = p, y = LL)) + theme_classic() + 
  geom_line(data = data.frame('p' = p_list, 'LL' = LL2), aes(x = p, y = LL)) + 
  ggtitle('1000 data points') + ylab('Likelihood')
plot2

# Let's look at the two plots together.
# Also, let's add the true value for p (remember, we simulated the data!)
library(cowplot)
cowplot::plot_grid(plot1 + geom_vline(xintercept = 0.4, color = 'red', alpha = .5), 
                   plot2 + geom_vline(xintercept = 0.4, color = 'red', alpha = .5), 
                   nrow = 2)

# The likelihood plot for parameter p becomes narrower when we have more data-
# because we are more confident in our estimate!


################################################################################
########################      Confidence intervals        ######################
################################################################################

#There are various ways to estimate the confidence intervals,
# A versatile option is via a 'bootstrap', where we resample our data,
# it is a bit like making lots of different datasets from only one dataset.

#The process involves:
# (i) Take the dataset and resample (with replacement), to randomly create a 
# 'new' dataset.
# (ii) Calculate the maximum likelihood value for p for this dataset
# Then we resample the data, and repeat the process!

#This example motivated by this webpost: 
#https://www.r-bloggers.com/2022/12/how-to-perform-bootstrapping-in-r/

#This is the library we'll need
library(boot)

# Now, we need to write a function that returns the statistic (in our case,
# the value of p that maximises the likelihood).
#The function arguments need to include 'data' and 'indices'. This is because
# the boot() function will use these arguments to resample the data
ML_function <- function(data, indices){
  
  #sample from the data
  dat2 <- data[indices]
  
  p_list <- seq(0.0025,1,0.005)
  
  number_of_heads2 <- sum(dat2)
  number_of_trials2 <- length(dat2)
  
  LL2 <- rep(0,length(p_list)) # a vector to store the likelihood values
  for(j in 1:length(p_list)){
    LL2[j] <- dbinom(x = number_of_heads2, size = number_of_trials2, prob = p_list[j], log = F)
  }
  
  #Return the Maximum Likelihood estimate for p
  return(p_list[which.max(LL2)]) # This is the Maximum Likelihood estimate for p
  
}

#Resample the data 1000 times, each time calculating the maximum likelihood
bootobj <- boot(data = d, statistic = ML_function, R = 1000)
bootobj

#Calculate the 95% confidence intervals for the ML
boot.ci(bootobj, conf = 0.95, type = "basic")





