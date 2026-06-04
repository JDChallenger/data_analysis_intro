library(ggplot2)

### Probability distributions

# Let's start with a simple example: tossing a coin.
# We will use this example to illustrate some of the 
# functions that we can use in R.

# When tossing a coin, there are only two possible 
# outcomes: heads or tails (equally, we could call this
# 'success' or 'failure'). We shall use the binomial
# distribution to model this. The binomial distribution
# describes a process that has two possible outcomes,
# the probability of success is p; the probability of failure
# is 1-p. If a coin is 'fair' there is an equal probability
# of each outcome (p=0.5).

#If we toss one coin (size=1), one time, either we record 'Heads' (x=1)
# or 'Tails' (x=0). If the coin is fair (p=0.5), the 
# probability of each outcome is equal
dbinom(x=1, size = 1, p = 0.5) #The probability of obtaining 'Heads'
dbinom(x=0, size = 1, p = 0.5) #The probability of obtaining 'Tails'

# We use the function dbinom() to obtain the probability 
# of observing a particular outcome.

#If we wish to simulate the process, we can use the function
# rbinom().

#For example, what if we take a fair coin (p=0.5), and we toss
# it 10 times? We can simulate this, using rbinom:
rbinom(n=10, size = 1, prob = 0.5)
#Note: this process is random ('stochastic')
#This means, if we do the same thing again, 
# we can obtain different results:
rbinom(n=10, size = 1, prob = 0.5)

#But, we can use dbinom to learn what we should expect 
# 'on average'.
# For example, If we toss a 
# coin 10 times what is the probability that we obtain 
# 4 heads (4 successes) and 6 tails (6 failures)?
dbinom(x = 4,size = 10,prob = 0.5)


# The binomial distribution is an example of a discrete
# probability distribution- only certain outcomes are possible

# There are also continuous probability distributions,
# where the outcome can be any real number 
# (for example 15.1, 15.01, 15.08, 15.0001).
# Let's think of an example:

# Suppose that we go to a hospital and we measure the weight
# of 1000 new-born babies. We find that the average weight
# of a newborn baby is 3kg, with a standard deviation of 0.6kg
# We notice that the distribution of weights is symmetric
# around the mean. Therefore, we decide to describe the 
# distribution of weights using a normal distribution

# We can use the function rnorm() to simulate the weights of 
# 500 babies, using this distribution
rnorm(n = 500, mean = 3, sd = 0.6)
#Let's draw a histogram of these values
hist(rnorm(n = 500, mean = 3, sd = 0.6))

# We can use the function dnorm() to learn about the 
# probability distribution.

curve(dnorm(x, mean = 3, sd = 0.6), from=-4, to=4)

# We can make a more attractive graph using the 
# ggplot2 package:

ggplot() + xlim(c(0,7)) + theme_classic() + 
  geom_function(fun = dnorm, args = list(mean = 3, sd = 0.6))

# From this distribution, what proportion of babies 
# weigh less than 4kg? To answer this question, we can 
# use the cumulative probability distribution 'pnorm()'

k <- pnorm(q = 4, mean = 3, sd = 0.6)
k

#Let's plot this value on a graph
ggplot() + xlim(c(0,7)) + theme_classic() + 
  geom_function(fun = dnorm, args = list(mean = 3, sd = 0.6)) + 
  geom_vline(xintercept = 4, color = 'red') +
  ylab('Probability density') + 
  xlab('Weight (kg)')

#What proportion of babies weigh less than 3kg? 
pnorm(q = 3, mean = 3, sd = 0.6)
#This makes sense- the distribution is symmetrical 
# about 3kg. 

#
ggplot() + xlim(c(0,7)) + theme_classic() + 
  geom_function(fun = dnorm, args = list(mean = 3, sd = 0.6)) + 
  geom_vline(xintercept = 3, color = 'red') +
  ylab('Probability density') + 
  xlab('Weight (kg)') + 
  annotate('text' , x = 1, y = 0.4, 
           label = '50% of \n babies < 3kg',
           color = 'red') + 
  annotate('segment', x = 1, xend = 2.8,
            y = 0.3, yend = 0.2, colour = 'red',
           arrow = arrow(length = unit(2, "mm"))
           ) +
  annotate('text' , x = 5, y = 0.4, 
           label = '50% of \n babies > 3kg',
           color = 'red') +
  annotate('segment', x = 5, xend = 3.8,
             y = 0.3, yend = 0.2, colour = 'red',
           arrow = arrow(length = unit(2, "mm")))

# What if we want to know the proportion of babies that 
# weight between 2.5kg and 4kg?

# Well, we know the proportion of babies that weight
# less than 4kg:
pnorm(4, mean = 3, sd = 0.6)
ggplot() + xlim(c(0,7)) + theme_classic() + 
  geom_function(fun = dnorm, args = list(mean = 3, sd = 0.6)) + 
  geom_vline(xintercept = 4, color = 'red') +
  ylab('Probability density') + 
  xlab('Weight (kg)') + 
  annotate('text', x = 0.7, y = 0.4, 
           label = '<4kg', color = 'red') + 
  annotate('segment', x = 0.7, xend = 2.5,
           y = 0.35, yend = 0.25, color = 'red',
           arrow = arrow(length = unit(2, "mm")))
  

# And we know the proportion of babies that weight less
# than 2.5kg:
pnorm(2.5, mean = 3, sd = 0.6)
ggplot() + xlim(c(0,7)) + theme_classic() + 
  geom_function(fun = dnorm, args = list(mean = 3, sd = 0.6)) + 
  geom_vline(xintercept = 2.5, color = 'red') +
  ylab('Probability density') + 
  xlab('Weight (kg)') + 
  annotate('text', x = 0.45, y = 0.34, 
           label = '<2.5kg', color = 'red') + 
  annotate('segment', x = 0.37, xend = 2.35,
           y = 0.25, yend = 0.2, color = 'red',
           arrow = arrow(length = unit(2, "mm")))

# Therefore, the proportion of babies that weigh
# between 2.5kg and 4kg:
pnorm(4, mean = 3, sd = 0.6) - pnorm(2.5, mean = 3, sd = 0.6) 


# There are many different types of probability distributions
# Here we will list some common examples. 

# First we look at distributions for discrete variables.
# We use this kind of distribution for things like mosquito counts,
# that take values 0,1,2,3 (i.e. positive integers).

# A popular distribution for counts is the Poisson distribution
# We can plot this distribution using the function dpois().
# This distribution only requires one parameter, lambda, which 
# is the average number of events we expect to observe in a
# given interval of time. For example, how many mosquitoes
# do we collect in one hour?

# If the average number is 5 (lambda = 5), what is the probability
# that, for a particular observation, we observe 3 mosquitoes?

dpois(x = 3, lambda = 5)

#Let's plot the whole distribution
data_pois <- data.frame('n_mosquitoes' = seq(0,30), 
          'prob' = dpois(x = seq(0,30), lambda = 5))
ggplot(data_pois) + theme_classic() + 
  geom_point(aes(x = n_mosquitoes, y = prob)) + 
  ylab('Probability') + 
  ggtitle('Poisson distribution') + 
  xlab('Number of mosquitoes collected in one hour')

#Note: for a discrete distribution, the probabilities
# should add up to 1. For example: 
sum(dpois(seq(0,30), lambda = 5))


# And we can also generate simulations from the distribution
# We do this using rpois():
# What if we performed 100 mosquito collections?
rpois(100,lambda = 5)
# we can plot this in a histogram
hist(rpois(100,lambda = 5))

# Now, we look at the binomial distribution. We use
# this for experiments that have only two outcomes. 
# For example, if we test an insecticide on a mosquito population
# we can record the status of each mosquito as dead (x=1),
# or alive (x=0). Parameter p is the proportion of dead mosquitoes
# that we observe on average.

# Let's say that we have 10 mosquitoes (size = 10) and 
# on average we expect 40% mortality (p=0.4).
# What is the probability that we observe 3 dead mosquitoes,
# and 7 alive mosquitoes?

dbinom(3, size = 10, prob = 0.4)

#Let's plot the whole distribution.
#Remember, the number of dead mosquitoes must be between 0 and 10,
# because we only have 10 mosquitoes in total!

data_bin <- data.frame('n_dead' = seq(0,10), 
          'prob' = dbinom(x = seq(0,10), size = 10, prob = 0.4))
ggplot(data_bin) + theme_classic() + 
  geom_point(aes(x = n_dead, y = prob), color = 'darkgreen') + 
  ggtitle('Binomial distribution') + 
  ylab('Probability') + 
  xlab('Number of dead mosquitoes observed')

#Again, if we sum up all the probabilities, we should 
# obtain 1.
dbinom(x = seq(0,10), size = 10, prob = 0.4)
#sum up all the values
sum(dbinom(x = seq(0,10), size = 10, prob = 0.4))

# Now let's look at some continuous distributions.
# We have already seen the normal distribution.
# This has two parameters, mu (the average, or 'mean'), and
# sd (the standard deviation)


#Let's plot some examples
#With the normal distribution, the values can be 
#positive or negative
ggplot() + xlim(c(-10,10)) + theme_classic() + 
  geom_function(fun = dnorm, args = list(mean = 0, sd = 1)) +
  geom_function(fun = dnorm, args = list(mean = 2, sd = 2),
                color = 'purple') + 
  geom_function(fun = dnorm, args = list(mean = -1, sd = 0.7),
                color = 'slateblue') + 
  ggtitle('The normal distribution') + 
  ylab('Probability density')

#Note: for a continuous probability distribution, the area under
# the curve must equal one. So it is different compared to a 
# discrete distribution

# Let's look at a different continuous distribution

#The log-normal distribution
# This distribution only takes positive values.
# It is useful for data that vary over several orders of magnitude
# The distribution is not symmetric- it has a 'tail'

#Let's plot some examples
#To plot the probability distribution, we use the function
# dlnorm(). It has two parameters, meanlog & sdlog
ggplot() + xlim(c(0,20)) + theme_classic() + 
  geom_function(fun = dlnorm, args = list(meanlog = 0, sdlog = 1)) +
  geom_function(fun = dlnorm, args = list(meanlog = 2.5, sdlog = 1),
                color = 'purple') + 
  geom_function(fun = dlnorm, args = list(meanlog = -1, sd = 2.5),
                color = 'slateblue') + 
  ggtitle('The log_normal distribution') + 
  ylab('Probability density')

# There are other continuous distributions
# The gamma distribution
# This distribution is useful for 'waiting times', or 'survival times'
?dgamma

#The Weibull distribution
# Similar to gamma- useful for 'time to failure', or survival analysis
?dweibull


# There are other discrete distributions

# The negative binomial distributions. 
# Very useful for count data with 'dispersion' (high variation)
# Here the mean parameter is mu, and the dispersion paramter is 'size'
?dnbinom

#let's plot a distribution, choosing some parameter values
data_nb <- data.frame('n_mosquitoes' = seq(0,50), 
          'prob1' = dnbinom(x = seq(0,50), mu = 10, size = 4),
          'prob2' = dnbinom(x = seq(0,50), mu = 10, size = 1))
ggplot(data_nb) + theme_classic() + 
  geom_point(aes(x = n_mosquitoes, y = prob1)) + 
  geom_point(aes(x = n_mosquitoes, y = prob2), 
             color = 'slateblue') + 
  ylab('Probability') + 
  ggtitle('Negative-binomial distribution') + 
  xlab('Number of counts observed')

#These two distributions have different shapes, but the average
# values are the same. We can check this by sampling from the 
# distribution, using rnbinom(), and calculating the mean.
# For this, it is a good idea to take a big sample (e.g. 10000):

mean(rnbinom(10000, mu = 10, size = 4))
mean(rnbinom(10000, mu = 10, size = 1))
#The two values should be very similar





