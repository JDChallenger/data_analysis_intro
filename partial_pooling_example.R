library(lme4)

#The data we will use is the 'reedfrogs' data
# from the rethinking package
# (https://github.com/rmcelreath/rethinking)
d <- readRDS('data/d_partial_pooling.rds')

View(d)

# Let's use the data to look at the effect that the presence of a predator 
# has on tadpole survival

#No tank effect here- this is 'complete pooling',
# i.e. we assume each tank is identical
m0 <- glm(
        cbind(surv, density - surv) ~ pred,
        data = d, family = binomial)
summary(m0)

#No pooling, estimate each tank separately
m1 <- glm(
  cbind(surv, density - surv) ~ pred + factor(tank),
  data = d, family = binomial)
summary(m1)

#Let's introduce a random effect for tank (partial pooling)
m2 <- glmer(
  cbind(surv, density - surv) ~ (1|tank) + pred,
  data = d, family = binomial)
summary(m2)

# Note: for negative binomial models, the function is called: glmer.nb()