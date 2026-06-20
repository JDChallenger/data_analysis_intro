library(ggplot2)
library(odin)

odin::can_compile()

SIR_model <- odin::odin({
  
  ## List the differential equations here
  deriv(S) <- -beta*I*S/N
  deriv(I) <- beta*I*S/N - sigma*I
  deriv(R) <- sigma*I
  
  ## Initial conditions for the ODE system
  initial(S) <- N0 - I0
  initial(I) <- I0 
  initial(R) <- 0
  
  N <- S + I + R
  
  ## Parameters (all rates in units of 1/day )
  ## Note: Use 'user()' to be able to easily change parameter values, without re-compiling the model
  beta <- user(0.8) #The rate at which
  sigma <- user(0.05) #The rate at which
  I0 <- user(1)
  N0 <- user(1000)

})

#Make a parameter list. You can include any parameters marked with 'user()'
params1 <- list(I0 = 2, beta = 0.7, sigma = 0.1)

#Define the model & parameters you want to use
mod1 <- SIR_model$new(user = params1)
#Timepoints you want to store. 
#We store model output from the first 50 days of the epidemic
t1 <- seq(0, 50, length.out=51)
#Run model, and generate output
y1 <- mod1$run(t1)
#look at the output
head(y1)
dfy <- data.frame('t'=y1[,1],'S'=y1[,2],'I'=y1[,3], 'R'=y1[,4])

ggplot(dfy) + geom_line(aes(x=t, y = S, color = 'a')) + xlab('Time (Days)') + 
  geom_line(aes(x=t, y = I, color = 'b')) + ylab('Population') + 
  geom_line(aes(x=t, y = R, color = 'c')) +
  scale_color_manual(values = c('black','orange','purple'),
                     labels = c('S', 'I', 'R'), name = 'Compartment') + 
  #ylab('log10 (Viral copies / ml)') +
  theme_classic()


#Make a new parameter list. In this example R0<1 (see PowerPoint presentation)

params2 <- list(I0 = 1, beta = 0.3, sigma = 0.4)

#Define the model & parameters you want to use
mod2 <- SIR_model$new(user = params2)
#Timepoints you want to store. 
#We store model output from the first 50 days of the epidemic
t1 <- seq(0, 50, length.out=51)
#Run model, and generate output
y2 <- mod2$run(t1)
#look at the output
head(y2)
dfy <- data.frame('t'=y2[,1],'S'=y2[,2],'I'=y2[,3], 'R'=y2[,4])

ggplot(dfy) + #geom_line(aes(x=t, y = S, color = 'a')) + 
  xlab('Time (Days)') + 
  geom_line(aes(x=t, y = I), color = 'orange') + 
  ylab('I(t)') + 
  #geom_line(aes(x=t, y = R, color = 'c')) +
  #scale_color_manual(values = c('black','orange','purple'),
   #                  labels = c('S', 'I', 'R'), name = 'Compartment') + 
  #ylab('log10 (Viral copies / ml)') +
  theme_classic()
