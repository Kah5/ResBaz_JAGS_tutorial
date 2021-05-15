# Example: adding driver uncertainty to simple linear regression
library(rjags)
library(coda)
library(ggplot2)

# JAGS expects data input as a list
car.data <- list(xvals = cars$dist, y = cars$speed, n = length(cars$speed))

# Assuming that we know that speed estimates have SD = 2.5
# JAGS model formulation, with informative prior for precision in data model
a_linear_regression_driver_unc <- "model{

  # Likelihood
  for(i in 1:n){
    
    # data model for speed measurements
   true_speed[i] ~ dnorm(xvals[i], tau_speed) 
   
    # data model for distance (y variable)
    y[i]   ~ dnorm(mu[i],inv.var) 
    
    # process model for the linear regression
    mu[i] <- alpha + beta*true_speed[i]  
  }

 # Priors
  beta ~ dnorm(0,0.0001)
  alpha ~ dnorm(0,0.0001)

  
  inv.var   ~ dgamma(0.01, 0.01) # Prior for the inverse variance
  sigma     <- 1/sqrt(inv.var) # note that in JAGS the second argument of the normal distribution is 1/sigma^2, or the precision Tau
  
  # prior for speed precsion
  tau_speed   ~ dgamma(10, 0.5) 

}"

jags.model.drive   <- jags.model (file = textConnection(a_linear_regression_driver_unc ),
                                  data = car.data,
                                  #inits = inits,
                                  n.chains = 3)
jags.drive.unc.out   <- coda.samples (model = jags.model.drive,
                                      variable.names = c("alpha","beta", "inv.var", "tau_speed" ),
                                      n.iter = 5000)

summary(jags.drive.unc.out)
traceplot(jags.drive.unc.out)
