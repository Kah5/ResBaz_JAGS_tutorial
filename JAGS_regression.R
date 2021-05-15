# A Bayesian linear regression model
library(rjags)
library(coda)
library(ggplot2)

# JAGS expects data in a list
car.data <- list(xvals = cars$dist, y = cars$speed, n = length(cars$speed))

# Model code (JAGS syntax)
asimple_linear_regression <- "model{

  # Likelihood
  for(i in 1:n){
  
    # data model 
    y[i]   ~ dnorm(mu[i], tau) # data y follows a normal distribution with mean mu and precision tau
    
    # process model for the linear regression
    mu[i] <- alpha + beta*xvals[i]  
  }

 # Priors
  beta ~ dnorm(0,0.0001) # uninformative priors for beta and alpha
  alpha ~ dnorm(0,0.0001)

  
  tau   ~ dgamma(0.01, 0.01) # Prior for the inverse variance, the precision tau
  sigma     <- 1/sqrt(tau) 
  
  # note that in JAGS the second argument of the normal 
  # distribution is 1/sigma^2, or the precision Tau

}"

# Initialize JAGS model
jags.model   <- jags.model (file = textConnection(asimple_linear_regression),
                            data = car.data,
                            #inits = inits, # you can also specific initial conditions
                            n.chains = 3)

# Sample and save the posterior
jags.reg.out   <- coda.samples (model = jags.model,
                                variable.names = c("alpha","beta", "tau"),
                                n.iter = 5000)

# Summary statistics of the output
summary(jags.reg.out)

# Check class of output
class(jags.reg.out)

# Model diagnostics: check for convergence visually and with an algorithm
par(mfrow = c(1,3))
traceplot(jags.reg.out)
gelman.diag(jags.reg.out)

# Check autocorrelation to determine thinning interval
autocorr(jags.reg.out)
par(mar = c(1, 1, 1, 1))
autocorr.plot(jags.reg.out)


# Convert mcmc.list object to matrix, plot parameters and uncertainty
jags.mat <- as.matrix(jags.reg.out) 
xpred <- min(cars$dist): max(cars$dist)
cred.lines <- list()
for(i in 1000:2000){
  cred.lines[[i]]<-  data.frame(MCMC.step = i,
                                xpreds = xpred, 
                                ypreds = jags.mat[i,"alpha"] + jags.mat[i,"beta"]*xpred)
}
cred.intervals <- do.call(rbind, cred.lines)


ggplot(cars, aes(dist, speed)) + 
  geom_point() +
  geom_line(data = cred.intervals, aes(x =xpreds, y = ypreds, group = MCMC.step),
            color = "grey", alpha = 0.5) +
  theme_bw(base_size = 12) +
  ylab("Distance") +
  xlab("Speed") 

# Simulate the credible intervals and predictive intervals 
# from random pairs of samples of the mcmc output
npred <-length(min(cars$dist): max(cars$dist))
nsamp <- 1000
mcmcsamples <- sample(nrow(jags.mat), nsamp)
xpred <- min(cars$dist): max(cars$dist) 	
ypred <- matrix(NA ,nrow=nsamp, ncol=npred)	
ycred <- matrix(NA, nrow=nsamp, ncol=npred)	

# Use for loop to calculate the expected value of y at each x 
# for each pair of regression parameters (credible interval)
# and then add additional random error from the data model (prediction interval)

for(i in 1:1000){
  params <-  jags.mat[mcmcsamples[i],]
  ycred[i,] <- params["alpha"] + params["beta"]*xpred # same as above
  ypred[i,] <- rnorm(n = npred, mean = ycred[i,], sd = 1/sqrt(params["tau"])) #draw from normal distribution with our estimated uncertainty!
}


# Combine the  ci, pi, and xpreds into a dataframe
ci.and.pi <- data.frame(xvals = xpred, 
                        ci.med = apply(ycred, 2, quantile, 0.5),
                        ci.lo= apply(ycred, 2, quantile, 0.025),
                        ci.hi= apply(ycred, 2, quantile, 0.975),
                        pi.med= apply(ypred, 2, quantile, 0.5),
                        pi.lo= apply(ypred, 2, quantile, 0.025),
                        pi.hi=apply(ypred, 2, quantile, 0.975))  

# Plot with all the 95% credible intervals and 95% posterior predictive intervals on it!

ggplot() +
  geom_point(data = cars, aes(dist, speed))+theme_bw(base_size = 12) + 
  ylab("Speed") +
  xlab("Distance") +
  geom_ribbon(data = ci.and.pi, aes(x = xvals, ymin = pi.lo, ymax = pi.hi), fill = "blue", alpha = 0.6) +
  geom_ribbon(data = ci.and.pi, aes(x = xvals, ymin = ci.lo, ymax = ci.hi), fill = "grey", alpha = 0.6)
