# Example: adding random effects to simple linear regression
library(rjags)
library(coda)
library(ggplot2)

# Generate data for 3 drivers and plot
driver1 <- data.frame(speed = cars$speed + rnorm(50, 10, 2), 
                      dist = cars$dist + rnorm(50, 0, 0.2), 
                      driver = 1)
driver2 <- data.frame(speed = cars$speed + rnorm(50, -7, 2), 
                      dist = cars$dist + rnorm(50, 0, 0.2), 
                      driver = 2)
driver3 <- data.frame(speed = cars$speed , 
                      dist = cars$dist , 
                      driver = 3)

cars.driver <- rbind(driver1, driver2, driver3)
cars.driver$driver <- as.character(cars.driver$driver)

ggplot(data = cars.driver, aes(dist, speed, color = driver)) +
  geom_point()+stat_smooth(method = "lm") + 
  theme_bw(base_size = 12) +  
  ylab("Speed") +
  xlab("Distance")


# JAGS expects data input as a list
cardriver.data <- list(xvals = cars.driver$dist, 
                       y = cars.driver$speed,  
                       driver = cars.driver$driver, 
                       n = length(cars.driver$speed), 
                       ndriver = length(unique(cars.driver$driver)))

# Specify JAGS model
a_linear_regression_random_intercept <- "model{

  # Likelihood
  for(i in 1:n){
   # data model for speed measurements
   
   true_speed[i] ~ dnorm(xvals[i], tau_speed) 
    y[i]   ~ dnorm(mu[i],inv.var) # data model 
    mu[i] <- alpha[driver[i]] + beta*true_speed[i]  # process model for the linear regression
  }

for (i in 1:ndriver) {
    alpha[i] ~ dnorm(mu_alpha, tau_alpha) # Specify random intercepts for each of the drivers, drawn from a distribution of mean  = mu_alpha and precision tau_alpha
}


 # Priors
  beta ~ dnorm(0,0.0001)
  inv.var   ~ dgamma(0.01, 0.01) # Prior for the inverse variance
  sigma     <- 1/sqrt(inv.var) # note that in JAGS the second argument of the normal distribution is 1/sigma^2, or the precision Tau
  
  
# hyperparameters/priors:

 mu_alpha ~ dnorm(0, 0.0001) # hyperparameter mean random alpha
 
 
 
 #sigma_alpha~dunif(0, 100) # SD hyperparameter for random intercepts
 tau_alpha ~ dgamma(0.01, 0.01) # convert sigma_alpha to a precision (what jags expects for a normal distribution)
# prior for speed precsion
  tau_speed   ~ dgamma(10, 0.5) 

}"

jags.int.model   <- jags.model (file = textConnection(a_linear_regression_random_intercept),
                                data = cardriver.data,
                                #inits = inits,
                                n.chains = 3)

jags.int.out   <- coda.samples (model = jags.int.model,
                                variable.names = c("alpha","beta", "inv.var", "tau_alpha"),
                                n.iter = 5000)

summary(jags.int.out)
traceplot(jags.int.out)

# Obtain credible intervals
jags.int.mat <- as.matrix(jags.int.out) 
jags.int.mat <- jags.int.mat[1000:15000,]#select burn in of 1000
xpred <- min(cars.driver$dist): max(cars.driver$dist)
plot(cars.driver$dist, cars.driver$speed)
cred.lines <- list()

# loop through to get the lines
for(i in 1000:2000){
  cred.lines[[i]]<-  data.frame(MCMC.step = i,
                                xpreds = xpred, 
                                ypreds.driver1 = jags.int.mat[i,"alpha[1]"] + jags.int.mat[i,"beta"]*xpred,
                                ypreds.driver2 = jags.int.mat[i,"alpha[2]"] + jags.int.mat[i,"beta"]*xpred,
                                ypreds.driver3 = jags.int.mat[i,"alpha[3]"] + jags.int.mat[i,"beta"]*xpred)
}

cred.intervals <- do.call(rbind, cred.lines)
cred.intervals.m <- reshape2::melt(cred.intervals, id.vars = c("MCMC.step", "xpreds"))
cred.intervals.m$driver <- ifelse(cred.intervals.m$variable %in% "ypreds.driver1", "1", 
                                  ifelse(cred.intervals.m$variable %in% "ypreds.driver2", "2", "3"))


ggplot(cars.driver, aes(dist, speed, color = driver)) +
  geom_point() +
  theme_bw(base_size = 12) + 
  ylab("Distance") +
  xlab("Speed") + 
  geom_line(data = cred.intervals.m, 
            aes(x =xpreds, y = value,  group = MCMC.step, color = driver), 
            alpha = 0.5) +
  facet_wrap(~variable)
