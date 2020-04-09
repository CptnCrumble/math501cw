# Requires source from 3.2.1_Frequentist_Anova.R

library(R2jags)
library(ggmcmc)

# PART F - WRITE BUGS CODE

# Data prep:
# Yield values
y <- df_long$Yield
# fertilizer groups - convert group to numeric factor
g1 <- df_long$Fertilizer
g2 <- substr(g1,2,2)
group <-as.factor(as.numeric(g2))
# number of data points
n <- length(y)
# number of groups 
I <- 4
data_anova <- list("y", "group", "n", "I")

#BUGS code
Bayesian_anova <- function(){
  
  # Likelihood function
  for(k in 1:n){
    y[k] ~ dnorm(mu[k], tau) 
    mu[k] <- m + alpha[group[k]] 
  }
  
  # Priors
  m ~ dnorm(0.0, 0.0001)
  alpha[1] <- 0 
  
  for(i in 2:I){ 
    alpha[i] ~ dnorm(0.0,0.0001)
  }
  
  tau ~ dgamma(0.001,0.001)
  sigma <- 1.0 / sqrt(tau)
  
  # Track Means
  mean_yield[1] = m + alpha[1]
  mean_yield[2] = m + alpha[2]
  mean_yield[3] = m + alpha[3]
  mean_yield[4] = m + alpha[4]
}
# Run JAGS
Bayesian_anova_inference <- jags(data = data_anova, 
                                 parameters.to.save = c("m", 
                                                        "alpha", 
                                                        "sigma", 
                                                        "tau",
                                                        "mean_yield"), 
                                 n.iter = 100000, 
                                 n.chains = 3,
                                 model.file = Bayesian_anova)

# PART G - Graph of Posterior densities

mcmc_data <- as.mcmc(Bayesian_anova_inference)
ggs_data <- ggs(mcmc_data)
ggs_density(ggs_data, family = "^alpha")+xlim(-10,10)
# + better titles all round
# Need discussion points

# PART H - plots of confidence intervals
# Plot of means
ggs_caterpillar(ggs_data,family = "^mean_yield")
# Plot of alphas
ggs_caterpillar(ggs_data,family = "^alpha")


# PART I - chat
print(Bayesian_anova_inference, intervals = c(0.025,0.5,0.975))
# could use some qbinom??

# PART J - Bayesian multi hypothesis ANOVA

delta_alpha_model <- function(){
  
  # Likelihood function
  for(k in 1:n){
    y[k] ~ dnorm(mu[k], tau) 
    mu[k] <- m + alpha[group[k]] 
  }
  
  # Priors
  m ~ dnorm(0.0, 0.0001)
  
  for(i in 1:I){ 
    alpha[i] ~ dnorm(0.0,0.0001)
    for (n in 1:(i-1)) {
      AlphaDelta[n,i] <- alpha[n]-alpha[i]
    }
  }
  
  # Variance
  tau ~ dgamma(0.001,0.001)
  sigma <- 1.0 / sqrt(tau)
  
  # Calculate for units...
  Alpha_four_test <- alpha[4] - ((alpha[1] + alpha[2] + alpha[3])/3)
}

delta_alpha_inference <- jags(data = data_anova, 
                              parameters.to.save = c("AlphaDelta","Alpha_four_test"), 
                              n.iter = 1000000, 
                              n.chains = 3,
                              model.file = delta_alpha_model)

print(delta_alpha_inference, intervals = c(0.025,0.5,0.975))

# Part K - the simpler model
simple_bayes_model <- function(){
  
  # Likelihood function
  for(k in 1:n){
    y[k] ~ dnorm(mu[k], tau)
    mu[k] <- fertilizer[group[k]]
  }
  
  # Priors
  for(i in 1:I){ 
    fertilizer[i] ~ dnorm(0.0,0.0001)
  }
  
  tau ~ dgamma(0.001,0.001)
  sigma <- 1.0 / sqrt(tau)
}

# Simple JAGS
simple_jags <- jags(data = data_anova, 
                    parameters.to.save = c("fertilizer", 
                                           "sigma", 
                                           "tau"), 
                    n.iter = 100000, 
                    n.chains = 3,
                    model.file = simple_bayes_model)

print(simple_jags, intervals = c(0.025,0.5,0.975))
ggs_simple <- ggs(as.mcmc(simple_jags))
ggs_caterpillar(ggs_simple,family = "^fertilizer")
# Not convinced...
