# Requires source from 3.2.1_Frequentist_Anova.R

library(R2jags)

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
  
  for(k in 1:n){
    y[k] ~ dnorm(mu[k], tau) 
    mu[k] <- m + alpha[group[k]] 
  }
  
  m ~ dnorm(0.0, 0.0001)
  alpha[1] <- 0 
  
  for(i in 2:I){ 
    alpha[i] ~ dnorm(0.0,0.0001)
  }
  
  tau ~ dgamma(0.001,0.001)
  sigma <- 1.0 / sqrt(tau)
}
# Run JAGS
Bayesian_anova_inference <- jags(data = data_anova, 
                                 parameters.to.save = c("m", # m is mu above
                                                        "alpha", 
                                                        "sigma", 
                                                        "tau"), 
                                 n.iter = 100000, 
                                 n.chains = 3,
                                 model.file = Bayesian_anova)

