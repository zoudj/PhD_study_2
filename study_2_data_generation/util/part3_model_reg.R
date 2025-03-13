
# regular IRT model for minor class (m = 1.0)
model_reg_m1 <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  t_mu ~ dnorm(1.0, 25)
  # simulee's ability
  for (s in 1:NS) {
    theta[s] ~ dnorm(t_mu, 1.0)
  }
  theta_mean <- mean(theta[1:NS])
  # item parameters
  # a: discrimination; b: difficulty
  for(i in 1:NI){
    # discrimination parameter a
    # apply item centering constraint
    # b parameter true value: mu == -1, sd == 1
    b[i] ~ dnorm(-1.0, 1)
    # log-normal distribution mean-log = -0.5, sd-log = 0.6 (2.78 = 1/(0.6^2))
    a[i] ~ dlnorm(-0.5, 2.78)
  }
  # Likelihood
  for (s in 1:NS){
    for (i in 1:NI){
      # Rasch model, a == 1
      logit(p[s,i]) <- a[i] * (theta[s] - b[i])
      r[s,i] ~ dbern(p[s,i])
    }
  }
}


# regular IRT model for minor class (m = 2.5)
model_reg_m25 <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  t_mu ~ dnorm(2.5, 25)
  # simulee's ability
  for (s in 1:NS) {
    theta[s] ~ dnorm(t_mu, 1.0)
  }
  theta_mean <- mean(theta[1:NS])
  # item parameters
  # a: discrimination; b: difficulty
  for(i in 1:NI){
    # discrimination parameter a
    # apply item centering constraint
    # b parameter true value: mu == -2, sd == 1
    b[i] ~ dnorm(-2.0, 1)
    # log-normal distribution mean-log = -0.5, sd-log = 0.6 (2.78 = 1/(0.6^2))
    a[i] ~ dlnorm(-0.5, 2.78)
  }
  # Likelihood
  for (s in 1:NS){
    for (i in 1:NI){
      # Rasch model, a == 1
      logit(p[s,i]) <- a[i] * (theta[s] - b[i])
      r[s,i] ~ dbern(p[s,i])
    }
  }
}