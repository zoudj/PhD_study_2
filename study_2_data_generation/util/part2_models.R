# created by DZ, Mar. 25, 2022
# this file stores mixture models I use in the study



#########################################################


#########################################################
### 1-class 2PL model

# FOR openBUGS
# regular 2PL IRT specification
model_1C <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  t_mu ~ dnorm(0.0, 1.0)
  # simulee's ability
  for (s in 1:NS) {
    theta[s] ~ dnorm(t_mu, 1.0)
  }
  # item parameters
  # a: discrimination; b: difficulty
  # in this case a in c1 = a in c2, b in c1 = b in c2 correspondingly
  for(i in 1:NI){
    # discrimination parameter a
    # apply item centering constraint
    # b parameter true value: mu == 0, sd == 2
    bb[i] ~ dnorm(0.0, 0.25)
    b[i] <- bb[i] - mean(bb[1:NI])
    # log-normal distribution mean-log = 0.15, sd-log = 0.2 
    a[i] ~ dlnorm(0.15, 25)
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

#########################################################
### 2-class 2PL correct model
# FOR openBUGS
# mixture 2PL IRT specification, Minor Class mean =2.5
model_2C_separated <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  # class proportion
  pi[1:C] ~ ddirich(prop[])
  # ability parameters
  # theta mu for C classes
  # standard deviation of theta for C classess
  # %_% is the dummy operator to prevent from throwing an error
  t_mu[1] ~ dnorm(0.0, 4.0)
  t_tau[1] <- 1.0
  t_mu[2] ~ dnorm(2.5, 4.0)
  t_tau[2] <- 4.0
  # simulee's class membership and ability
  for (s in 1:NS) {
    c_mem[s] ~ dcat(pi[1:C])
    theta[s] ~ dnorm(t_mu[c_mem[s]], t_tau[c_mem[s]])
  }
  # item parameters
  # a: discrimination; b: difficulty
  for(i in 1:(NI-5)){
    bb[i] ~ dnorm(0.0, 0.25)
    b1[i] <- bb[i] - mean(bb[1:(NI-5)])
    b2[i] ~ dnorm(-3.0, 1.0)
  }
  # the last 5 items are anchoring items
  bb1[1] <- -2.0
  bb1[2] <- -1.0
  bb1[3] <- 0.0
  bb1[4] <- 1.0
  bb1[5] <- 2.0
  bb2[1] <- bb1[1]
  bb2[2] <- bb1[2]
  bb2[3] <- bb1[3]
  bb2[4] <- bb1[4]
  bb2[5] <- bb1[5]
  for(i in 1:(NI-5)){
    # log-normal distribution  
    a1[i] ~ dlnorm(0.15, 25)
    # log-normal distribution 
    a2[i] ~ dlnorm(-0.8, 2.78)
  }
  for(i in (NI-4):NI){
    # log-normal distribution  
    a1[i] ~ dlnorm(0.15, 25)
    a2[i] ~ dlnorm(0.15, 25)
  }
  # Likelihood
  for (s in 1:NS){
    for (i in 1:(NI-5)){
      logit(p[s,i]) <- step(c_mem[s]-1.5) * (a2[i] * (theta[s] - b2[i])) + (1-step(c_mem[s]-1.5)) * (a1[i] * (theta[s] - b1[i]))
      r[s,i] ~ dbern(p[s,i])
    }
    for (i in (NI-4):NI){
      logit(p[s,i]) <- step(c_mem[s]-1.5) * (a2[i] * (theta[s] - bb2[i-50])) + (1-step(c_mem[s]-1.5)) * (a1[i] * (theta[s] - bb1[i-50]))
      r[s,i] ~ dbern(p[s,i])
    }
  }
}

# mixture 2PL IRT specification, Minor Class mean =1
model_2C_merged <- function(){
  ### constants
  # C, NS, NI, prop, 
  ### responses data
  for (s in 1:NS) {
    for (i in 1:NI) {
      r[s, i] <- resp[s, i]
    }
  }
  ### Priors
  # class proportion
  pi[1:C] ~ ddirich(prop[])
  # ability parameters
  # theta mu for C classes
  # standard deviation of theta for C classess
  # t_mu[1] ~ dnorm(0.0, 1.0E+2)
  t_mu[1] ~ dnorm(0.0, 4.0)
  t_tau[1] <- 1.0
  # t_mu[2] ~ dnorm(1.0, 1.0E+2)
  t_mu[2] ~ dnorm(1.0, 4.0)
  t_tau[2] <- 4.0
  # simulee's class membership and ability
  for (s in 1:NS) {
    c_mem[s] ~ dcat(pi[1:C])
    theta[s] ~ dnorm(t_mu[c_mem[s]], t_tau[c_mem[s]])
  }
  # item parameters
  # a: discrimination; b: difficulty
  for(i in 1:(NI-5)){
    bb[i] ~ dnorm(0.0, 0.25)
    b1[i] <- bb[i] - mean(bb[1:(NI-5)])
    b2[i] ~ dnorm(-2.0, 1.0)
  }
  # the last 5 items are anchoring items
  bb1[1] <- -2.0
  bb1[2] <- -1.0
  bb1[3] <- 0.0
  bb1[4] <- 1.0
  bb1[5] <- 2.0
  bb2[1] <- bb1[1]
  bb2[2] <- bb1[2]
  bb2[3] <- bb1[3]
  bb2[4] <- bb1[4]
  bb2[5] <- bb1[5]
  for(i in 1:(NI-5)){
    # log-normal distribution 
    a1[i] ~ dlnorm(0.15, 25)
    # log-normal distribution  
    a2[i] ~ dlnorm(-0.5, 2.78)
  }
  for(i in (NI-4):NI){
    # log-normal distribution 
    a1[i] ~ dlnorm(0.15, 25)
    a2[i] ~ dlnorm(0.15, 25)
  }
  # Likelihood
  for (s in 1:NS){
    for (i in 1:(NI-5)){
      logit(p[s,i]) <- step(c_mem[s]-1.5) * (a2[i] * (theta[s] - b2[i])) + (1-step(c_mem[s]-1.5)) * (a1[i] * (theta[s] - b1[i]))
      r[s,i] ~ dbern(p[s,i])
    }
    for (i in (NI-4):NI){
      logit(p[s,i]) <- step(c_mem[s]-1.5) * (a2[i] * (theta[s] - bb2[i-50])) + (1-step(c_mem[s]-1.5)) * (a1[i] * (theta[s] - bb1[i-50]))
      r[s,i] ~ dbern(p[s,i])
    }
  }
}

#########################################################
