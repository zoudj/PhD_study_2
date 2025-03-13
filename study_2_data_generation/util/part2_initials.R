# created by DZ, Mar. 25, 2022
# this file stores functions that generate initial values for Bayesian analyses

### for 2PL models
# 1-class
# 2-class

# initial proportion values for the two classes
pi_list <- list(c(0.7,0.3), c(0.5, 0.5), c(0.3, 0.7))

# generating initial values for the use in OpenBUGS
gen_inits <- function(NI = NULL, NS = NULL, C = NULL, nchains = 3, separation = NULL) {
  inits <- list()
  if(C == 1 & is.null(separation)){
    # regular 2PL IRT case
    for(i in 1:nchains){
      inits[[i]] <- list(bb = rnorm(NI+5, 0, 1), a = rlnorm(NI+5, 0.15, 0.2), t_mu = rnorm(1, 0, 1), theta = runif(NS, -5, 5))
    }
  } else if(C == 2 & separation == 'separated'){
    # mixture 2PL IRT case, Minor Class mean = 2.5
    for (i in 1:nchains){
      inits[[i]] <- list(bb = rnorm(NI, 0, 1), b2 = rnorm(NI, -3, 0.5), a1 = rlnorm(NI+5, 0.15, 0.2), a2 = c(rlnorm(NI, -0.8, 0.6), rlnorm(5, 0.15, 0.2)), t_mu = c(rnorm(1, 0, 1), rnorm(1, 2.5, 0.5)), pi = pi_list[[i]], c_mem = rcat(NS, prob = c(0.5, 0.5)), theta = runif(NS, -6, 4))
    }
  } else if(C == 2 & separation == 'merged'){
    # mixture 2PL IRT case, Minor Class mean = 1
    for (i in 1:nchains){
      inits[[i]] <- list(bb = rnorm(NI, 0, 1), b2 = rnorm(NI, -2, 0.5), a1 = rlnorm(NI+5, 0.15, 0.2), a2 = c(rlnorm(NI, -0.5, 0.6), rlnorm(5, 0.15, 0.2)), t_mu = c(rnorm(1, 0, 1), rnorm(1, 1.0, 0.5)), pi = pi_list[[i]], c_mem = rcat(NS, prob = c(0.5, 0.5)), theta = runif(NS, -6, 4))
    }
  } 
  return(inits)
}