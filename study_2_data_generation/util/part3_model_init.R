

# library(R2OpenBUGS)

gen_inits <- function(M = NULL, NI = NULL, NS = NULL, nchains = 3, seed = NULL) {
  print(paste("Use seed: ", seed, sep = ""))
  inits <- list()
  if(M == 1){
    # when mean of Minor Class equal to 1
    for (i in 1:nchains){
      inits[[i]] <- list(b = rnorm(NI, -1, 1), a = rlnorm(NI, -0.5, 0.6), t_mu = rnorm(1, 1, 0.2), theta = runif(NS, -0.5, 2.5))
    }
  } else if(M == 25){
    # when mean of Minor Class equal to 2.5
    for (i in 1:nchains){
      inits[[i]] <- list(b = rnorm(NI, -2, 1), a = rlnorm(NI, -0.5, 0.6), t_mu = rnorm(1, 2.5, 0.2), theta = runif(NS, 1, 4))
    }
  }
  
  return(inits)
}
