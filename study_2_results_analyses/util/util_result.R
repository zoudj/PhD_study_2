


### functions
# calculating mean of a parameter from a Markov chain
calc_mean <- function(chain = NULL, rowname = NA){
  mymean <- chain %>%
    as.data.frame() %>%
    summarise_if(is.numeric, mean)
  row.names(mymean) <- rowname
  return(mymean)
}
# calculating standard deviation of a parameter from a Markov chain
calc_sd <- function(chain = NULL, rowname = NA){
  mysd <- chain %>%
    as.data.frame() %>%
    summarise_if(is.numeric, sd)
  row.names(mysd) <- rowname
  return(mysd)
}
# calculating median of a parameter from a Markov chain
calc_median <- function(chain = NULL, rowname = NA){
  mymedian <- chain %>%
    as.data.frame() %>%
    summarise_if(is.numeric, median)
  row.names(mymedian) <- rowname
  return(mymedian)
}
# retrieve data from Markov chains
# returning data in the chain (not the chain itself)
get_data <- function(seed_idx = NULL){
  print(paste0('Working on: ', seed_idx))
  # 1C wrong model data
  chain1 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_1C_wrong_chain1')) %>% 
    `[[`(1) %>% `[[`(1)
  chain2 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_1C_wrong_chain2')) %>% 
    `[[`(1) %>% `[[`(1)
  chain3 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_1C_wrong_chain3')) %>% 
    `[[`(1) %>% `[[`(1)
  # combine 3 chains
  chain_all <- rbind(chain1, chain2) %>%
    rbind(chain3)
  # calculate mean, SD, median
  mean_all <- calc_mean(chain = chain_all, rowname = 'mean_1cwr')
  sd_all <- calc_sd(chain = chain_all, rowname = 'sd_1cwr')
  median_all <- calc_median(chain = chain_all, rowname = 'median_1cwr') 
  data_1cwr <- rbind(mean_all, sd_all) %>% 
    rbind(median_all)
  attr(data_1cwr, 'seed') <- seed_idx
  # 2C correct model data
  chain1 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_2C_correct_chain1')) %>% 
    `[[`(1) %>% `[[`(1)
  chain2 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_2C_correct_chain2')) %>% 
    `[[`(1) %>% `[[`(1)
  chain3 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_2C_correct_chain3')) %>% 
    `[[`(1) %>% `[[`(1)
  # combine 3 chains
  chain_all <- rbind(chain1, chain2) %>%
    rbind(chain3)
  # calculate mean, SD, median
  mean_all <- calc_mean(chain = chain_all, rowname = 'mean_2cco')
  sd_all <- calc_sd(chain = chain_all, rowname = 'sd_2cco')
  median_all <- calc_median(chain = chain_all, rowname = 'median_2cco') 
  data_2cco <- rbind(mean_all, sd_all) %>%   
    rbind(median_all)
  attr(data_2cco, 'seed') <- seed_idx
  return(list(data_1cwr, data_2cco))
}

#######################################################
# retrieve chain data from Markov chains
# returning chains
get_chains <- function(seed_idx = NULL){
  print(paste0('Working on: ', seed_idx))
  # 1C wrong model data
  chain1 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_1C_wrong_chain1'))  %>% 
    `[[`(1) %>% `[[`(1)
  chain2 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_1C_wrong_chain2'))  %>% 
    `[[`(1) %>% `[[`(1)
  chain3 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_1C_wrong_chain3')) %>% 
    `[[`(1) %>% `[[`(1)
  # combine 3 chains
  chains_1cwr <- mcmc.list(chain1, chain2, chain3)
  # 2C correct model data
  chain1 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_2C_correct_chain1'))  %>% 
    `[[`(1) %>% `[[`(1)
  chain2 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_2C_correct_chain2'))  %>% 
    `[[`(1) %>% `[[`(1)
  chain3 <- get_ob_chain(folder_path = paste0('./', seed_idx, '_BUGS_2C_correct_chain3'))  %>% 
    `[[`(1) %>% `[[`(1)
  # combine 3 chains
  chains_2cco <- mcmc.list(chain1, chain2, chain3)
  return(list(chains_1cwr, chains_2cco))
}

#######################################################
# calculate classification accuracy
calc_accu <- function(est_class = NULL, true_class = NULL){
  temp_tab <- table(est_class, true_class, deparse.level = 2)
  if(dim(temp_tab)[1] %in% c(2L, 3L)){
    # in rare cases, '1.5' will appear in the crosstab
    accuracy <- (temp_tab['1', '1'] + temp_tab['2', '2'])/sum(temp_tab)
  } else if(dim(temp_tab)[1] == 1){
    if(rownames(temp_tab) == '1'){
      accuracy <- temp_tab['1', '1']/sum(temp_tab)
    } else {
      accuracy <- temp_tab['2', '2']/sum(temp_tab)
    }
  } else {
    print("Something is wrong in the crosstab.")
    accuracy <- NA
  }
  return(accuracy)
}


# calculate bias
calc_bias <- function(df = NULL, true = NULL){
  n_rep <- nrow(df)
  n_para <- ncol(df)
  temp_bias <- vector()
  for(bb in 1:n_para){
    temp_bias[bb] <- sum(df[, bb] - true$x[bb]) / n_rep 
  }
  return(temp_bias)
}
# calculate standard error
# empirical SE
calc_se <- function(df = NULL){
  n_rep <- nrow(df)
  n_para <- ncol(df)
  temp_se <- vector()
  for(bb in 1:n_para){
    dd <- df[, bb] - mean(df[, bb])
    temp_se[bb] <- (sum(dd^2) / (n_rep - 1)) %>% sqrt()
  }
  return(temp_se)
}

# calculate RMSE
calc_rmse <- function(df = NULL, true = NULL){
  n_rep <- nrow(df)
  n_para <- ncol(df)
  temp_rmse <- vector()
  for(bb in 1:n_para){
    dd <- df[, bb] - true$x[bb]
    temp_rmse[bb] <- (sum(dd^2) / (n_rep - 1)) %>% sqrt()
  }
  return(temp_rmse)
}

# calculate coverage, Proportion of times the 100(1 − alpha)% confidence interval include the true value
calc_covr <- function(df_mean = NULL, df_sd = NULL, true = NULL){
  covr <- vector()
  if(is.data.frame(true)){
    true <- true$x
  }
  for(i in 1:nrow(df_mean)){
    covr_idx <- pmap_df(list(df_mean[i,], df_sd[i, ], true), ~{
      ifelse((..3 <= (..1 + 1.96*..2)) & (..3 >= (..1 - 1.96*..2)), 1L, 0L)
    })
    # print(table(covr_idx))
    covr[i] <- sum(covr_idx)/length(covr_idx)
  }
  return(covr)
}

# calculate coverage length
calc_covr_len <- function(df = NULL){
  # n_para <- ncol(df)
  # temp_se <- calc_se(df = df)
  # return(sum(2*1.96*temp_se) / n_para) 
  temp_se <- calc_se(df = df)
  return(2*1.96*temp_se)
}
