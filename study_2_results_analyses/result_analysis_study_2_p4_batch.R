# this script is run for executing result_analysis_study_4_p1.R across all folders that store OpenBUGS results 

# it also calculate R hat to check the convergence of Markov chains
library(dplyr)
library(coda)
library(ggplot2)
library(purrr)
library(MCMCvis)
library(rlist)

# get directories in the target directory
folders <- list.dirs(path = './', recursive = F)
folder_idx <- grepl('Mixture', folders)
folder_t <- folders[folder_idx]
folder_t

for(j in seq_along(folder_t)){
  source('./util/util_OpenBUGS.R')
  source('./util/util_result.R')
  # set working directory
  setwd(folder_t[j])
  print(paste0('Working on ', folder_t[j]))
  # load result analysis file
  source('../result_analysis_study_2_p4.R')
  # reset working directory
  setwd('../')
}

# condition names
cdn_name <- map_chr(folder_t, ~{
  .x %>% 
    strsplit(split = '//', fixed = T) %>% 
    unlist() %>% 
    `[`(2)
})


# Markov chain summaries
mcmc_sum_all <- list()
for(j in seq_along(cdn_name)){
  cdn_idx <- cdn_name[j] %>%
    strsplit(split = '_', fixed = T) %>%
    unlist() %>%
    `[`(2)
  load(file = paste0('./results_new/', cdn_name[j], '/Mixture_', cdn_idx, '_Rhat.RData'))
  mcmc_sum_all[[j]] <- rhat_all
  rm(list = setdiff(ls(), c("cdn_name", "folder_t", "mcmc_sum_all")))
}

# mcmc_sum_all structure
# 50 reps -> 2 models -> summary data frame

# R hat for regular IRT results
rhat_1cwr <- map(mcmc_sum_all, ~{
  temp_df <- map(.x, ~{
    filter_idx <- grepl('theta', rownames(.x[[1]]))
    rhat_1cwr <- range(.x[[1]]$Rhat[filter_idx], na.rm = T)
    # rhat_2cco <- range(.x[[2]]$Rhat, na.rm = T)
  }) %>% 
    list.rbind() %>% 
    as.data.frame() %>% 
    rename(min = V1, max = V2)
  return(temp_df)
}) %>% 
  list.cbind() %>% 
  as.data.frame()
colnames(rhat_1cwr) <- paste0(rep(c('min', 'max'), 8), rep(1:8, each = 4))

# R hat for mixture IRT results
rhat_2cco <- map(mcmc_sum_all, ~{
  temp_df <- map(.x, ~{
    filter_idx <- grepl('theta', rownames(.x[[2]]))
    rhat_2cco <- range(.x[[2]]$Rhat[filter_idx], na.rm = T)
    # rhat_2cco <- range(.x[[2]]$Rhat, na.rm = T)
  }) %>% 
    list.rbind() %>% 
    as.data.frame() %>% 
    rename(min = V1, max = V2)
  return(temp_df)
}) %>% 
  list.cbind() %>% 
  as.data.frame()

colnames(rhat_2cco) <- paste0(rep(c('min', 'max'), 8), rep(1:8, each = 4))

rhat_2cco %>% 
  select(starts_with('max')) %>% 
  summarise_all(.funs = max)

# mean of R hat
rhat_2cco_mean <- map(mcmc_sum_all, ~{
  temp_df <- map(.x, ~{
    filter_idx <- grepl('theta', rownames(.x[[2]]))
    rhat_2cco <- mean(.x[[2]]$Rhat[filter_idx], na.rm = T)
  }) %>% 
    list.rbind() %>% 
    as.data.frame() %>% 
    rename(mean = V1)
  return(temp_df)
}) %>% 
  list.cbind() %>% 
  as.data.frame()
colnames(rhat_2cco_mean) <- paste0('mean', rep(1:8, each = 2))

original_vector <- 1:16

# Switch odd and even numbers
switched_vector <- ifelse(original_vector %% 2 == 0, original_vector - 1, original_vector + 1)
names(rhat_2cco_mean) <- paste0('mean', switched_vector)

# range of R hat
rhat_2cco_mean %>% 
  summarise_all(.funs = mean) %>% 
  map_dbl(.f = mean) %>% 
  range()

# median of R hat
rhat_2cco_median <- map(mcmc_sum_all, ~{
  temp_df <- map(.x, ~{
    filter_idx <- grepl('theta', rownames(.x[[2]]))
    rhat_2cco <- median(.x[[2]]$Rhat[filter_idx], na.rm = T)
  }) %>% 
    list.rbind() %>% 
    as.data.frame() %>% 
    rename(median = V1)
  return(temp_df)
}) %>% 
  list.cbind() %>% 
  as.data.frame()
colnames(rhat_2cco_median) <- paste0('median', rep(1:8, each = 2))

rhat_2cco_median %>% 
  summarise_all(.funs = mean)
