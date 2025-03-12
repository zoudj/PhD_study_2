
# this script processes results from data_gen_part1.R and obtains item parameters for the Minor Class

# loading R packages
library(dplyr)
library(purrr)
library(rlist)
library(tidyr)
library(tibble)
library(ggplot2)
library(R2OpenBUGS)

# setting the working directory to the folder where OpenBUGS results are stored
# results of Minor Class (mean = 1) and (mena = 2.5) are stored in separate subfolders
setwd('./M1')
setwd('./M25')

# there are 30 replications. Each replication is stored in a independent subfolder separately
# finding all subfolders that store results of replications
folder_list <- list.dirs(full.names = F, recursive = F)
target_idx <- grepl('chain', folder_list, fixed = T)
folder_target <- folder_list[target_idx]

# retrieving OpenBUGS logs in 30 replications (each replication has a log file containing the info of entire process and a summary of results)
log_list <- map(folder_target, ~{
  print(paste0('Retrieve log from ', .x, '...'))
  bugs.log(paste0(.x, '/log.txt'))$stats})

# a function that retrieves item parameters from log files
get_para <- function(df = NULL,paras = c('a', 'b'), mystats = 'median'){
  temp_result <- map(paras, ~{
    temp_df <- df %>% 
      as.data.frame() %>% 
      mutate(para = rownames(.)) %>% 
      filter(grepl(paste0(.x, '\\['), para)) %>% 
      as.data.frame() %>% 
      select(all_of(mystats))
    return(temp_df)
  }) %>% list_cbind()
  colnames(temp_result) <- paras
  rownames(temp_result) <- NULL
  return(temp_result)
}

# a function that calculates item parameter stats(mean, minimum, maximum)
get_stats <- function(df = NULL, paras = c('a', 'b')){
  temp_result <- map(paras, ~{
    temp_df <- df %>% 
      as.data.frame() %>% 
      mutate(para = rownames(.)) %>% 
      filter(grepl(paste0(.x, '\\['), para)) %>% 
      as.data.frame()
    temp_stats <- temp_df$median %>% 
      {c(mean(.), min(.), max(.))}
    return(temp_stats)
  }) %>% list.cbind()
  colnames(temp_result) <- paras
  rownames(temp_result) <- c('mean', 'min', 'max')
  return(temp_result)
}

# a wrap-up function that gets item parameters and calculates item parameter stats for log list 
# the input is a log list, not a single log file
parse_log <- function(loglist = NULL){
  res_list <- map(loglist, ~{
    temp_para <- get_para(df = .x)
    temp_stats <- get_stats(df = .x)
    return(list(temp_para, temp_stats))
  })
}

# retrieving all log info
log_all <- parse_log(loglist = log_list)
# check item parameters
map(log_all, ~{round(.x[[2]], 2)})

# check ability mean
map_dbl(log_list, ~{.x['theta_mean', 'mean']})
theta_mean <- map_dbl(log_list, ~{.x['theta_mean', 'mean']}) 
theta_mean[theta_mean > 0.3]


# check item difficulty distribution
hist(log_m1[[1]][[1]][['b']])
hist(log_m1[[1]][[1]][['a']])

setwd('../')

# results for Minor Class (mean = 1)
log_m1 <- parse_log(loglist = log_list)
map(log_m1, ~{round(.x[[1]], 2)})
map(log_m1, ~{round(.x[[2]], 2)})
theta_m1 <- map_dbl(log_list, ~{.x['theta_mean', 'mean']})

# results for Minor Class (mean = 2.5)
log_m25 <- parse_log(loglist = log_list)
map(log_m25, ~{round(.x[[2]], 2)})
theta_m25 <- map_dbl(log_list, ~{.x['theta_mean', 'mean']})

##############################################

# importing item parameters for the Major Class
diff_c1 <- read.csv('diff_c1.csv') %>% .$x
disc_c1 <- read.csv('disc_c1.csv') %>% .$x

# # adding anchoring items to the end of the vector
# # item difficulties
# diff_c1_55 <- c(diff_c1, c(-2, -1, 0, 1, 2))
# write.csv(diff_c1_55, file = 'diff_c1_55.csv', row.names = F)
# hist(diff_c1, breaks = 15)
# range(diff_c1)
# mean(diff_c1)
# 
# hist(disc_c1, breaks = 15)
# range(disc_c1)
# mean(disc_c1)
# median(disc_c1)
# # item discriminations
# disc_c1_55 <- c(disc_c1, rep(mean(disc_c1), 5))
# write.csv(disc_c1_55, file = 'disc_c1_55.csv', row.names = F)


# aggregate data across 30 replications
a_m1 <- map(log_m1, ~{.x[[1]]$a}) %>% 
  list.cbind() %>% 
  as.data.frame() 

a_m1_est <- rowMeans(a_m1)
hist(a_m1_est, breaks = 15)
range(a_m1_est)
mean(a_m1_est)

disc_agg <- cbind(disc_c1, a_m1_est) %>% 
  cbind(a_m25_est) %>% 
  round(digits = 2) %>% 
  as.data.frame() %>% 
  mutate(item_idx = 1:nrow(.)) %>% 
  arrange(disc_c1)
write.csv(disc_agg, 'disc_agg.csv', row.names = F)


b_m1 <- map(log_m1, ~{.x[[1]]$b}) %>% 
  list.cbind() %>% 
  as.data.frame() 

b_m1_est <- rowMeans(b_m1)
hist(b_m1_est, breaks = 15)
range(b_m1_est)
mean(b_m1_est)

diff_agg <- cbind(diff_c1, b_m1_est) %>% 
  cbind(b_m25_est) %>% 
  round(digits = 2) %>% 
  as.data.frame() %>% 
  mutate(item_idx = 1:nrow(.)) %>% 
  arrange(diff_c1)
write.csv(diff_agg, 'diff_agg.csv', row.names = F)


a_m25 <- map(log_m25, ~{.x[[1]]$a}) %>% 
  list.cbind() %>% 
  as.data.frame() 

a_m25_est <- rowMeans(a_m25)
hist(a_m25_est, breaks = 15)
range(a_m25_est)
mean(a_m25_est)
cbind(disc_c1, a_m25_est) %>% 
  round(digits = 2) %>% 
  as.data.frame() %>% 
  arrange(disc_c1)


b_m25 <- map(log_m25, ~{.x[[1]]$b}) %>% 
  list.cbind() %>% 
  as.data.frame() 

b_m25_est <- rowMeans(b_m25)
hist(b_m25_est, breaks = 15)
range(b_m25_est)
mean(b_m25_est)
cbind(diff_c1, b_m25_est) %>% 
  round(digits = 2) %>% 
  as.data.frame() %>% 
  arrange(diff_c1)

# saving item parameters for Minor Class, including the anchoring items
write.csv(c(a_m1_est, rep(mean(disc_c1), 5)), file = 'disc_c2_m1_55.csv', row.names = F)
write.csv(c(a_m25_est, rep(mean(disc_c1), 5)), file = 'disc_c2_m25_55.csv', row.names = F)
write.csv(c(b_m1_est, c(-2, -1, 0, 1, 2)), file = 'diff_c2_m1_55.csv', row.names = F)
write.csv(c(b_m25_est, c(-2, -1, 0, 1, 2)), file = 'diff_c2_m25_55.csv', row.names = F)

# checking item parameters
c(b_m1_est, c(-2, -1, 0, 1, 2))
c(b_m25_est, c(-2, -1, 0, 1, 2))
c(a_m1_est, rep(mean(disc_c1), 5))
c(a_m25_est, rep(mean(disc_c1), 5))


