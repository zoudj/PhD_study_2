
# calculate potential scale reduction factor, or PSRF
# check convergence of chains in each replication


# get directories in the target directory
dir_chains <- list.dirs(path = './', recursive = F)
# ignore seed directories
target_idx <- grepl('chain', dir_chains, fixed = TRUE)
dir_data <- dir_chains[target_idx]

# count the number of files in each target directory
file_num <- vector()
# identify directory seed number
dir_seed <- vector()
seed_ignore <- vector()

for(t in seq_along(dir_data)){
  # parse directory name to get directory seed number
  temp_str1 <- dir_data[t] %>%
    strsplit(split = '/', fixed = T) %>% 
    unlist()
  temp_str2 <- temp_str1[length(temp_str1)] %>% 
    strsplit(split = '_', fixed = T) %>% 
    unlist() %>% 
    `[`(1)
  dir_seed[t] <- temp_str2
  
  # check if the chain is finished normally or early
  if(file.exists(paste0(dir_data[t], '/log.txt'))){
    temp_log <- bugs.log(paste0(dir_data[t], '/log.txt')) %>% 
      `[[`(1)
    if(is.matrix(temp_log)){
      temp_sample_size <- temp_log[, dim(temp_log)[2]] %>% 
        unique()
      if(!(temp_sample_size %in% c(2500, 5000))){
        print(dirs_t[t])
        seed_ignore <- append(seed_ignore, temp_str2)
      }
    } else{
      print(dir_data[t])
      seed_ignore <- append(seed_ignore, temp_str2)
    }
  }
  # count the nubmer of files in each target directory
  temp_files <- list.files(path = dir_data[t])
  file_num[t] <- length(temp_files)
  if((file_num[t] != 9) & (file_num[t] != 3)){
    print(dir_data[t])
  }
  if(file_num[t] != 9){
    seed_ignore <- append(seed_ignore, temp_str2)
  }
}

dir_seed <- unique(dir_seed)
seed_ignore <- unique(seed_ignore)
seed_keep <- setdiff(dir_seed, seed_ignore)
if(length(seed_keep) > 50){
  set.seed(123)
  seed_final <- sample(seed_keep, size = 50)
} else if(length(seed_keep) <= 50){
  seed_final <- seed_keep
} 

condition_idx <- getwd() %>%
  strsplit(split = '/', fixed = T) %>%
  unlist() %>%
  `[`(length(.)) %>%
  strsplit(split = '_', fixed = T) %>%
  unlist() %>%
  `[`(2)

condition_name <- folder_t[j] %>% 
  strsplit(split = '//', fixed = T) %>% 
  unlist() %>% 
  `[`(2)

chains_all <- map(seed_final, get_chains)

rhat_all <- imap(chains_all, ~{
  print(paste0('Working on replication #', .y))
  rhat_1cwr <- MCMCsummary(.x[[1]], round = 3)
  rhat_2cco <- MCMCsummary(.x[[2]], round = 3)
  return(list(rhat_1cwr, rhat_2cco))
})


rm(chains_all, temp_log)
# save all results as a image
save.image(file = paste0('../results_new/', condition_name, '/Mixture_', condition_idx, '_Rhat.RData'))
# remove all objects in the workspace
rm(list = setdiff(ls(), "folder_t"))
