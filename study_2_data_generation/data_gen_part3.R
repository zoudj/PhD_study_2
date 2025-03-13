
# this script generates item parameters (item difficulties, item discriminations) for the Minor Class.

# for conditions that test difficulty range covers both classes
# loading R packages used in this script
library(dplyr)
library(purrr)
library(rlist)
library(tidyr)
library(R2OpenBUGS)
# loading utility scripts
source(file = './util/part3_initials.R')
source(file = './util/part3_models.R')

# prepare response data, models and initials

# set random seeds to generate reproductive results
# seed_v1 <- sample(100:999, 100)
# write.csv(seed_v1, "seed_v1_2023Oct24.csv", row.names = F)
# seed_v2 <- sample(100:999, 100)
# write.csv(seed_v2, "seed_v2_2023Oct24.csv", row.names = F)

# importing random seed lists
seed_m1 <- read.csv("seed_v1_2023Oct24.csv") %>%
  `$`(x)
seed_m25 <- read.csv("seed_v2_2023Oct24.csv") %>%
  `$`(x)
# number of items
n_item <- 50
# number of simulees
n_person <- 1000


# calculate model score
calc_model_score <- function(a = NULL, b = NULL, theta = NULL){
  z <- a * (theta - b)
  p <- exp(z)/(1 + exp(z))
  return(p)
}

# select columns and convert the long format data frame into a wide format data frame
long2wide <- function(long_data){
  long_data %>%
    dplyr::select(item_id, person_id, s_score) %>%
    spread(item_id, s_score) %>%
    return()
}
#####################################################
# generate true values of item difficulty and discrimination
# generate true ability for class 2 (minor class)
# generate response data

# generate item parameters
# c1 refers to the Major Class, m1 refers to Minor Class mean = 1
para_c1_m1 <- map(1:100, ~{
  set.seed(seed = .x)
  diff_c1 <- rnorm(n = n_item, mean = 0.5, sd = 1.5)
  disc_c1 <- rlnorm(n = n_item, meanlog = 0.15, sdlog = 0.2)
  return(list('diff' = diff_c1, 'disc' = disc_c1))
})

# checking characteristics in c1_m1
map(para_c1_m1, ~{.x[[1]] %>% range()})
map(para_c1_m1, ~{.x[[2]] %>% range()})
para_c1_m1[[54]][[1]] %>% sort()
para_c1_m1[[54]][[2]] %>% sort()
hist(para_c1_m1[[54]][[1]], breaks = 15)
hist(para_c1_m1[[54]][[2]], breaks = 15)
# c1 refers to the Major Class, m25 refers to Minor Class mean = 2.5
para_c1_m25 <- map(1:100, ~{
  set.seed(seed = .x)
  diff_c1 <- rnorm(n = n_item, mean = 1.5, sd = 1.5)
  disc_c1 <- rlnorm(n = n_item, meanlog = 0.15, sdlog = 0.2)
  return(list('diff' = diff_c1, 'disc' = disc_c1))
})

# checking item difficulty range
map(para_c1_m25, ~{.x[[1]] %>% 
    range()})

# checking characteristics in c1_m25
para_c1_m25[[99]][[1]] %>% sort()
para_c1_m25[[99]][[2]] %>% sort()
hist(para_c1_m25[[99]][[1]], breaks = 15)
hist(para_c1_m25[[99]][[2]], breaks = 15)

# select results from seed 54 for m1
diff_c1_m1 <- para_c1_m1[[54]][[1]] 
disc_c1_m1 <- para_c1_m1[[54]][[2]] 

write.csv(diff_c1_m1, file = 'diff_c1_m1.csv', row.names = F)
write.csv(disc_c1_m1, file = 'disc_c1_m1.csv', row.names = F)

# select results from seed 99 for m25
diff_c1_m25 <- para_c1_m25[[99]][[1]] 
disc_c1_m25 <- para_c1_m25[[99]][[2]] 

write.csv(diff_c1_m25, file = 'diff_c1_m25.csv', row.names = F)
write.csv(disc_c1_m25, file = 'disc_c1_m25.csv', row.names = F)

# generating response data for Minor Class (mean = 1)
# 30 replications (i.e., 30 sets of response data)
resp_m1 <- map(seed_m1[1:30], ~{
  set.seed(seed = .x)
  ls <- list()
  ls$diff_c1 <- diff_c1_m1
  ls$disc_c1 <- disc_c1_m1
  ab_m1 <- rnorm(n = n_person, mean = 1, sd = 0.5)
  ls$ab_m1 <- ab_m1
  df_item <- data.frame(item_id = 1:n_item, diff = diff_c1_m1, disc = disc_c1_m1) 
  df_person_m1 <- data.frame(person_id = 1:length(ab_m1), ability = ab_m1)
  df_long_m1 <- merge(df_item, df_person_m1) %>% 
    mutate(
      m_score = calc_model_score(a = disc, b = diff, theta = ability),
      random = runif(n = nrow(.), min = 0, max = 1),
      s_score = ifelse(m_score >= random, 1, 0)
    ) 
  # response data
  resp_m1 <- long2wide(df_long_m1)
  ls$df_long_m1 <- df_long_m1
  ls$resp_m1 <- resp_m1
  
  return(ls)
})

# check parameters
# map(resp_m1, ~{.x$diff_c1 %>% mean()})
# map(resp_m1, ~{.x$diff_c1 %>% range()})
# map(resp_m1, ~{.x$disc_c1 %>% mean()})
# map(resp_m1, ~{.x$disc_c1 %>% range()})
# map(resp_m1, ~{.x$ab_m1 %>% mean()})
# map(resp_m1, ~{.x$ab_m1 %>% sd()})

# generating response data for Minor Class (mean = 2.5)
# 30 replications (i.e., 30 sets of response data)
resp_m25 <- map(seed_m25[1:30], ~{
  set.seed(seed = .x)
  ls <- list()
  ls$diff_c1 <- diff_c1_m25
  ls$disc_c1 <- disc_c1_m25
  ab_m25 <- rnorm(n = n_person, mean = 2.5, sd = 0.5)
  ls$ab_m25 <- ab_m25
  df_item <- data.frame(item_id = 1:n_item, diff = diff_c1_m25, disc = disc_c1_m25) 
  
  df_person_m25 <- data.frame(person_id = 1:length(ab_m25), ability = ab_m25)
  df_long_m25 <- merge(df_item, df_person_m25) %>% 
    mutate(
      m_score = calc_model_score(a = disc, b = diff, theta = ability),
      random = runif(n = nrow(.), min = 0, max = 1),
      s_score = ifelse(m_score >= random, 1, 0)
    ) 
  resp_m25 <- long2wide(df_long_m25)
  ls$df_long_m25 <- df_long_m25
  ls$resp_m25 <- resp_m25
  
  return(ls)
})

# check parameters
# map(resp_m25, ~{.x$diff_c1 %>% mean()})
# map(resp_m25, ~{.x$diff_c1 %>% range()})
# map(resp_m25, ~{.x$disc_c1 %>% mean()})
# map(resp_m25, ~{.x$disc_c1 %>% range()})
# map(resp_m25, ~{.x$ab_m25 %>% mean()})
# map(resp_m25, ~{.x$ab_m25 %>% sd()})

#####################################################
# saving response data, initial values, corresponding models for the use in OpenBUGS

# creating new subfolders for storing data
# M1 refers to the Minor Class (mean = 1)
dir.create('M1')
# M25 refers to the Minor Class (mean = 2.5)
dir.create('M25')

s <- 100L
setwd('./M1')
walk2(seed_m1[1:30], resp_m1, ~{
  temp_dir <- paste0('./seed', .x)
  if (file.exists(temp_dir)){
    setwd(temp_dir)
  } else {
    dir.create(temp_dir)
    setwd(temp_dir)
  }
  # response data
  df_res <- .y$resp_m1[, 2:ncol(.y$resp_m1)] %>%
    as.matrix()
  data_BUGS <- list("NS" = n_person, "NI" = n_item, "resp" = df_res)
  bugs.data(data_BUGS, dir = "./", digits = 5, data.file = "data_bugs_m1.txt")
  # initials
  inits <- gen_inits(M = 1, NI = n_item, NS = n_person, nchains = 1, seed = seed_m1[s])
  s <<- s - 1
  bugs.inits(inits, n.chains = 1, digits = 5, inits.files = paste("./inits", 1, ".txt", sep = ""))
  # model
  write.model(model_reg_m1, "./model_reg_m1.txt")
  # item paramters
  write.csv(.y$diff_c1, 'diff_c1.csv', row.names = F)
  write.csv(.y$disc_c1, 'disc_c1.csv', row.names = F)
  # person abilities
  write.csv(.y$ab_m1, 'ab_m1.csv', row.names = F)
  setwd('../')
})


# create working directories and copy data to each directory
folders <- list.dirs(full.names = F, recursive = F)
walk(folders, ~{
  model_file <- list.files(path = paste0('./', .x), pattern = 'model', full.names = T)
  data_file <- list.files(path = paste0('./', .x), pattern = 'data', full.names = T)
  for(w in 1:1){
    init_file <- paste0(.x, '/inits', w, '.txt')
    dest_dir <- paste0(.x, '_bugs_chain', w)
    dir.create(dest_dir)
    file.copy(from = c(model_file, data_file, init_file), to = dest_dir)
  }
})

setwd('../')
######################################

s <- 100L
setwd('./M25')
walk2(seed_m25[1:30], resp_m25, ~{
  temp_dir <- paste0('./seed', .x)
  if (file.exists(temp_dir)){
    setwd(temp_dir)
  } else {
    dir.create(temp_dir)
    setwd(temp_dir)
  }
  # response data
  df_res <- .y$resp_m25[, 2:ncol(.y$resp_m25)] %>%
    as.matrix()
  data_BUGS <- list("NS" = n_person, "NI" = n_item, "resp" = df_res)
  bugs.data(data_BUGS, dir = "./", digits = 5, data.file = "data_bugs_m25.txt")
  # initials
  inits <- gen_inits(M = 25, NI = n_item, NS = n_person, nchains = 1, seed = seed_m25[s])
  s <<- s - 1
  bugs.inits(inits, n.chains = 1, digits = 5, inits.files = paste("./inits", 1, ".txt", sep = ""))
  # model
  write.model(model_reg_m25, "./model_reg_m25.txt")
  # item paramters
  write.csv(.y$diff_c1, 'diff_c1.csv', row.names = F)
  write.csv(.y$disc_c1, 'disc_c1.csv', row.names = F)
  # person abilities
  write.csv(.y$ab_m25, 'ab_m25.csv', row.names = F)
  setwd('../')
})

# create working directories and copy data to each directory
folders <- list.dirs(full.names = F, recursive = F)
walk(folders, ~{
  model_file <- list.files(path = paste0('./', .x), pattern = 'model', full.names = T)
  data_file <- list.files(path = paste0('./', .x), pattern = 'data', full.names = T)
  for(w in 1:1){
    init_file <- paste0(.x, '/inits', w, '.txt')
    dest_dir <- paste0(.x, '_bugs_chain', w)
    dir.create(dest_dir)
    file.copy(from = c(model_file, data_file, init_file), to = dest_dir)
  }
})

setwd('../')






