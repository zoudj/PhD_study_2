# this script generates simulated data for conditions that test difficulty range covers only the Major Class (8 conditions)
# including response data, initial values and specifications of IRT models for the use in OpenBUGS

# importing data generation scripts
source("./util/part2_data_generation.R")
source("./util/part2_models.R")
source("./util/part2_initials.R")
# loading R packages
library(purrr)
library(rlist)
library(dplyr)
library(R2OpenBUGS)

### variables to be manipulated
## two-class proportion
# 3:1, 5:1
## IRT model
# Rasch model: different item difficulty, but same item discrimination across items
# 2PL model: different item difficulties and item discriminations across items
## model class
# 1-class(regular IRT model, Bayesian approach), 2-class(Bayesian approach only)
## item difficulty distribution
# uniform distribution, normal distribution
## item difficulty coverage, covering all theta range or not
# full coverage, partial coverage
# smaple size
# 600, 1200, 2400

### fixed variables
# Set number of simulees
sim_n <- c(600L, 1200L)
# Set number of items
# suppose all items are dichotomous and within the same dimension
item_n <- 50
# set random seeds to generate reproductive results
# seed_vector_xxxx <- sample(1000:9999, 1000)
# write.csv(seed_vector_xxxx, "seed_vector_xxxx_2023Sept29.csv", row.names = F)
seed_vector <- read.csv("seed_vector_xxxx_2023Sept29.csv") %>%
  `$`(x)
# each replication has 3 Markov chains
N_chain <- 3
# number of replications
# creating data for 100 replications, but not necessarily all replications being run in OpenBUGS
N_rep <- 100
# seed index
s <- 1L
###########################################################
# class proportions
prop_v <- c("3:1", "5:1")
# range_v <- c("full", "C1only")
# since we only generate response data for conditions that test difficulty range covers only the Majo Class, the vector below only contains one element
# for conditions that test difficulty range covers both latent classes, see part3 and part4 scripts
range_v <- c("C1only")
# mean distance of abilities (i.e., class ability distribution in the dissertation)
# separated indicating m = 2.5 while merged indicating m = 1.0 for the Minor Class
mean_dist <- c("separated", "merged")
# the number of lattent classes
C <- 1:2


# aggregate experiment conditions
conditions <- vector()
cd <- 1
for(i in seq_along(sim_n)){
  for(j in seq_along(prop_v)){
    for(m in seq_along(mean_dist)){
      for(n in seq_along(range_v)){
        prop <- paste0(substr(prop_v[j],1,1), 'to', substr(prop_v[j],3,3))
        conditions <- append(conditions, paste('Mixture', cd, sim_n[i], prop, mean_dist[m], range_v[n], sep = '_'))
        cd <- cd + 1
      }
    }
  }
}


# experiment conditions and levels in each condition
conditions_split <- strsplit(conditions, split = '_', fixed = T)
df_conditions <- data.frame(Name = conditions) %>% 
  mutate(S_size = map_chr(conditions_split, ~{.x[3]}),
         Proportion = map_chr(conditions_split, ~{.x[4]}),
         M_distance = map_chr(conditions_split, ~{.x[5]}),
         Coverage = map_chr(conditions_split, ~{.x[6]}))

# combinations of sample size and proportion
# 4 levels
cd_sspp <- df_conditions %>% 
  select(S_size, Proportion) %>%  
  distinct(S_size, Proportion, .keep_all = TRUE)
# combinations of sample size, proportion and mean distance
# 8 levels
cd_ssppmd <- df_conditions %>% 
  select(S_size, Proportion, M_distance) %>%  
  distinct(S_size, Proportion, M_distance, .keep_all = TRUE)

# combinations of mean distance (class ability distribution) and item difficulty coverage (test difficulty range)
cd_md_cover <- df_conditions %>% 
  select(M_distance, Coverage) %>%  
  distinct(M_distance, Coverage, .keep_all = TRUE)

# IRT model type: 2PL
# item difficulty distribution: normal

set.seed(seed_vector[s])
s <- s + 1
# generate person class indices needed for 16 conditions
# using sample size and class proportion to generate class indexes
# 4 different person class index sets
gen_class_idx <- function(sspp = NULL){
  # sample size
  ss <- sspp$S_size %>% 
    as.integer()
  # class proportion
  pp <- sspp$Proportion %>% 
    map(get_prop)
  # generate class index
  class_idx <- map2(ss, pp, ~{get_class_idx(.x, .y)})
  # save data
  pwalk(list(class_idx, sspp$S_size, sspp$Proportion, 1:length(class_idx)), ~{write.csv(..1, file = paste('class_idx',..4, ..2, ..3, '.csv', sep = '_'), row.names = F)})
  return(class_idx)
}

# save true class indices
true_class_idx <- gen_class_idx(sspp = cd_sspp)
# check true class indices
map(true_class_idx, table)


# item parameters are imported from external files that were generated from Part 1
# diff refers to item difficulty
diff_c1 <- read.csv('diff_c1_55.csv')[['x']]

diff_c2_m1 <- read.csv('diff_c2_m1_55.csv')[['x']]
diff_c2_m25 <- read.csv('diff_c2_m25_55.csv')[['x']]
# disc refers to item discrimination
disc_c1 <- read.csv('disc_c1_55.csv')[['x']]

disc_c2_m1 <- read.csv('disc_c2_m1_55.csv')[['x']]
disc_c2_m25 <- read.csv('disc_c2_m25_55.csv')[['x']]
# minimum and maximum of item difficulties
diff_c1_max <- diff_c1 %>% range() %>% `[`(2)
diff_c1_min <- diff_c1 %>% range() %>% `[`(1)


# generate person ability sets needed for 8 conditions
# 8 different person ability sets
gen_true_ab <- function(ssppmd = NULL, class_idx = NULL){
  if( nrow(ssppmd) != length(class_idx)){
    warning('Length of true class idx list should be equal to rows of ssppmd.')
    break
  }
  # sample size
  ss <- ssppmd$S_size %>% 
    as.integer()
  # class proportion
  pp <- ssppmd$Proportion %>% 
    map(get_prop)
  # mean distance types
  md <- ssppmd$M_distance
  
  # generate true ability sets
  ability_list <- list()
  for(j in 1:nrow(ssppmd)){
    if(md[j] == 'separated'){
      repeat{
        # class ability distributions are specified here
        temp_ab  <- get_ability(N = ss[j], class_idx = class_idx[[j]], c1_m = 0, c1_v = 1, c2_m = 2.5, c2_v = 0.25)
        temp_range <- range(temp_ab[class_idx[[j]] == 1])
        # make sure the range of item difficulties covers the Major Class
        if(dplyr::between(temp_range[1] - diff_c1_min, left = 0, right = 0.15) & dplyr::between(temp_range[2] - diff_c1_max, left = -0.15, right = 0)){
          ability_list[[j]] <- temp_ab
          break
        }
      }
      
    } else if (md[j] == 'merged'){
      repeat{
        # class ability distributions are specified here
        temp_ab  <- get_ability(N = ss[j], class_idx = class_idx[[j]], c1_m = 0, c1_v = 1, c2_m = 1, c2_v = 0.25)
        temp_range <- range(temp_ab[class_idx[[j]] == 1])
        # make sure the range of item difficulties covers the Major Class
        if(dplyr::between(temp_range[1] - diff_c1_min, left = 0, right = 0.15) & dplyr::between(temp_range[2] - diff_c1_max, left = -0.15, right = 0)){
          ability_list[[j]] <- temp_ab
          break
        }
      }
      
    } else {
      print('The specification of mean distance is wrong.')
    }
  }
  # save data
  pwalk(list(ability_list, ssppmd$S_size, ssppmd$Proportion, ssppmd$M_distance, 1:length(ability_list)), ~{write.csv(..1, file = paste('ability',..5, ..2, ..3, ..4, '.csv', sep = '_'), row.names = F)})
  return(ability_list)
}

# using the above function to generate simulees' abilities
true_ability <- gen_true_ab(cd_ssppmd, class_idx = rep(true_class_idx, each =2))
# checking true ability range (mixture sample, including both classes)
true_ab_range <- map(true_ability, range)
true_ab_range
# check maximum ability in Major Class
map2_dbl(true_ability, rep(true_class_idx, each =2), ~{
  .x[.y == 1] %>%
    max()})
map2(true_ability, rep(true_class_idx, each =2), ~{
  .x[.y == 1] %>%
    range()})
# ability range in Major Class
true_ab_c1_range <- map2(true_ability, rep(true_class_idx, each =2), ~{.x[.y == 1] %>% range()}) %>% 
  list.rbind() %>% 
  as.data.frame()
names(true_ab_c1_range) <- c('min', 'max')
  
# check empirical ability SD
map_dbl(true_ability, sd)
map_dbl(true_ability, mean)
map_dbl(true_ability, median)



# create condition folders and copy files
for(j in 1:nrow(df_conditions)){
  dir.create(df_conditions$Name[j])
  setwd(df_conditions$Name[j])
  # true ability
  write.csv(true_ability[[j]], 'true_ability.csv', row.names = F)
  # true class index
  write.csv(rep(true_class_idx, each =2)[[j]], 'true_class_idx.csv', row.names = F)
  # item difficulty
  write.csv(diff_c1, 'item_diff_c1.csv', row.names = F)
  # item discrimination
  write.csv(disc_c1, 'item_disc_c1.csv', row.names = F)
  if(df_conditions$M_distance[j] == 'separated'){
    write.csv(diff_c2_m25, 'item_diff_c2_m25.csv', row.names = F)
    write.csv(disc_c2_m25, 'item_disc_c2_m25.csv', row.names = F)
  } else if(df_conditions$M_distance[j] == 'merged'){
    write.csv(diff_c2_m1, 'item_diff_c2_m1.csv', row.names = F)
    write.csv(disc_c2_m1, 'item_disc_c2_m1.csv', row.names = F)
  } 
  setwd('../')
}

##############################################
# a utility function
copy_data <- function(destination = dirname(getwd()), from_folder = NULL, N_chain = 3){
  # create a new directory and save data, model and itnitials
  current_folder <- strsplit(getwd(), split = '/', fixed = T)%>% 
    unlist() %>% 
    `[`(length(.))
  bugs_files <- list.files(from_folder)
  data_file <- bugs_files[startsWith(bugs_files, 'data')]
  model_file <- bugs_files[startsWith(bugs_files, 'model')]
  for(n in 1:N_chain){
    new_folder <- paste0(destination, '/', current_folder, '_', from_folder, '_chain', n)
    dir.create(new_folder)
    setwd(from_folder)
    file.copy(c(data_file, model_file, paste('inits', n, '.txt', sep = '')), new_folder)
    setwd('../')
  }
}


######################################################
# a utility function, for generating response data
rep_data_gen <- function(myseed = NULL, N_sim = NULL, N_item = NULL, myprop = NULL, mean_dist = NULL, N_chain = 3){
  # set random seed
  set.seed(myseed[s])
  root_folder <- getwd()
  # creat a new seed folder
  folder_temp <- paste0('seed', myseed[s])
  s <<- s + 1
  dir.create(folder_temp)
  setwd(folder_temp)
  # creat two new sub-folders
  dir.create("BUGS_2C_correct")
  # dir.create("BUGS_2C_wrong")
  dir.create("BUGS_1C_wrong")
  
  # get sub-population proportion
  prop <- gsub('to', ':', myprop) %>%
    get_prop()
  # get class index
  class_idx <- read.csv('../true_class_idx.csv') %>%
    `$`(x)
  # get individual ability
  ability <- read.csv('../true_ability.csv') %>%
    `$`(x)
  
  # get item difficulty
  difficulty_c1 <- read.csv('../item_diff_c1.csv') %>%
    `$`(x)
  diff_c2_file <- list.files(path = '../', pattern = 'item_diff_c2')
  difficulty_c2 <- read.csv(paste0('../', diff_c2_file)) %>%
    `$`(x)
  # get item discrimination
  discrimination_c1 <- read.csv('../item_disc_c1.csv') %>%
    `$`(x)
  disc_c2_file <- list.files(path = '../', pattern = 'item_disc_c2')
  discrimination_c2 <- read.csv(paste0('../', disc_c2_file)) %>%
    `$`(x)
  
  # generate response data
  df_person <- get_person_df(N_sim, class_idx, ability)
  df_item <- get_item_df(n = N_item+5, n_set = 2, diffi1 = difficulty_c1, discri1 = discrimination_c1, diffi2 = difficulty_c2, discri2 = discrimination_c2)
  df_long <- merge(df_item, df_person) %>%
    mutate(model_score = get_model_score(prop = prop, a1 = discrimination_c1, b1 = difficulty_c1, a2 = discrimination_c2, b2 = difficulty_c2, theta = ability))
  # convert probability to response scores, i.e., 0 or 1
  sim_score <- vector()
  temp_p <- runif(n = nrow(df_long), min = 0, max = 1)
  for(q in 1:nrow(df_long)){
    # sim_score[q] = rbern(1, df_long$model_score[q])
    if(df_long$model_score[q] >= temp_p[q]){
      sim_score[q] <- 1
    } else {
      sim_score[q] <- 0
    }
  }
  df_long$sim_score <- sim_score
  write.csv(df_long$model_score, 'model_score.csv', row.names = F)
  write.csv(df_long$sim_score, 'simulated_score.csv', row.names = F)
  ### convert long data format to wide data format
  df_wide <- long2wide(df_long)
  df_res <- df_wide[, 3:ncol(df_wide)] %>%
    as.matrix()
  # coverting response data to data files used in OpenBUGS
  # for C = 1, 1-class model
  data_BUGS_1c <- list("NS" = N_sim, "NI" = N_item+5, "resp" = df_res)
  bugs.data(data_BUGS_1c, dir = "./BUGS_1C_wrong/", digits = 5, data.file = "data_bugs_1c_wrong.txt")
  # for C = 2, 2-class model
  data_BUGS_2c <- list("NS" = N_sim, "NI" = N_item+5, "C" = 2, "prop" = prop, "resp" = df_res)
  bugs.data(data_BUGS_2c, dir = "./BUGS_2C_correct/", digits = 5, data.file = "data_bugs_2c.txt")
  
  # generate initial values and models
    # 2PL 1-class wrong model inits
    inits_c1 <- gen_inits(NI = N_item, NS = N_sim, C = 1,  nchains = N_chain)
    bugs.inits(inits_c1, n.chains = N_chain, digits = 5, inits.files = paste("./BUGS_1C_wrong/inits", 1:N_chain, ".txt", sep = ""))
    write.model(model_1C, "./BUGS_1C_wrong/model_1c_2PL_wr.txt")
    copy_data(from_folder = 'BUGS_1C_wrong')
    
    # 2PL 2-class correct model inits
    inits_c2 <- gen_inits(NI = N_item, NS = N_sim, C = 2,  nchains = N_chain, separation = mean_dist)
    bugs.inits(inits_c2, n.chains = N_chain, digits = 5, inits.files = paste("./BUGS_2C_correct/inits", 1:N_chain, ".txt", sep = ""))
    
    if(mean_dist == 'separated'){
      write.model(model_2C_separated, "./BUGS_2C_correct/model_2c_2PL_separated.txt")
    } else if(mean_dist == 'merged'){
      write.model(model_2C_merged, "./BUGS_2C_correct/model_2c_2PL_merged.txt")
    } else {
      print("Something is wrong with your 2-class model.")
    }
    
    
    copy_data(from_folder = 'BUGS_2C_correct')
    
  setwd("../")
}


######################################################
# finding generated directories that will store data, models, initial values for use in OpenBUGS
dirs <- list.dirs(full.names = F, recursive = F)
target_idx <- grepl('Mixture', dirs, fixed = TRUE)
dirs_target <- dirs[target_idx]
dirs_target
# check item difficulty range and ability range in each folder
walk(dirs_target, ~{
  temp_diff <- read.csv(file = paste0(.x, '/item_diff_c1.csv'))$x[1:50]
  temp_ab <- read.csv(file = paste0(.x, '/true_ability.csv'))$x
  temp_idx <- read.csv(file = paste0(.x, '/true_class_idx.csv'))$x
  temp_ab_c1 <- temp_ab[temp_idx == 1]
  print(c(range(temp_diff), range(temp_ab_c1)))
  print(length(temp_ab))
  print(table(temp_idx))
})

# generating and copying data to the target directories
# 100 replications in each experimental condition
for(d in seq_along(dirs_target)){
  dir_str <- strsplit(dirs_target[d], split = '_', fixed = T)%>% unlist()
  sim <- as.integer(dir_str[3])  
  prop <- dir_str[4]
  m_dist <- dir_str[5]
  setwd(dirs_target[d])
  for(r in 1:N_rep){
    rep_data_gen(myseed = seed_vector, N_sim = sim, N_item = item_n, myprop = prop, mean_dist = m_dist, N_chain = 3)
  }
  setwd('../')
}


