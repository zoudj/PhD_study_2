

# get directories in the target directory
dirs <- list.dirs(path = './', recursive = F)
# ignore seed directories, only find directories that contain Markov chains
target_idx <- grepl('chain', dirs, fixed = TRUE)
# target directories
dirs_t <- dirs[target_idx]

# count the number of files in each target directory
file_num <- vector()
# identify directory seed number
dir_seed <- vector()
seed_ignore <- vector()


for(t in seq_along(dirs_t)){
  # parse directory name to get directory seed number
  temp_str1 <- dirs_t[t] %>%
    strsplit(split = '/', fixed = T) %>% 
    unlist()
  temp_str2 <- temp_str1[length(temp_str1)] %>% 
    strsplit(split = '_', fixed = T) %>% 
    unlist() %>% 
    `[`(1)
  dir_seed[t] <- temp_str2
  
  # check if the chain is finished normally or early
  if(file.exists(paste0(dirs_t[t], '/log.txt'))){
    temp_log <- bugs.log(paste0(dirs_t[t], '/log.txt')) %>% 
      `[[`(1)
    if(is.matrix(temp_log)){
      temp_sample_size <- temp_log[, dim(temp_log)[2]] %>% 
        unique()
      if(!(temp_sample_size %in% c(2500, 5000))){
        print(dirs_t[t])
        seed_ignore <- append(seed_ignore, temp_str2)
      }
    } else{
      print(dirs_t[t])
      seed_ignore <- append(seed_ignore, temp_str2)
    }
  }
  # count the nubmer of files in each target directory
  # if the chain is not finished normally, the number of files would not equal 9 (the initial number of files is 3)
  temp_files <- list.files(path = dirs_t[t])
  file_num[t] <- length(temp_files)
  if((file_num[t] != 9) & (file_num[t] != 3)){
    print(dirs_t[t])
  }
  if(file_num[t] != 9){
    seed_ignore <- append(seed_ignore, temp_str2)
  }
}

dir_seed <- unique(dir_seed)
seed_ignore <- unique(seed_ignore)
seed_keep <- setdiff(dir_seed, seed_ignore)
print(paste0('Valid replications: ', length(seed_keep)))

# checking the number of valid replications, just in case there are abnormal results
if(length(seed_keep) > 50){
  set.seed(123)
  seed_final <- sample(seed_keep, size = 50)
} else if(length(seed_keep) <= 50){
  seed_final <- seed_keep
} 
print(paste0('Final selected replications: ', length(seed_final)))
if(length(seed_keep) < 50){
  warning('Final selected replications are less than 50!')
}
##################################################
# get the index of experimental conditions from the directory name
condition_idx <- getwd() %>% 
  strsplit(split = '/', fixed = T) %>% 
  unlist() %>% 
  `[`(length(.)) %>% 
  strsplit(split = '_', fixed = T) %>% 
  unlist() %>% 
  `[`(2)

# import data in the target directory
# class indices, true ability, item parameters
class_idx <- read.csv('./true_class_idx.csv', header = T)
ability <- read.csv('./true_ability.csv', header = T)
diff_c1_file <- list.files(pattern = 'item_diff_c1', full.names = F)
diff_c1 <- read.csv(diff_c1_file, header = T)
disc_c1_file <- list.files(pattern = 'item_disc_c1', full.names = F)
disc_c1 <- read.csv(disc_c1_file, header = T)

diff_c2_file <- list.files(pattern = 'item_diff_c2', full.names = F)
diff_c2 <- read.csv(diff_c2_file, header = T)
disc_c2_file <- list.files(pattern = 'item_disc_c2', full.names = F)
disc_c2 <- read.csv(disc_c2_file, header = T)

# OpenBUGS results
data_all <- map(seed_final, ~{get_data(seed_idx = .x)})
# ability estimates derived from 1-calss regular IRT 
theta_1cwr <- map(data_all, ~{.x[[1]] %>% 
    select(starts_with('theta'))})
# ability estimates derived from 2-calss mixture IRT 
theta_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('theta'))})

# item discrimination estimates
a_1cwr <- map(data_all, ~{.x[[1]] %>% 
    select(starts_with('a'))})
a1_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('a1'))})
a2_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('a2'))})
# item difficulty estimates
b_1cwr <- map(data_all, ~{.x[[1]] %>% 
    select(starts_with('b'))})
b1_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('b1')) })
b2_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('b2')) })

# class membership estimates derived from mixtue IRT
cmem_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('c_mem')) %>% 
    filter(row.names(.) %in% c('median_2cco')) %>% 
    as.vector() %>% 
    unlist()}) %>% 
    list.rbind() %>% 
    as.data.frame()
# class proportion estimates
pi_2cco <- map(data_all, ~{.x[[2]] %>% 
    select(starts_with('pi'))})
pi_ratio <- map_dbl(pi_2cco, ~{.x[1,1]/.x[1,2]})

theta_1cwr_mean <- map_df(theta_1cwr, ~{.x['mean_1cwr',]}) 
theta_2cco_mean <- map_df(theta_2cco, ~{.x['mean_2cco',]}) 
theta_1cwr_sd <- map_df(theta_1cwr, ~{.x['sd_1cwr',]}) 
theta_2cco_sd <- map_df(theta_2cco, ~{.x['sd_2cco',]}) 

a_1cwr_mean <- map_df(a_1cwr, ~{.x['mean_1cwr',]})
a1_2cco_mean <- map_df(a1_2cco, ~{.x['mean_2cco',]}) 
a2_2cco_mean <- map_df(a2_2cco, ~{.x['mean_2cco',]}) 
a1_2cco_sd <- map_df(a1_2cco, ~{.x['sd_2cco',]}) 
a2_2cco_sd <- map_df(a2_2cco, ~{.x['sd_2cco',]}) 

b_1cwr_mean <- map_df(b_1cwr, ~{.x['mean_1cwr',]})
b1_2cco_mean <- map_df(b1_2cco, ~{.x['mean_2cco',]}) 
b2_2cco_mean <- map_df(b2_2cco, ~{.x['mean_2cco',]}) 
b1_2cco_sd <- map_df(b1_2cco, ~{.x['sd_2cco',]}) 
b2_2cco_sd <- map_df(b2_2cco, ~{.x['sd_2cco',]}) 


# calculate SE & RMSE
se_theta_2cco <- calc_se(df = theta_2cco_mean)
se_theta_1cwr <- calc_se(df = theta_1cwr_mean)
# mean(se_theta_1cwr)
# mean(se_theta_2cco)
# range(se_theta_1cwr)
# range(se_theta_2cco)

rmse_theta_1cwr <- calc_rmse(df = theta_1cwr_mean, true = ability)
rmse_theta_2cco <- calc_rmse(df = theta_2cco_mean, true = ability)
# mean(rmse_theta_1cwr)
# mean(rmse_theta_2cco)
# range(rmse_theta_1cwr)
# range(rmse_theta_2cco)

# 95% credible interval coverage for ability estimates
covr_theta_2cco <- calc_covr(df_mean = theta_2cco_mean, df_sd = theta_2cco_sd, true = ability$x)

covr_theta_1cwr <- calc_covr(df_mean = theta_1cwr_mean, df_sd = theta_1cwr_sd, true = ability)

# range(covr_theta_1cwr)
# range(covr_theta_2cco)

# coverage length
covr_len_theta_2cco <- calc_covr_len(df = theta_2cco_mean)
covr_len_theta_1cwr <- calc_covr_len(df = theta_1cwr_mean)

# 95% credible interval coverage for item discrimination estimates
covr_a1_2cco <- calc_covr(df_mean = a1_2cco_mean, df_sd = a1_2cco_sd, true = disc_c1)
covr_a2_2cco <- calc_covr(df_mean = a2_2cco_mean, df_sd = a2_2cco_sd, true = disc_c2)
# 95% credible interval coverage for item difficulty estimates
covr_b1_2cco <- calc_covr(df_mean = b1_2cco_mean, df_sd = b1_2cco_sd, true = diff_c1[1:50,])
covr_b2_2cco <- calc_covr(df_mean = b2_2cco_mean, df_sd = b2_2cco_sd, true = diff_c2[1:50,])

##############################################
# calculate theta bias
bias_theta_1cwr <- calc_bias(df = theta_1cwr_mean, true = ability)
bias_theta_2cco <- calc_bias(df = theta_2cco_mean, true = ability)

# range(bias_theta_1cwr)
# range(bias_theta_2cco)
# mean(bias_theta_1cwr)
# mean(bias_theta_2cco)
# median(bias_theta_1cwr)
# median(bias_theta_2cco)

# visualize bias
p_bias_1cwr <- ggplot(mapping = aes(x=ability$x, y= bias_theta_1cwr))+
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(color = 'green4', alpha = 0.5) + 
  geom_smooth(mapping = aes(x=ability$x, y= bias_theta_1cwr), method = 'loess', color = 'green4') + 
  labs(x = expression(True~Ability~(theta))) + 
  scale_y_continuous(expression(Bias~of~hat(theta)))
# p_bias_1cwr

p_bias_2cco <- ggplot(mapping = aes(x=ability$x, y= bias_theta_2cco))+ 
  geom_hline(yintercept = 0, linetype = "dashed") + 
  geom_point(color = 'brown', alpha = 0.5) + 
  geom_smooth(mapping = aes(x=ability$x, y= bias_theta_2cco), method = 'loess', color = 'brown') + 
  labs(x = expression(True~Ability~(theta))) + 
  scale_y_continuous(expression(Bias~of~hat(theta)))
# p_bias_2cco

# incorporate two bias plots in one 
p_bias <- ggplot(mapping = aes(x=ability$x, y= bias_theta_1cwr))+
  geom_point(color = 'green4', alpha = 0.5)+ 
  geom_point(aes(x=ability$x, y=bias_theta_2cco), inherit.aes = F, color = 'brown', alpha = 0.5) + 
  geom_smooth(mapping = aes(x=ability$x, y= bias_theta_1cwr), method = 'loess', color = 'green4') + 
  geom_smooth(mapping = aes(x=ability$x, y= bias_theta_2cco), method = 'loess', color = 'brown') 
# p_bias

##############################################

# bias, SE, coverage by latent class, mixture IRT only
theta_by_cls <- data.frame('t_bias' = bias_theta_2cco, 't_se' = se_theta_2cco, 't_covr' = covr_theta_2cco, 'c_idx' = class_idx$x)


theta_by_cls %>% 
  reframe(across(t_bias:t_covr, range), .by = c_idx)

theta_est_2cco <- theta_2cco_mean %>% 
  reframe(across(`theta[1]`:`theta[600]`, mean)) %>% 
  t() %>% 
  as.data.frame()

df_dens <- data.frame('Ability' = ability$x, 'Class_idx' = as.character(class_idx$x), 'ab_2cco' = theta_est_2cco$V1)

# mixture IRT result, density model by true classes
p_dens <- ggplot(data = df_dens) + 
  geom_density(aes(x = Ability, color = Class_idx, linetype = '1'), adjust = 1) + 
  geom_density(aes(x= ab_2cco, color = Class_idx, group = Class_idx, linetype = '2'), adjust = 1, alpha = 0.7) + 
  scale_y_continuous(limits = c(NA, 1.2), n.breaks = 6) +
  scale_x_continuous(limits = c(-4, 4), n.breaks = 6)+ 
  labs(x = expression(Ability(theta)), y = 'Density') +
  scale_linetype_manual(name = 'Line Types', values = c(1, 2), labels = c("True Value", "Mixture IRT")) + 
  scale_color_manual(name = 'Classes', values = c("darkorange", "turquoise4"), labels = c("Class 1", "Class 2"))
# p_dens
##############################################

# calculate class membership accuracy
cmem_accuracy <- vector()
for(i in 1:nrow(cmem_2cco)){
  temp <- cmem_2cco[i,] %>%
    as.vector() %>%
    unlist()
  crosstab <- table(temp, class_idx$x, deparse.level = 2)
  if(dim(crosstab)[1] == 2){
    accuracy <- (crosstab[1,1] + crosstab[2,2])/sum(crosstab)
  } else if(dim(crosstab)[1] == 1){
    if(rownames(crosstab) == '1'){
      accuracy <- crosstab[1,1]/sum(crosstab)
    } else {
      accuracy <- crosstab[1,2]/sum(crosstab)
    }
  } else {
    print("Something is wrong in the crosstab.")
    accuracy <- NA
  }
  cmem_accuracy[i] <- accuracy
}

############################################################
# save all results as a image
save.image(file = paste0('../results_new/', folder_name, '/results_V1.RData'))
# remove all objects in the workspace, except for target folder.
rm(list = setdiff(ls(), "folder_t"))

