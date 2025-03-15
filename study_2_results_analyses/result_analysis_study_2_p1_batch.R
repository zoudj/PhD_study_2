# this script is run for executing result_analysis_study_2_p1.R across all folders that store OpenBUGS results 

library(dplyr)
library(coda)
library(ggplot2)
library(rlist)

# get directories in the target directory
folders <- list.dirs(path = './', recursive = F)
folder_idx <- grepl('Mixture', folders)
folder_t <- folders[folder_idx]
folder_t

for(j in 1:length(folder_t)){
  source('./util/util_OpenBUGS.R')
  source('./util/util_result.R')
  # set working directory
  setwd(folder_t[j])
  folder_name <- strsplit(folder_t[j], split = '//', fixed = T) %>% 
    unlist() %>% 
    `[`(2)
  dir.create(paste0('../results_new/', folder_name))
  print(paste0('Working on ', folder_t[j]))
  # load result analysis file
  source('../result_analysis_study_2_p1.R')
  # reset working directory
  setwd('../')
}


