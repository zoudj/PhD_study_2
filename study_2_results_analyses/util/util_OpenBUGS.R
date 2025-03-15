# created by DZ, Jan 11, 2022
library(R2OpenBUGS)
# library(dplyr)
library(tidyverse)
library(rlist)
# library(coda)
# library(ggplot2)
library(patchwork)

# read chain data from a folder where OpenBUGS results are stored

# get log files from OpenBUGS results
get_ob_log <- function(folder_path = NULL){
  # first, get sub-folders
  # if there are multiple chain results and they are stored in separate sub-folders, like sub-folder chain1, chain2, chain3, etc.
  # if there's no sub-folder, it means all results are stored in one folder
  sub_folder_path <- list.dirs(folder_path, full.names = TRUE, recursive = FALSE)
  if(!is_empty(sub_folder_path)){
    # when all results are stored in separate sub-folders
    n_chain <- length(sub_folder_path)
    log_list <- list()
    for(i in 1:n_chain){
      file_path <- sub_folder_path[i]
      log_list[[i]] <- bugs.log(paste(file_path, 'log.txt', sep = '/'))
    }
    names(log_list) <- paste('chain', 1:n_chain, 'log', sep = '_')
    return(log_list)
  } else{
    # when all results are stored in one folder
    # there's only one log file
    ob_log <- bugs.log(paste(folder_path, 'log.txt', sep = '/'))
    # return the log data (a list, containing DIC and stats information)
    return(ob_log)
    # df_dic <- 'log_dic'
    # df_stats <- 'log_chain_stats'
    # # assign temp to df_name then return
    # eval(call("<-", as.name(df_dic), temp_log$DIC))
    # eval(call("<-", as.name(df_stats), as.data.frame(temp_log$stats)))
    
  }
}

# get chain data from OpenBUGS results
get_ob_chain <- function(folder_path = NULL){
  # first, get sub-folders
  # if there are multiple chain results and they are stored in separate sub-folders, like sub-folder chain1, chain2, chain3, etc.
  # if there's no sub-folder, it means all results are stored in one folder
  sub_folder_path <- list.dirs(folder_path, full.names = TRUE, recursive = FALSE)
  if(!is_empty(sub_folder_path)){
    # when all results are stored in separate sub-folders
    n_chain <- length(sub_folder_path)
    chain_list <- list()
    for(i in 1:n_chain){
      file_path <- sub_folder_path[i]
      chain_list[[i]] <- read.bugs(paste(file_path, 'CODAchain1.txt', sep = '/'))
    }
  } else{
    # when all results are stored in one folder
    # first, find chain data file, starting with 'CODAchain'
    chain_files <- list.files(path = folder_path, pattern = '^CODAchain', full.names = F, recursive = F)
    n_chain <- length(chain_files)
    chain_list <- list()
    for(i in 1:n_chain){
      chain_list[[i]] <- read.bugs(paste(folder_path, chain_files[i], sep = '/'))
    }
  }
  names(chain_list) <- paste('chain', 1:n_chain, 'data', sep = '_')
  return(chain_list)
}

###########################################################
# parameter stats in the OpenBUGS log file is a matrix, convert it to a data frame and convert the rowname to a column
convert_log_stats <- function(log_stats = NULL){
  df <- log_stats %>%
    as.data.frame() %>%
    mutate(parameter = rownames(.))
  return(df)
}

# parameter stats in the OpenBUGS log file contains all stats of monitored parameters, select only parameters of interest 
select_stats <- function(log_stats = NULL, para = NULL){
  df <- log_stats %>%
    filter(str_detect(parameter, para))
  return(df)
}

get_para_stats <- function(log_path = NULL, para = NULL){
  df_log <- get_ob_log(folder_path = log_path)
  names_log <- names(df_log)
  # if there's only one log file
  if('stats' %in% names_log & 'DIC' %in% names_log & length(df_log) == 2){
    df_para_stats <- df_log$stats %>%
      convert_log_stats() %>%
      select_stats(para = para)
    return(df_para_stats)
  }
  # if there are more than one log file
  if(!('stats' %in% names_log) & !('DIC' %in% names_log)){
    n_log <- length(df_log)
    para_stats_list <- list()
    for(n in 1:n_log){
      df_para_stats <- df_log[[n]]$stats %>%
        convert_log_stats() %>%
        select_stats(para = para)
      para_stats_list[[n]] <- df_para_stats
    }
    return(para_stats_list)
  }
}


##############################################################
# get chain data for a given parameter, for the purpose of drawing a trace plot
# para should be a string of parameter name
get_iteration <- function(chain = NULL, para = NULL){
  n_chain <- length(chain)
  chain_name <- paste('chain', 1:n_chain, sep = '')
  chain_data <- list()
  for(i in 1:n_chain){
    chain_data[[i]] <- chain[[i]][[1]] %>%
      as.data.frame() %>%
      select(all_of(para))
    colnames(chain_data[[i]]) <- chain_name[i]
  }
  # combine chain data into a data frame
  df_chain <- list.cbind(chain_data)
  iteration_vector <- df_chain %>%
    rownames() %>%
    as.integer()
  df_chain$iteration <- iteration_vector
  return(df_chain)
}
###########################################################
### draw trace plots
draw_trace <- function(chain = NULL, para = NULL, save_path = NULL){
  # prepare data
  df_trace <- get_iteration(chain = chain, para = para)
  df_trace_long <- df_trace %>%
    pivot_longer(!iteration, names_to = 'chain', values_to = 'value')
  # draw a trace plot
  p_trace <- ggplot(data = df_trace_long, aes(x = iteration, y = value, group = chain, color = chain)) + geom_line(alpha = 0.8) + theme(legend.position = "bottom", plot.title = element_text(hjust = 0.5)) + labs(title = paste('Trace Plot of', para, 'from', deparse(substitute(chain)), sep = ' '))
  p_trace
  # save plot
  if(is.null(save_path)){
    ggsave(paste('Trace_Plot_of', para, 'from', deparse(substitute(chain)), '.pdf', sep = '_'), width = 2000, height = 800, units = 'px')
  } else{
    ggsave(paste(save_path, '/Trace_Plot_of', para, 'from', deparse(substitute(chain)), '.pdf', sep = '_'), width = 2000, height = 800, units = 'px')
  }
}


###########################################################
### draw density plot
## prepare data
# get class membership iterations for each simulee
# two sub-population scenario
get_cprob <- function(chain = NULL, simulee = NULL){
  df_cmem <- get_iteration(chain = chain, para = paste('c_mem[', simulee, ']', sep = ''))
  df_cmem_long <- df_cmem %>%
    pivot_longer(!iteration, names_to = 'chain', values_to = 'value')
  cprob <- df_cmem_long %>%
    summarise(cprob = table(.$value)/nrow(.))
  return(cprob)
}


# construct a data frame for plotting density lines
# columns: simulee index, estimated class, class 1 probability, class 2 probability, true class, true ability, estimated ability
get_iteration_batch <- function(chain = NULL, para = NULL){
  n_chain <- length(chain)
  chain_name <- paste('chain', 1:n_chain, sep = '')
  chain_data <- list()
  for(i in 1:n_chain){
    chain_data[[i]] <- chain[[i]][[1]] %>%
      as.data.frame() %>%
      select(starts_with(para))
    # colnames(chain_data[[i]]) <- chain_name[i]
  }
  # n_iter <- nrow(chain_data[[1]])
  # iter_vector <- rep(rownames(chain_data[[1]]), n_chain)
  # combine chain data into a data frame
  df_chain <- list.rbind(chain_data)
  rownames(df_chain) <- NULL
  # df_chain$iteration <- paste('chain', rep(1:n_chain, each = n_iter), '_', iter_vector, sep = '')
  return(df_chain)
}

get_cprob_batch <- function(data = NULL){
  df_cprob <- data.frame('cprob1' = NA_real_ * ncol(data), 'cprob2' = NA_real_)
  for(i in 1:ncol(data)){
    cprob <- table(data[[i]])
    if(!is.na(cprob['1'])){
      df_cprob[i, 'cprob1'] <- cprob['1']/ nrow(data)
      if(df_cprob[i, 'cprob1'] == 1.0){
        df_cprob[i, 'cprob2'] <- 0
      }
    }
    if(!is.na(cprob['2'])){
      df_cprob[i, 'cprob2'] <- cprob['2']/ nrow(data)
      if(df_cprob[i, 'cprob2'] == 1.0){
        df_cprob[i, 'cprob1'] <- 0
      }
    }
  }
  return(df_cprob)
}


get_est_theta <- function(chain = NULL, use_mean = TRUE){
  n_chain <- length(chain)
  theta_list <- list()
  for(n in 1:n_chain){
    temp_theta <- chain[[n]][[1]] %>%
      as.data.frame() %>%
      select(starts_with('theta')) %>%
      select(-c('theta_mean'))
    rownames(temp_theta) <- paste('chain', n, '_', rownames(temp_theta), sep = '')
    theta_list[[n]] <- temp_theta
  }
  df_theta <- list.rbind(theta_list)
  est_theta <- vector()
  if(use_mean){
    for(i in 1:ncol(df_theta)){
      est_theta[i] <- mean(df_theta[, i])
    }
  } else{
    for(i in 1:ncol(df_theta)){
      est_theta[i] <- median(df_theta[, i])
    }
  }
  # return(df_theta)
  return(est_theta)
}

build_dens_df <- function(chain = NULL, true_class = NULL, true_ability = NULL, use_mean = TRUE){
  n_chain <- length(chain)
  df_iter <- get_iteration_batch(chain = chain, para = 'c_mem')
  df_cprob <- get_cprob_batch(data = df_iter)
  n_simulee <- nrow(df_cprob)
  df_cprob$simulee <- 1:n_simulee
  # assign class membership
  df_dens <- df_cprob %>%
    mutate(est_class = case_when(
      cprob1 > cprob2 ~ 1L,
      cprob1 < cprob2 ~ 2L,
      cprob1 == cprob2 ~ NA_integer_
    )) 
  df_dens$true_class <- true_class$x
  df_dens$true_ability <- true_ability$x
  if(use_mean){
    df_dens$est_ability <- get_est_theta(chain = chain, use_mean = TRUE)
    print('Use mean as estimated ability.')
  } else{
    df_dens$est_ability <- get_est_theta(chain = chain, use_mean = FALSE)
    print('Use median as estimated ability.')
  }
  df_equal_prob <- df_dens %>%
    filter(cprob1 == cprob2)
  if(nrow(df_equal_prob) != 0){
    print(paste(nrow(df_equal_prob), "row(s) of equal probability in both classes detected!", sep = ' '))
  }
  df_dens <- df_dens %>%
    filter(!is.na(est_class)) %>%
    mutate(misclass = ifelse(est_class == true_class, "no", "yes")) %>%
    mutate(mc = str_c(true_class, '->', est_class)) %>%
    mutate(diff2 = est_ability - true_ability)
  return(df_dens)
}

get_classification_crosstab <- function(data = NULL){
  table(data$true_class, data$est_class, deparse.level = 2)
}

# draw separate density plot
draw_density_line <- function(data = NULL, save_as = NULL){
  plot_df_est <- data %>%
    select(simulee, est_class, est_ability, misclass, mc) %>%
    mutate(group = 'est')
  colnames(plot_df_est) <- c('simulee', 'class', 'ability', 'misclass', 'mc', 'group')
  
  plot_df_true <- data %>%
    select(simulee, true_class, true_ability, misclass, mc) %>%
    mutate(group = 'true')
  colnames(plot_df_true) <- c('simulee', 'class', 'ability', 'misclass', 'mc', 'group')
  
  plot_df <- rbind(plot_df_est, plot_df_true) %>%
    mutate(gc = str_c(group, '_', class)) 
  # data of misclassification
  plot_df_mc <- plot_df %>%
    filter(misclass == 'yes' & group == 'est')
  
  # plotting
  p_density <- ggplot(data = plot_df, aes(x = ability, group = gc, color = gc)) + geom_density(aes(linetype = group)) + scale_linetype_manual(values = c("true" = "solid", "est" = "dashed")) + geom_jitter(data = plot_df_mc, aes(x = ability, y = 0, color = mc), size = 0.05, height = 0.01, alpha = 0.5) + labs(x = 'Ability', y = 'Density', color = "Types") + ggtitle("Separate Density Plot of Estimated and True Abilities") + theme(plot.title = element_text(size = 10))
  p_density
  if(is.null(save_as)){
    ggsave(paste("Density_plot_", Sys.Date(), ".pdf", sep = ''), width = 2000, height = 1200, units = 'px')
  } else{
    ggsave(save_as, width = 2000, height = 1200, units = 'px')
  }
  
}

draw_misclass <- function(data = NULL, box_show = NULL, save_as = NULL){
  plot_df_est <- data %>%
    select(simulee, est_class, est_ability, misclass, mc) %>%
    mutate(group = 'est')
  colnames(plot_df_est) <- c('simulee', 'class', 'ability', 'misclass', 'mc', 'group')
  
  plot_df_true <- data %>%
    select(simulee, true_class, true_ability, misclass, mc) %>%
    mutate(group = 'true')
  colnames(plot_df_true) <- c('simulee', 'class', 'ability', 'misclass', 'mc', 'group')
  
  plot_df <- rbind(plot_df_est, plot_df_true) %>%
    mutate(gc = str_c(group, '_', class)) 
  # data of misclassification
  plot_df_mc <- plot_df %>%
    filter(misclass == 'yes' & group == 'est')
  
  # create a summary mean and sd data frame
  ability_mean_sd <- plot_df %>%
    group_by(gc) %>%
    summarise(mean = mean(ability), sd = sd(ability)) 
  
  # create a data frame for plotting difference between estimated and true ability
  # get true-value data
  data_true <- plot_df %>%
    filter(group == 'true') 
  colnames(data_true) <- paste(colnames(data_true), "true", sep = "_")
  # get estimated-value data
  data_est <- plot_df %>%
    filter(group == 'est')
  colnames(data_est) <- paste(colnames(data_est), "est", sep = "_")
  
  # combine data for reordering by true ability 
  data_combined <- cbind(data_true, data_est) %>%
    arrange(ability_true) %>%
    tibble::rownames_to_column( var = "new_idx") %>%
    mutate(new_idx = as.integer(new_idx)) %>%
    mutate(type = case_when(ability_true > ability_est ~ "smaller", ability_true < ability_est ~ "larger", ability_true == ability_est ~ "equal")) %>%
    mutate(diff = ability_est - ability_true)
  
  # data frame for boxplot
  # get sorted true-value data
  data_true_sort <- data_combined %>%
    select(c(ends_with("_true"), "new_idx")) 
  # get estimated-value data with the same order as the above
  data_est_sort <- data_combined %>%
    select(c(ends_with("_est"), "new_idx"))
  
  # get a subset of misclassification from the combined data
  data_misclass <- data_combined %>%
    filter(misclass_est == "yes", group_est == "est")
  
  # horizontal lines
  hline <- geom_hline(data = ability_mean_sd, aes(yintercept = mean, linetype = c("true_1" = "solid", "true_2" = "solid", "est_1" = "dashed", "est_2" = "dashed")), alpha = 0.7, show.legend = T)
  
  # segments of misclassification with colors
  misclass_seg <- geom_segment(data = data_misclass, aes(x = new_idx, y = ability_true, xend = new_idx, yend = ability_est, color = mc_est), size = 0.2, inherit.aes = F) 
  # true ability points
  true_ab_pt <- geom_point(aes(x = new_idx, y = ability_true, color = as.factor(class_true)), size = 0.05, alpha = 0.7)
  # item difficulty points
  item_diff_pt <- geom_point(data = difficulty, aes(x = -50, y = item_diff), size = 0.1, inherit.aes = FALSE)
  
  # set a common y axis limit
  ylimits <- coord_cartesian(ylim = c(min(data_combined$ability_true), max(data_combined$ability_true)))
  
  # draw
  p_diff <- ggplot(data = data_combined) + 
    hline + #hline_text + text_seg + 
    geom_segment(aes(x = new_idx, y = ability_true, xend = new_idx, yend = ability_est), size = 0.2, color = "gray60")  +
    misclass_seg + true_ab_pt + item_diff_pt + 
    geom_segment(x = -40, y = 1.8, xend = 60, yend = 1.8, color = "gray40") +
    geom_text(x = 60, y = 1.8, label = "item difficulty", hjust = 0, size = 3, color = "gray40") +
    labs(title = paste("Difference of Estimated and True Ability"), x = "Simulees", y = "Ability") +  
    # scale_colour_manual(limits = c("1", "2", "1->2", "2->1"), labels = c("c1", "c2", "c1->c2", "c2->c1")) + 
    scale_linetype_manual(values = c("solid", "dashed"), labels = c("true class mean", "estimated class mean")) +        
    theme(legend.key=element_rect(fill=NA), legend.position = "bottom", legend.title = element_blank(), legend.key.size = unit(0.5, 'cm'), plot.title = element_text(size=7)) + 
    guides(color = guide_legend(override.aes = list(size=0.5, stroke=0.5),  nrow = 2, byrow = T), linetype = guide_legend(nrow = 2)) + 
    ylimits
  
  # regular boxplot
  pbox <- ggplot(plot_df) +
    hline +
    geom_boxplot(aes(x = gc, y = ability, group = gc), linetype = c("solid", "dashed", "solid", "dashed"), outlier.size = 0.05, notch = F) +
    scale_x_discrete(limits=c("true_1", "est_1", "true_2", "est_2")) +
    labs(title = "Boxplot of Ability by Class", x = "Classes") +
    theme(axis.title.y = element_blank(), legend.position = "none", plot.title = element_text(size=7)) + scale_y_continuous(position = "right") +
    ylimits
  
  # boxplot with mean and sd
  pbox_msd <- ggplot(plot_df) +
    hline + geom_boxplot(data = ability_mean_sd, aes(x = gc, lower=mean-sd,upper=mean+sd,middle=mean,ymin=mean-3*sd,ymax=mean+3*sd), stat="identity", linetype = c("solid", "dashed", "solid", "dashed")) +
    scale_x_discrete(limits=c("true_1", "est_1", "true_2", "est_2")) +
    labs(title = "Ability Boxplot of Mean and SD", x = "Classes") +
    theme(axis.title.y = element_blank(), legend.position = "none", plot.title = element_text(size=7)) +               scale_y_continuous(position = "right") +
    ylimits
  
  # boxplot indicating mean of difference between estimated and true ability
  pbox_diff <- ggplot(data = data_combined) +
    geom_boxplot(mapping = aes(x = class_true, y = diff, group = as.factor(class_true)), outlier.size = 0.05) + 
    stat_summary(aes(x = class_true, y = diff), fun="mean", geom="point") +
    scale_x_discrete(limits=c("class_1", "class_2")) +
    labs(title = "Boxplot of Estimated and True Ability Difference", x = "Classes") +
    theme(axis.title.y = element_blank(), legend.position = "none", plot.title = element_text(size=6)) + 
    scale_y_continuous(position = "right")
  
  # boxplot indicating mean of difference between estimated and true ability
  pbox_diff2 <- ggplot(data = data) +
    geom_boxplot(mapping = aes(x = mc, y = diff2, group = as.factor(mc)), outlier.size = 0.05) + 
    stat_summary(aes(x = mc, y = diff2), fun="mean", geom="point") +
    scale_x_discrete(limits=c("1->1", "1->2", "2->2", "2->1")) +
    labs(title = "Boxplot of Estimated and True Ability Difference", x = "Classification") +
    theme(axis.title.y = element_blank(), legend.position = "none", plot.title = element_text(size=6)) + 
    scale_y_continuous(position = "right")
  
  
  # difference segment plot without boxplot
  p1 <- p_diff
  # difference segment plot with regular boxplot
  p2 <- p_diff + pbox + plot_layout(ncol=2,widths=c(5,2))
  # difference segment plot with boxplot (showing mean and sd of sub-populations)
  p3 <- p_diff + pbox_msd + plot_layout(ncol=2,widths=c(5,2))
  # difference segment plot with boxplot (showing difference of estimated and true ability)
  p4 <- p_diff + pbox_diff + plot_layout(ncol=2,widths=c(5,2))
  p5 <- p_diff + pbox_diff2 + plot_layout(ncol=2,widths=c(5,2))
  #save plot
  if(is.null(box_show)){
    if(is.null(save_as)){
      ggsave(paste("Difference_segment_plot_", Sys.Date(), ".pdf", sep = ''), plot = p1, width = 2000, height = 1200, units = 'px')
    } else{
      ggsave(save_as, plot = p1, width = 2000, height = 1200, units = 'px')
    }
  } else if(box_show == 'regular'){
    if(is.null(save_as)){
      ggsave(paste("Difference_segment_plot_with_regular_boxplot_", Sys.Date(), ".pdf", sep = ''), plot = p2, width = 2000, height = 1200, units = 'px')
    } else{
      ggsave(save_as, plot = p2, width = 2000, height = 1200, units = 'px')
    }
  } else if(box_show == 'msd'){
    if(is.null(save_as)){
      ggsave(paste("Difference_segment_plot_with_boxplot_mean_sd_", Sys.Date(), ".pdf", sep = ''), plot = p3, width = 2000, height = 1200, units = 'px')
    } else{
      ggsave(save_as, plot = p3, width = 2000, height = 1200, units = 'px')
    }
  } else if(box_show == 'diff'){
    if(is.null(save_as)){
      ggsave(paste("Difference_segment_plot_with_boxplot_diff_", Sys.Date(), ".pdf", sep = ''), plot = p4, width = 2000, height = 1200, units = 'px')
    } else{
      ggsave(save_as, plot = p4, width = 2000, height = 1200, units = 'px')
    }
  } else if(box_show == 'diff2'){
    if(is.null(save_as)){
      ggsave(paste("Difference_segment_plot_with_boxplot_diff2_", Sys.Date(), ".pdf", sep = ''), plot = p5, width = 2000, height = 1200, units = 'px')
    } else{
      ggsave(save_as, plot = p5, width = 2000, height = 1200, units = 'px')
    }
  }
}
###########################################################
### functions for 1-class models

build_dens_df_1c <- function(chain = NULL, true_class = NULL, true_ability = NULL, use_mean = TRUE){
  n_chain <- length(chain)
  # n_simulee <- nrow(df_cprob)
  df_1c <- data.frame('simulee' = 1: nrow(true_class), 'true_class' = true_class$x, 'true_ability' = true_ability$x)
  if(use_mean){
    df_1c$est_ability <- get_est_theta(chain = chain, use_mean = TRUE)
    print('Use mean as estimated ability.')
  } else{
    df_1c$est_ability <- get_est_theta(chain = chain, use_mean = FALSE)
    print('Use median as estimated ability.')
  }
  df_1c <- df_1c %>%
    mutate(diff = est_ability - true_ability)
  return(df_1c)
}


draw_density_1c <- function(data = NULL, save_as = NULL){
  plot_df_est <- data %>%
    select(simulee, est_ability) %>%
    mutate(group = 'est', class = 'est_1_class')
  colnames(plot_df_est) <- c('simulee', 'ability', 'group', 'class')
  
  plot_df_true <- data %>%
    select(simulee, true_class, true_ability) %>%
    mutate(group = 'true', class = case_when(true_class == 1 ~ 'true_c1', true_class == 2 ~ 'true_c2')) %>%
    select(simulee, true_ability, group, class)
  colnames(plot_df_true) <- c('simulee', 'ability', 'group', 'class')
  
  plot_df_true_all <- data %>%
    select(simulee, true_ability) %>%
    mutate(group = 'true', class = 'true_overall')
  
  colnames(plot_df_true_all) <- c('simulee', 'ability', 'group', 'class')
  
  plot_df <- rbind(plot_df_est, plot_df_true, plot_df_true_all) 
  
  p_density <- ggplot(data = plot_df) + geom_density(aes(x = ability, color = class, group = class, linetype = group)) +  scale_linetype_manual(values = c("true" = "solid", "est" = "dashed")) + labs(x = 'Ability', y = 'Density', color = "Group", linetype = 'Linetype') + ggtitle("Separate and Overall Density Plot of Estimated and True Abilities - 1 class") + theme(plot.title = element_text(size = 10))
  p_density
  if(is.null(save_as)){
    ggsave(paste("Density_plot_1_class_model_", Sys.Date(), ".pdf", sep = ''), width = 2000, height = 1200, units = 'px')
  } else{
    ggsave(save_as, width = 2000, height = 1200, units = 'px')
  }
}


###########################################################
###########################################################
###########################################################
###########################################################
###########################################################
###########################################################