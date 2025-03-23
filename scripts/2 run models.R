# March 19th, 2025
#
# 
#' *Run models*
#
#
#
# Prepare workspace ---------------------------------------
#
rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2024-07-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "dplyr", 
          "cpp11", "withr", "colorspace", "mvtnorm",
          "foreach", "doSNOW")
groundhog.library(libs, groundhog.day)


# >>> Helper functions ----------------------------------------------------

prepare_cluster <- function(n){
  require("parallel")
  require("foreach")
  require("doSNOW")
  
  nCores <- parallel::detectCores() -1 
  cl <- makeCluster(nCores)
  registerDoSNOW(cl)
  
  # Progress bar
  pb <- txtProgressBar(max = n, style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  ret <- list(opts, pb, cl)
  names(ret) <- c("options", "progress", "cluster")
  return(ret)
  
  cat("Pass 'x$options' to .opts in foreach;
      'x$progress' to setTxtProgressBar(x$progress, i);
      'x$cluster' to stopCluster(x$cluster) after foreach")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# Load raw model guide & prepared data------------------------------------------------------------------

master_guide <- readRDS("outputs/main_text/data/master_guide.Rds")
master_guide

dat <- fread("data/master_data.csv")

# For I2 calculation with orchaRdr, we need to specify formulas with mods instead 
# of formulas:
master_guide[, mods := gsub("yi_smd ", "", formula)]

# Run models -------------------------------------------------------
file.remove(list.files("outputs/main_text/models/", full.names = T))

clust_out <- prepare_cluster(n = nrow(master_guide))

master_guide
m <- c()
sub.dat <- c()
i <- 1
#' *Note that some models won't run because there is insufficient N for their random effects*

success <- foreach(i = 1:nrow(master_guide), 
        .options.snow = clust_out$options,
        .errorhandling = "pass",
        .packages = c("data.table", "metafor")) %dopar% {
  
  sub.dat <- dat[eval(parse(text = master_guide[i, ]$exclusion)), ]
  
  #
  m <- rma.mv(yi = yi_smd,
              V = vi_smd,
              mods = as.formula(master_guide[i, ]$mods),
              random = eval(parse(text = master_guide[i, ]$random_effect)),
              dfs = "contain",
              test = "t",
              method = "ML",
              struct = "AR",
              data = sub.dat)

  saveRDS(m, master_guide[i, ]$model_path)
  
  setTxtProgressBar(clust_out$progress, i)
}
stopCluster(clust_out$cluster)

master_guide[!file.exists(model_path), ]

master_guide[file.exists(model_path), ]

