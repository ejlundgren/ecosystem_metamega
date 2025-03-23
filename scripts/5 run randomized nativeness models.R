# March 21st, 2025
#
#
#' [Run randomized nativeness models]
#' *NOTE: This will be very time consuming on a personal computer. We ran these models*
#' *on a remote SLURM server. The code we used is in outputs/remote_cluster_randomization_mirror/*
#' *Below we present code that could be run locally (see section 1) if the users desired as well as the *
#' *code used to manage server runs (see section 2)*
#
# PREPARE workspace ------------------------------------------------------------------

rm(list = ls())

library("groundhog")
groundhog.day <- "2024-07-15"
libs <- c("data.table", "metafor", "data.table",
          "dplyr", "broom", "parallel", "doSNOW")
groundhog.library(libs, groundhog.day)
set.seed(33)

# >>> Helper functions --------------------------------------------
getVars <- function(formula_str){
  formula_str <- gsub("yi_smd ~ ", "", formula_str)
  formula_str <- gsub("- 1", "", formula_str)
  
  formula_str <- gsub("[*]", "+", formula_str)
  vars <- unlist(strsplit(formula_str, split = "[+]"))
  vars <- trimws(vars)
  vars
  return(vars)
}

prepare_cluster <- function(n){
  require("parallel")
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


prop_change_in_variance <- function(mod1, mod2){
  # https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor
  r2 <- (sum(mod1$sigma2) - sum(mod2$sigma2)) / 
    sum(mod1$sigma2)
  return(r2)
  
}

I2 <- function(mod){
  
  W <- diag(1/mod$vi)
  X <- model.matrix(mod)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  return(100 * sum(mod$sigma2) / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P))))
  
}

tidy_with_CIs <- function(m){
  require("broom")
  require("dplyr")
  x <- tidy(m) %>%
    mutate(ci.lb = m$ci.lb, ci.ub = m$ci.ub)
  return(x)
}
# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# 0. Load model guide & data------------------------------------------------------------------
local_path <- "outputs/local_randomized_models/"

dat <- fread("data/master_data.csv")

master_guide <- fread("outputs/main_text/summaries/model_comparison_table.csv")

master_guide <- master_guide[preferred_model == "yes"]
master_guide

master_guide <- master_guide[nativeness_var != "Africa_Comparison", ]
master_guide

# >>> Cross Join so 1 call per iteration ---------------------------------

scaffold <- CJ(iter = seq(1:1000),
               model_id_nativeness = unique(master_guide$model_id_nativeness))
scaffold.mrg <- merge(scaffold,
                      master_guide,
                      by = "model_id_nativeness")
scaffold.mrg

scaffold.mrg[, randomized_model_id := paste0(model_id_nativeness, ".iter_", iter)]
scaffold.mrg[, model_path := paste0(local_path, "models/", randomized_model_id, ".Rds")]

# Save locally
saveRDS(scaffold.mrg, paste0(local_path, "data/randomization_model_guide.Rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# 1. RUN LOCALLY -----------------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
run <- FALSE
if(run){
  #'[This is very computationally expensive. Code is for illustration purposes only]
  #'[If you wish to test run the foreach, change total number of iterations]
  
  sub.guide <- c() # the guide at i
  sub.dat <- c() # the subset of dat
  Ns <- c() # to store the number of native vs introduced or invasive citations
  var <- c() # the variable (nativeness or invasiveness) to randomize
  template <- c() # A template for assigning randomized values
  
  # Data structures for storing model fit statistics
  tidy.m <- c()
  stats <- c()
  intercept.m <- c()
  
  # We need to compare to intercept only models. Loading them inside of foreach is
  # computationally expensive. So load them here as a list:
  intercept_list <- lapply(unique(scaffold.mrg$model_path_null), 
                           readRDS)
  
  names(intercept_list) <- unique(scaffold.mrg$model_id_null)
  
  # Begin foreach -------------------------------------------------------------
 
  # remove models that have already run:
  working_guide <- scaffold.mrg[!file.exists(model_path)]
  working_guide
  
  #
  clust_out <- prepare_cluster(n=nrow(working_guide))
  
  i <- 1
  success <- foreach(i = 1:nrow(working_guide), #' *change nrow(working_guide) to a smaller number to test loop*
                     .options.snow = clust_out$options,
                     .errorhandling = "pass",
                     .packages = c("data.table", "metafor", "dplyr", "broom")) %dopar% {
                       
                       sub.guide <- working_guide[i, ]
                       sub.dat <- dat[eval(parse(text = sub.guide$exclusion)), ]
                       var <- sub.guide$nativeness_var
                       intercept.m <- intercept_list[[sub.guide$model_id_null]]
                       
                       setnames(sub.dat, var, "nativeness_var")
                       template <- unique(sub.dat[, .(Citation, nativeness_var)])
                       Ns <- template[, .(refs = uniqueN(Citation)), by = nativeness_var]
                       
                       template[, rando_seq := sample(.N)]
                       template[, fake_nativeness := ifelse(rando_seq <= Ns[1, ]$refs,
                                                            Ns[1, ]$nativeness, Ns[2, ]$nativeness_var)]
                       
                       sub.dat <- merge(sub.dat,
                                        template[, .(Citation, fake_nativeness)],
                                        by = "Citation")
                       
                       m <- rma.mv(yi = yi_smd,
                                   V = vi_smd,
                                   mods = ~ fake_nativeness,
                                   random = eval(parse(text = sub.guide$random_effect)),
                                   dfs = "contain",
                                   test = "t",
                                   method = "ML",
                                   struct = "AR",
                                   data = sub.dat)
                       saveRDS(m, sub.guide$model_path)
                       #
                       tidy.m <- tidy_with_CIs(m)
                       stats <- data.table(model_id_nativeness = sub.guide$model_id_nativeness,
                                           iter = sub.guide$iter,
                                           nativeness_var = var,
                                           sample_size =  paste(Ns$str, collapse = "; "),
                                           total_variance = prop_change_in_variance(mod1 = intercept.m,
                                                                                    mod2 = m),
                                           BIC = BIC(m),
                                           AIC = AIC(m),
                                           I2_total = I2(m),
                                           contrast = paste(names(coef(m)), collapse = "-"),
                                           contrast_estimate = tidy.m[2, ]$estimate,
                                           contrast_ci.lb = tidy.m[2, ]$ci.lb,
                                           contrast_ci.ub = tidy.m[2, ]$ci.ub,
                                           contrast_t_stat = tidy.m[2, ]$statistic,
                                           contrast_pval = tidy.m[2, ]$p.value)
                       saveRDS(stats, paste0(local_path, "/summaries/", sub.guide$randomized_model_id, ".Rds"))
                       
                       #
                       if(!is.null(stats)){
                         return("success")
                       }
                       
                       # Reset variables just to be extra safe:
                       sub.guide <- c() 
                       sub.dat <- c() 
                       Ns <- c() 
                       var <- c() 
                       template <- c() 
                       tidy.m <- c()
                       stats <- c()
                       intercept.m <- c()
                       
                       setTxtProgressBar(clust_out$progress, i)
                     }
  
  stopCluster(clust_out$cluster)
  unique(success)
  
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# 2. REMOTE SERVER SCRIPT ----------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------

remote_path <- "/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization"

dynoChunker <- function(dat, no_per_chunk = 50){
  #' *This function assigns a chunk variable to split datasets across server cores*
  suppressWarnings(dat$chunk <- NULL)
  chunks <- rep(seq(1:ceiling(nrow(dat)/no_per_chunk)), 
                no_per_chunk)
  chunks
  length(chunks) > nrow(dat)
  
  dat$chunk <- chunks[1:nrow(dat)]

  return(dat)
}

updateArray <- function(sh_path,
                        no_jobs){
  #' * This function updates the bash array script for server *
  if(!file.exists(sh_path)){ 
    print("File does not exist!") 
  }else{
    submit_array.script <- readLines(sh_path)
    original_formula <- submit_array.script[grepl("max_formulas=", submit_array.script)]
    forms <- paste0("max_formulas=",  no_jobs)
    submit_array.script.mod <- gsub(original_formula, forms, submit_array.script)
    submit_array.script.mod
    writeLines(submit_array.script.mod, sh_path)
    print("Job array updated")
  }
  
}

updateJob <- function(job_path,
                      gb,
                      time){
  #' * This function updates the bash job script for number of GB and time per core *
  if(is.na(as.numeric(gb))){
    print("ERROR: gb should be coercible to a numeric. E.g., '7'")
  }else if(sum(gregexpr(":", time, fixed=TRUE)[[1]] > 0) != 2){
    print("ERROR: time should be h:m:s format. E.g., '4:00:00'")
  }else if(!file.exists(job_path)){ 
    print("ERROR: File does not exist!") 
    
  }else{
    job.script <- readLines(job_path)
    
    # gb:
    original_slurm <- job.script[grepl("#SBATCH --mem-per-cpu=", job.script)]
    new_slurm <- paste0("#SBATCH --mem-per-cpu=",  gb, "gb")
    job.script.mod <- gsub(original_slurm, new_slurm, job.script)
    
    # time:
    original_slurm <- job.script.mod[grepl("#SBATCH --time ", job.script.mod)]
    new_slurm <- paste0("#SBATCH --time ",  time)
    job.script.mod2 <- gsub(original_slurm, new_slurm, job.script.mod)
    
    writeLines(job.script.mod2, job_path)
    print("Job script updated")
  }
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Round 1 -----------------------------------------------------------------
# Remove models that have already run from the guide (if rerunning this due to time outs or insufficient GB/time, etc)
files <- list.files(paste0(remote_path, "/models"))
files <- paste0("models/", files)

scaffold.mrg[, model_path := paste0("models/", randomized_model_id, ".Rds")]

working_guide <- scaffold.mrg[!model_path %in% files, ]
working_guide

# Copy intercept models to remote drive
rerun <- F
if(rerun){
  file.copy(unique(working_guide$model_path_null),
            paste0(remote_path, "/data/null_models/", unique(working_guide$model_id_null), ".Rds"))
}

# >>> Add chunk -----------------------------------------------------------
working_guide <- dynoChunker(dat=working_guide, no_per_chunk = 1000)

# Save to remote server:
saveRDS(working_guide, paste0(remote_path, "/data/master_guide.Rds"))
saveRDS(dat, paste0(remote_path, "/data/master_data.Rds"))

# >>> Update array and job ------------------------------------------------
updateArray(sh_path = paste0(remote_path, "/submit_array.sh"),
            no_jobs = max(working_guide$chunk))

# ask for 3GB and 4 hours per chunk
updateJob(job_path = paste0(remote_path, "/randomization_job.sh"),
          gb = 3,
          time = "4:00:00")

#' [submit array shell script in console with 'sbatch submit_array.sh']

# Record some job IDs:
# 55768922     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768923     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768924     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768925     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768926     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768927     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768928     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768929     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768930     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768931     short randomiz ejlundgr  R       0:19      1 s21n34
# 55768932     short randomiz ejlundgr  R       0:19      1 s21n64
# 55768933     short randomiz ejlundgr  R       0:19      1 s21n64
# 55768934     short randomiz ejlundgr  R       0:19      1 s21n64
# 55768935     short randomiz ejlundgr  R       0:19      1 s21n64
# 55768936     short randomiz ejlundgr  R       0:19      1 s21n64
# 55768937     short randomiz ejlundgr  R       0:19      1 s21n71
# 55768938     short randomiz ejlundgr  R       0:19      1 s21n71
# 55768939     short randomiz ejlundgr  R       0:19      1 s21n71
# 55768940     short randomiz ejlundgr  R       0:19      1 s21n71

#' [Can use 'jobinfo 55768741' to see if job failed from going over time or over memory ]
#' 
# Check logs and outfiles for other errors:
logs <- list.files(paste0(remote_path, "/logs/"), full.names = T)
readLines(logs[5])

outs <- list.files(paste0(remote_path, "/outfiles/"), full.names = T)
readLines(outs[5])

# >>> Check success -----------------------------------------------------------------

files <- list.files(paste0(remote_path, "/summaries/"))
files <- gsub(".Rds", "", files)

scaffold.mrg[randomized_model_id %in% files, ]

scaffold.mrg[!randomized_model_id %in% files, ]
#' *If 0 rows, then all the models ran.*

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Round 2 -----------------------------------------------------------------
# Remove models that have already run from the guide (if rerunning this due to time outs or insufficient GB/time, etc)

working_guide <- scaffold.mrg[!randomized_model_id %in% files, ]
working_guide

# >>> Add chunk -----------------------------------------------------------
working_guide <- dynoChunker(dat=working_guide, no_per_chunk = 500)

# Save to remote server:
saveRDS(working_guide, paste0(remote_path, "/data/master_guide.Rds"))
saveRDS(dat, paste0(remote_path, "/data/master_data.Rds"))

# >>> Update array and job ------------------------------------------------
updateArray(sh_path = paste0(remote_path, "/submit_array.sh"),
            no_jobs = max(working_guide$chunk))

# ask for 3GB and 4 hours per chunk
updateJob(job_path = paste0(remote_path, "/randomization_job.sh"),
          gb = 3,
          time = "4:00:00")

#' [submit array shell script in console with 'sbatch submit_array.sh']

# Record some job IDs:
# 55769013 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769012 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769011 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769010 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769009 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769008 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769007 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769006 short,nor randomiz ejlundgr PD       0:00      1 (None)
# 55769005 short,nor randomiz ejlundgr PD       0:00      1 (None)

#' [Can use 'jobinfo 55768741' to see if job failed from going over time or over memory ]
#' 
# Check logs and outfiles for other errors:
logs <- list.files(paste0(remote_path, "/logs/"), full.names = T)
readLines(logs[5])

outs <- list.files(paste0(remote_path, "/outfiles/"), full.names = T)
readLines(outs[5])

# >>> Check success -----------------------------------------------------------------

files <- list.files(paste0(remote_path, "/summaries/"))
files <- gsub(".Rds", "", files)

scaffold.mrg[randomized_model_id %in% files, ]

scaffold.mrg[!randomized_model_id %in% files, ]
#' *If 0 rows, then all the models ran.*

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Compile summaries and copy to local summary folder  ---------------------------------------

#' [sbatch the compile_results.sh script]

# Now copy the summaries to local mirror
files <- list.files(remote_path, full.names = T)
files

file.copy("/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization/final_summarized_model_results.Rds",
          "outputs/remote_cluster_randomization_mirror/summaries/final_summarized_model_results.Rds",
          overwrite = T)

