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
groundhog.day <- "2025-04-15"
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
  return(sum(mod1$sigma2) - sum(mod2$sigma2)) / sum(mod1$sigma2)

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
local_path <- "outputs/remote_cluster_randomization_mirror/"

dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")

master_guide <- fread("outputs/revision/summaries/model_comparison_table.csv")

# Note that this was run both with 'the best' random effects
# and with simple random effects (to prevent overfitting)

# master_guide <- master_guide[preferred_model == "yes"]
master_guide <- master_guide[random_effect == "list(~1 | Citation / data_point_ID)" &
                               effect_size == "smd" &
                               filter_big_CVs == "yes", ]
nrow(master_guide)
master_guide[duplicated(model_comparison_id), ]
# Must be 0 rows

master_guide <- master_guide[nativeness_var != "Africa_Comparison", ]
master_guide

dat <- dat[eff_type == "smd", ]
dat

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
  #'[This is computationally expensive. Code is for illustration purposes only]
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

remote_path <- "/Users/ejlundgren/GenomeDK/meta_megafauna/nov_2025_randomization"

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
# Set up remote directory -------------------------------------------------
if(!file.exists(remote_path)){
  
  # Guided based on previous version before submission:
  list.files("/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization/")
  
  dir.create(remote_path)
  dir.create(file.path(remote_path, "data"))
  dir.create(file.path(remote_path, "logs"))
  dir.create(file.path(remote_path, "models"))
  dir.create(file.path(remote_path, "outfiles"))
  dir.create(file.path(remote_path, "summaries"))
  
  
  list.files("/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization/data")
  saveRDS(dat, file.path(remote_path, "data", "master_data.Rds"))
  saveRDS(scaffold.mrg, file.path(remote_path, "data", "master_guide.Rds"))
  
  list.files("/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization/data/null_models/")
  dir.create(file.path(remote_path, "data", "null_models"))

  scaffold.mrg
  
  #
  nulls <- (unique(scaffold.mrg$model_path_null))
  names(nulls) <- basename((unique(scaffold.mrg$model_path_null)))
  
  # ms <- lapply(nulls,
  #              readRDS)
  # ms.sigma2 <- lapply(ms, function(x) min(x$sigma2)) |> unlist()
  # min(ms.sigma2)
  # scaffold.mrg[model_id_null == "model_107", ]
  # 
  # 
  file.remove(list.files(file.path(remote_path, "data", "null_models"), 
                         full.names = T))
  
  file.exists(nulls)
  file.copy(from = nulls,
            to = file.path(remote_path, "data", "null_models", names(nulls)))
  
  #
  all(file.exists(file.path(remote_path, "data", "null_models", names(nulls))))
  
  setdiff((list.files("/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization/")),
          (list.files(remote_path)))

}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Round 1 -----------------------------------------------------------------
# Remove models that have already run from the guide (if rerunning this due to time outs or insufficient GB/time, etc)
files <- list.files(file.path(remote_path, "models"))

files <- file.path("models", files)
scaffold.mrg[, model_path := paste0("models/", randomized_model_id, ".Rds")]

working_guide <- scaffold.mrg[!model_path %in% files, ]
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
          gb = 2,
          time = "1:00:00")

#' [submit array shell script in console with 'sbatch submit_array.sh']
# Record some job IDs:

# 11072445 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072444 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072443 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072442 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072441 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072440 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072439 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072438 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072437 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072436 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072435 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072434 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072433 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072432 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072431 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072430 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072429 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072428 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072427 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072426 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072425 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072424 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072423 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072422 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072421 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072420 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072419 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072418 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072417 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072416 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072415 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072414 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072413 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072412 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072411 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072410 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072409 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072408 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072407 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072406 short,nor randomiz ejlundgr PD       0:00      1 (Priority)
# 11072405 short,nor randomiz ejlundgr PD       0:00      1 (Priority)


#' [Can use 'jobinfo [JOB_ID]' to see if and how a job failed]
# 
#
# Check logs and outfiles for other errors:
logs <- list.files(paste0(remote_path, "/logs/"), full.names = T)
readLines(logs[5])

outs <- list.files(paste0(remote_path, "/outfiles/"), full.names = T)
readLines(outs[5])

# >>> Check success -----------------------------------------------------------------

files <- list.files(paste0(remote_path, "/summaries/"))
files <- gsub(".Rds", "", files)

head(files)
head(scaffold.mrg$randomized_model_id)

scaffold.mrg[randomized_model_id %in% files, ]

scaffold.mrg[!randomized_model_id %in% files, ]
#' *If 0 rows, then all the models ran.*

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Compile summaries and copy to local summary folder  ---------------------------------------

#' [sbatch the compile_results.sh script]

# Now copy the summaries to local mirror
files <- list.files(remote_path, full.names = T)
files

file.copy("/Users/ejlundgren/GenomeDK/meta_megafauna/nov_2025_randomization/final_summarized_model_results.Rds",
          "outputs/remote_cluster_randomization_mirror/summaries/final_summarized_model_results_simple_RE.Rds",
          overwrite = T)


