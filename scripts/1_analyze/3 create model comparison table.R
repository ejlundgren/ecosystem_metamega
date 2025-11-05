#' [March 19th, 2025]
#
#
#' [Create model comparison table]
#' *AIMS:*
# 1. Compare models by BIC to find model with best random effect structure.
# 2. Extract posthoc comparisons
# 3. Compare intercept-only models ('null' models) to models with nativeness
#
#

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# PREPARE WORKSPACE ------------------------------------------------------------------

rm(list = ls())
gc()

library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("data.table", "foreach", "parallel",
          "doSNOW", "metafor", "multcomp",
          "broom", "dplyr",
          "car")
groundhog.library(libs, groundhog.day)

# >>> Helper functions ----------------------------------------------------

# We'll use this function in a couple places:
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

I2 <- function(mod){
  
  W <- diag(1/mod$vi)
  X <- model.matrix(mod)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  return(100 * sum(mod$sigma2) / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P))))
  
}

prop_change_in_variance <- function(mod1, mod2){
  # https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor
  # I was surprised to see this as a difference between 2 models....
  r2 <- (sum(mod1$sigma2) - sum(mod2$sigma2)) / 
    sum(mod1$sigma2)
  return(r2)
  
}

tidy_with_CIs <- function(m){
  x <- tidy(m) %>%
    mutate(ci.lb = m$ci.lb, ci.ub = m$ci.ub)
  return(x)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------------
# 0. Load data ------------------------------------------------------------

dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")
guide <- readRDS("outputs/revision/data/master_guide.Rds")

guide[!file.exists(model_path), ]
guide[file.exists(model_path), ]

length(list.files("outputs/revision/models/"))

guide <- guide[file.exists(model_path), ]

# test:
m1 <- readRDS(guide[400, ]$model_path)
m1

I2(m1)

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# 1. Extract model level information ----------------------------------------------
# Extract I2, BIC, min_sigma (to flag overfitting)

guide

i <- 1
m <- c()

clust_out <- prepare_cluster(n = nrow(guide))

model_info <- foreach(i = 1:nrow(guide), 
                      .options.snow = clust_out$options,
                      .errorhandling = "pass",
                      .packages = c("data.table", "metafor")) %dopar% {
                        
    m <- readRDS(guide[i, ]$model_path)                      
    m                       
    
    return(data.table(model_id = guide[i, ]$model_id,
                      i2 = I2(m),
                      BIC = BIC(m),
                      min_sigma = min(m$sigma2)))    
    setTxtProgressBar(clust_out$progress, i)
    
}

stopCluster(clust_out$cluster)
model_info <- rbindlist(model_info)

# Merge into guide
guide.mrg <- merge(guide,
                   model_info,
                   by = "model_id",
                   all.x = T,
                   all.y = T)

guide.mrg[is.na(i2), ]
guide.mrg[is.na(BIC), ]
#' *Hopefully 0 rows*

guide.mrg

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# 2. Cast guide wide and choose best random effects by BIC -----------------------------
unique(guide.mrg$model_type)

guide.wide <- dcast(guide.mrg,
                    ... ~ model_type,
                    value.var = c("formula", "model_path", "i2", "BIC", "min_sigma", "model_id"))
guide.wide[is.na(formula_nativeness), ]

# These are the models that didn't run
guide.wide <- guide.wide[!is.na(formula_nativeness), ]
guide.wide <- guide.wide[!is.na(formula_null), ]

guide.wide

# >>> Check for overfit models, if min sigma is 0 -------------------------------------------

guide.wide[, overfit := ifelse(min_sigma_nativeness == 0 | min_sigma_null == 0,
                               "yes", "no")]
guide.wide[overfit == "yes", ]

# >>> Random effect comparison ----------------------------------------------------
# Do for nativeness models, not intercept-only null models. And exclude overfitted models.

guide.wide

#
guide.wide[, min_BIC := min(.SD[overfit == "no"]$BIC_nativeness),
           by = .(model_comparison_id)]

guide.wide[, delta_BIC := BIC_nativeness - min_BIC]
guide.wide[, preferred_model := ifelse(overfit == "no" & delta_BIC == 0,
                                       "yes", "no")]

guide.wide[, .(any_yes = any(preferred_model == "yes")),
           by = .(model_comparison_id)][any_yes == FALSE]
#' *Must be 0 rows*
#
guide.wide

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# 3. Extract posthoc comparisons ------------------------------------------
#
guide.wide

m <- c()
out <- list()
i <- 1

coef <- c()

clust <- prepare_cluster(n = nrow(guide.wide))

posthoc_comp_out <- foreach(i = 1:nrow(guide.wide), 
                            .packages = c("metafor", "data.table", "dplyr",
                                          "tidyr", "broom", "multcomp"),
                            .options.snow = clust$options,
                            .errorhandling = "pass") %dopar% {
                              
        # Load model
        m <- readRDS(guide.wide[i, ]$model_path_nativeness)
        #
        dfs <- m$ddf
        
        #
        out <- m %>%
          tidy_with_CIs() %>%
          filter(term != "intercept") %>%
          mutate(intercept_df = dfs["intrcpt"],
                 contrast_df = dfs[!names(dfs) %in% "intrcpt"],
                 model_id_nativeness = guide.wide[i, ]$model_id_nativeness,
                 model_path_nativeness = guide.wide[i, ]$model_path_nativeness,
                 preferred_model = guide.wide[i, ]$preferred_model,
                 nativeness_var = guide.wide[i, ]$nativeness_var,
                 contrast = paste0("intercept-", term),
                 effect_size = guide.wide[i, ]$effect_size,
                 filter_big_CVs = guide.wide[i, ]$filter_big_CVs,
                 analysis_group = guide.wide[i, ]$analysis_group,
                 analysis_group_category = guide.wide[i, ]$analysis_group_category,
                 min_refs = guide.wide[i, ]$min_refs,
                 max_refs = guide.wide[i, ]$max_refs)
        
      setDT(out)
      return(out)
        
}

stopCluster(clust_out$cluster)

posthoc_final <- rbindlist(posthoc_comp_out)

unique(posthoc_final$contrast)
unique(posthoc_final$term)
unique(guide.wide$nativeness_var)

posthoc_final[preferred_model == "yes" & p.value < 0.05, ]

posthoc_final

posthoc_final[intercept_df != contrast_df, ]

# >>> Flip signs of estimates ---------------------------------------------
#' *Let's make it so positive values mean a more positive effect of introduced/invasive megafauna*
# And clarify contrast string while we're at it
unique(posthoc_final$contrast)

# This one does not flip:
posthoc_final[contrast == "intercept-Africa_ComparisonIntroduced", 
              `:=` (contrast = "Introduced-Intact_Africa",
                     estimate = estimate,
                     ci.lb = ci.lb,
                     ci.ub = ci.ub)]

# These flip:
posthoc_final[contrast == "intercept-InvasiveNative", 
              `:=` (contrast = "Invasive-Native",
                     estimate = -estimate,
                     ci.lb = -ci.lb,
                     ci.ub = -ci.ub)]

posthoc_final[contrast == "intercept-Herbivore_nativenessNative", 
              `:=` (contrast = "Introduced-Native",
                    estimate = -estimate,
                    ci.lb = -ci.lb,
                    ci.ub = -ci.ub)]

posthoc_final

fwrite(posthoc_final, "outputs/revision/summaries/posthoc_comparisons.csv",
       na = "NA")

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# 4. Does nativeness or invasiveness improve model quality?  -------------------------------------------------

guide.wide
#
m.null <- c()
m.native <- c()
model_comp_out <- list()
out <- c()

clust_out <- prepare_cluster(n = nrow(guide.wide))
i <- 1

model_comp_out <- foreach(i = 1:nrow(guide.wide), 
                       .options.snow = clust_out$options,
                       .errorhandling = "remove",
                       .packages = c("data.table", "metafor")) %dopar% {
                         
     m.null <- readRDS(guide.wide[i, ]$model_path_null)
     m.native <- readRDS(guide.wide[i, ]$model_path_nativeness)
     
     temp <- anova(m.null, m.native)
     
     out <- data.table(model_id_nativeness = guide.wide[i, ]$model_id_nativeness,
                       LRT = temp$LRT,
                       LRT_pval = temp$pval,
                       prop_variance_reduced = prop_change_in_variance(mod1 = m.null,
                                                                        mod2 = m.native))
     
     setTxtProgressBar(clust_out$progress, i)
     
     return(out)    
                         
 }

stopCluster(clust_out$cluster)

model_comp_out.df <- rbindlist(model_comp_out, fill = T)
 
guide.wide.mrg <- merge(guide.wide,
                        model_comp_out.df,
                        by = "model_id_nativeness",
                        all.x = T)
  
guide.wide.mrg

# ~~~~~~~~~~~~~~~~~~~~ ----------------------------------------------------
# 5. Save final model comparison table -----------------------------------------

fwrite(guide.wide.mrg, "outputs/revision/summaries/model_comparison_table.csv",
       na = "NA")


