# Feb 2025
#
# 
# Compare model with nativeness to randomized nativeness
#
#
#
# HEADER ------------------------------------------------------------------

rm(list = ls())

library("metafor")
library("data.table")
library("multcomp")
library("dplyr")
library("broom")

tidy_with_CIs <- function(m){
  require("broom")
  require("dplyr")
  x <- tidy(m) %>%
    mutate(ci.lb = m$ci.lb, ci.ub = m$ci.ub)
  return(x)
}


I2 <- function(mod){
  
  W <- diag(1/mod$vi)
  X <- model.matrix(mod)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  return(100 * sum(mod$sigma2) / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P))))
  
}

prop_change_in_variance <- function(mod1, mod2){
  # https://stackoverflow.com/questions/22356450/getting-r-squared-from-a-mixed-effects-multilevel-model-in-metafor
  r2 <- (sum(mod1$sigma2) - sum(mod2$sigma2)) / 
    sum(mod1$sigma2)
  return(r2)
  
}
#library("sp")
# set.seed(42) if you set a seed then each script will do the same thing as the others...

# LOAD DATA -------------------------------------------------------------
# the root directory will be the folder that the analysis is run in
# setwd("/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization/")
# index <- 1
local <- FALSE
if(local){
  index <- 1
  setwd("/Users/ejlundgren/GenomeDK/meta_megafauna/feb_2025_randomization/")
}else{
  # get model number for this iteration
  args <- commandArgs()
  print(args)
  
  index <- as.numeric(args[6]) # get index value from bash script
}

print(paste("start at", Sys.time()))

# set seed by index so each node has its own random sequence..!!
set.seed(index)

dat <- readRDS("data/master_data.Rds")
guide <- readRDS("data/master_guide.Rds")

setDT(dat)
setDT(guide)

guide <- guide[chunk == index, ]

# sub.guide[!file.exists(paste0("data/null_models/", model_id_null, ".Rds"))]
#
#
# guide[!file.exists(paste0("data/null_models/", model_id_null, ".Rds"))]
# length(list.files("data/null_models")) == length(unique(guide$model_id_null))
# list.files("models/")

guide_i <- c()
intercept.m <- c()
sub.dat <- c()
var <- c()
template <- c()
Ns <- c()
tidy.m <- c()
stats <- c()

# >>> LOOP -------------------------------------------------------------
i <- 1
for(i in 1:nrow(guide)){
  
  guide_i <- guide[i, ]
  
  intercept.m <- readRDS(paste0("data/null_models/", guide_i$model_id_null, '.Rds'))
  
  sub.dat <- dat[eval(parse(text = guide_i$exclusion)), ]
  var <- guide_i$nativeness_var
  
  setnames(sub.dat, var, "nativeness_var")
  template <- unique(sub.dat[, .(Citation, nativeness_var)])
  Ns <- template[, .(refs = uniqueN(Citation)), by = nativeness_var]
  
  # setorder(Ns, nativeness_var)
  template[, rando_seq := sample(.N)]
  template[, fake_nativeness := ifelse(rando_seq <= Ns[nativeness_var == "Native", ]$refs,
                                       "Native", 
                                       Ns[nativeness_var != "Native", ]$nativeness_var)]
  
  sub.dat <- merge(sub.dat,
                   template[, .(Citation, fake_nativeness)],
                   by = "Citation")
  
  tryCatch(
    expr= {
      
      if(!file.exists(guide_i$model_path)){
        m <- rma.mv(yi = yi_smd,
                    V = vi_smd,
                    mods = ~ fake_nativeness,
                    random = eval(parse(text = guide_i$random_effect_formula)),
                    dfs = "contain",
                    test = "t",
                    method = "ML",
                    struct = "AR",
                    data = sub.dat)
        saveRDS(m, guide_i$model_path)
        
      }else{
        m <- readRDS(guide_i$model_path)
      }
 
    # Get number of nativeness from the template to make sure the operation worked...
    Ns <- template[, .(n = .N), by = .(fake_nativeness)]
    Ns[, str := paste(fake_nativeness, n)]
    
    #
    tidy.m <- tidy_with_CIs(m)
    stats <- data.table(model_id_nativeness = guide_i$model_id_nativeness,
                        iter = guide_i$iter,
                        nativeness_var = var,
                        # sample_size =  paste(Ns$str, collapse = "; "),
                        prop_variance_explained = prop_change_in_variance(mod1 = intercept.m,
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
    saveRDS(stats, paste0("summaries/", guide_i$model_id_nativeness, ".iter_", guide_i$iter, ".Rds"))
    
    if(nrow(stats) > 1) print("FUCK, more than 1 row of summary")
    
    if(!is.null(m)) print("MODEL SUCCESSFUL")
    
    },
    error = function(e) print("MODEL FAILED")
  )
 
  # reset the variables:
  guide_i <- c()
  intercept.m <- c()
  sub.dat <- c()
  var <- c()
  template <- c()
  Ns <- c()
  tidy.m <- c()
  stats <- c()
  
}
 
