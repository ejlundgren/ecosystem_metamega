#' *March 21 2025*
#' *Updated November 4th, 2025*
#
#
#' [Multilevel publication bias tests]
#
#
#
# Prepare workspace ---------------------------------------
#

rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "shades", "patchwork", "dplyr",
          "scico", "stringr", "gt",
          "plotly", "nlme", 
          "ggtext")#"ggh4x", "ggstance"
groundhog.library(libs, groundhog.day)
library("orchaRd")

# >>> Helper functions ----------------------------------------------------


tidy_with_CIs <- function(m){
  x <- tidy(m) %>%
    mutate(ci.lb = m$ci.lb, ci.ub = m$ci.ub)
  return(x)
}


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



rma_predictions <- function(m, newgrid,
                            has_intercept = T){
  
  if(!is.data.frame(newgrid)){errorCondition("ERROR newgrid must be a data frame")}
  #create the new model matrix. 
  
  if(!all(unlist(lapply(names(newgrid), # lapply through names of newgrid to check that they're in formula
                        function(x) grepl(pattern=x,
                                          x = as.character(m$formula.mods)[-1]))))){
    errorCondition("ERROR: variables in newgrid are not in model formula")
  }
  
  
  # Drop levels that might be missing from the model...
  cols <- names(newgrid)
  coef_nms <- names(coef(m))
  temp <- c()
  
  if(has_intercept == F){
    
    for(i in 1:length(cols)){
      if(class(unlist(newgrid[, cols[i], with = F])) %in% c("factor", "character")){
        temp <- paste0(names(newgrid[, cols[i], with = F]),
                       unlist(newgrid[, cols[i], with = F]))
        newgrid <- newgrid[temp %in% coef_nms, ]
      }
    }
    
  }
  
  newgrid
  
  # Create prediction matrix
  predgrid <- (model.matrix(m$formula.mods, data=newgrid))
  predgrid
  
  if(any(grepl("intercept", colnames(predgrid), 
               ignore.case = TRUE))){
    #if intercept is present, remove it?
    predgrid <- predgrid[, -1]
  }
  
  # predict onto the new model matrix
  pred.out <- as.data.frame(predict(m, newmods=predgrid))
  
  #attach predictions to variables for plotting
  final.pred <- cbind(newgrid, pred.out)
  
  return(final.pred)
}


# >>> Plotting constants --------------------------------------------------

tertiary_palette <- c("Introduced" = "#57b7db",
                      "Invasive" = "#F0C808", #green: "#60A561", yellow: "#F0C808"
                      "Native" = "#a7928c")


theme_lundy <-   theme_bw()+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(color = "black"),  #, family = "Liberation Sans")
        axis.text = element_text(color = "black"), #, family = "Liberation Sans")
        panel.grid = element_blank(),
        panel.border = element_blank())

# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# Load datasets and model guide -----------------------------------------------
dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")

master_guide <- fread("outputs/revision/summaries/model_comparison_table.csv")

unique(master_guide$preferred_model)

sub_guide <- master_guide[preferred_model == "yes", ]

unique(sub_guide$analysis_group)
nrow(sub_guide)

length(unique(sub_guide$analysis_group))
unique(sub_guide$analysis_group)

#
unique(sub_guide$nativeness_var)
unique(sub_guide$analysis_group_category)

sub_guide$analysis_group

# >>> Calculate effective N -----------------------------------------------

dat[, eff_N := (4*N_High_Megafauna*N_Low_Megafauna) / (N_High_Megafauna + N_Low_Megafauna)]
dat

# 1/sqrt(eff_N) for testing for publication bias. If slope is significant == BIAS
dat[, inv_sqrt_eff_N := 1/sqrt(eff_N)]

# If there is bias, use 1/eff_N. The intercept of this is an unbiased estimate of true effect (use alpha = 0.01)
dat[, inv_eff_N := 1/(eff_N)]


# >>> Create model guide ------------------------------------------------------

bias_guide <- sub_guide[,  .(model_id_null, model_path_null, 
                            nativeness_var, random_effect, exclusion,
                            analysis_group_category,
                            analysis_group)]


bias_guide[, `:=` (predictor1 = "inv_sqrt_eff_N", predictor2 = "inv_eff_N")]
bias_guide[duplicated(model_id_null), ]
# So we can use model_id_nativeness to compare these two predictors

bias_guide.mlt <- melt(bias_guide,
                        measure.vars = c("predictor1", "predictor2"),
                        value.name = "response")
bias_guide.mlt$variable <- NULL
bias_guide.mlt

bias_guide.mlt[, formula := paste("~", response)]
bias_guide.mlt

bias_guide.mlt[, model_id := paste0("bias_model_", 1:.N)]
bias_guide.mlt

bias_guide.mlt[, model_path := file.path("outputs/publication_bias/models", paste0(model_id, ".Rds"))]
bias_guide.mlt

saveRDS(bias_guide.mlt, "outputs/publication_bias/data/bias_guide.Rds")

guide <- copy(bias_guide.mlt)

guide[, .(n = .N), by = .(model_id_null, response)][n > 1, ]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------
# Run bias models ---------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------

#  Load guide and data -------------------------------------------------

clust_out <- prepare_cluster(n = nrow(guide))

guide
m <- c()
sub.dat <- c()
i <- 1

file.remove(list.files("outputs/publication_bias/models/", full.names = T))

# >>> foreach -------------------------------------------------------------

success <- foreach(i = 1:nrow(guide), 
                   .options.snow = clust_out$options,
                   .errorhandling = "pass",
                   .packages = c("data.table", "metafor")) %dopar% {
                     
                     if(!file.exists(guide[i, ]$model_path)){
                       sub.dat <- dat[eval(parse(text = guide[i, ]$exclusion)), ]
                       
                       #
                       m <- rma.mv(yi = yi,
                                   V = vi,
                                   mods = as.formula(guide[i, ]$formula),
                                   random = eval(parse(text = guide[i, ]$random_effect)),
                                   dfs = "contain",
                                   test = "t",
                                   method = "ML",
                                   struct = "AR",
                                   data = sub.dat)
                       
                       saveRDS(m, guide[i, ]$model_path)
                     }
                     
                     setTxtProgressBar(clust_out$progress, i)
                   }

stopCluster(clust_out$cluster)

# >>> Load models --------------------------------
guide[!file.exists(model_path)]

guide <- guide[file.exists(model_path), ]

ms <- lapply(guide$model_path, readRDS)

names(ms) <- guide$model_id

# >>> Tidy models ---------------------------------------------------

ms.tidy <- lapply(ms, tidy_with_CIs)

ms.tidy.dat <- rbindlist(ms.tidy, idcol = "model_id")

ms.tidy.dat

ms.tidy.dat.mrg <- merge(ms.tidy.dat,
                         guide[, .(model_id, analysis_group_category,
                                   nativeness_var,
                                   response, model_id_null, model_path_null,
                                   analysis_group, model_path)],
                         by = "model_id")

ms.tidy.dat.mrg[, type := ifelse(response == "inv_sqrt_eff_N", "Testing for bias", "Corrected estimate")]

ms.tidy.dat.mrg[term == "inv_sqrt_eff_N", .(.N), by = .(model_id_null)]
ms.tidy.dat.mrg[model_id_null == "model_457", ]

ms.tidy.dat.mrg[, bias := ifelse(.SD[term == "inv_sqrt_eff_N"]$p.value < 0.05, 
                                 "yes", "no"),
                by = .(model_id_null)]
ms.tidy.dat.mrg

ms.tidy.dat.mrg[p.value < 0.05 & term == "inv_sqrt_eff_N"]

# Whoa, that's surprising huh?

ms.tidy.dat.mrg

ms.tidy.dat.mrg[bias == "yes" & type == "Testing for bias" & term == "inv_sqrt_eff_N", ]
# OK, so Soil pH has publication bias. The only one that does. Surprising
# estimate = 7.6, p = 0.035

ms.tidy.dat.mrg[bias == "yes" & type == "Corrected estimate" & term == "intercept", ]
# estimate = -1.014, p=0.04. But alpha should 0.01 for this. So non-significant negative effect


# >>> Make a table --------------------------------------------------------

lvls <- c("Primary_Productivity",
          "Aboveground_Primary_Productivity",
          "Dead_Vegetation",
          "Litter_Cover", "Bare_Ground",
          "Soil_Compaction", "Soil_Moisture",
          "Soil_Temperature", 
          "Soil_Respiration", "CO2_Respiration",
          "Soil_Decomposition_Rate",
          "Root_Biomass",
          "Soil_Organic_Matter",
          "Soil_Organic_C", 
          "Soil_Total_C",
          "Soil_C:N", "Soil_Total_N", "Soil_Temperature",
          "Soil_Labile_N", "Soil_Total_P",
          "Soil_Total_Ca", "Soil_Total_Mg", "Soil_K",
          "Soil_pH", "Microbe_Abundance", "Fungi_Abundance",
          
          "Plant_C:N", "Plant_C", "Plant_N", 
          
          "Invertebrate_Diversity", "Invertebrate_Abundance",
          "Invert_Herbivore_Diversity", "Invert_Herbivore_Abundance",
          "Invert_Predator_Diversity", "Invert_Predator_Abundance",
          "Invert_Detritivore_Abundance",
          
          "Vertebrate_Diversity", "Vertebrate_Abundance",
          "Vert_Herb_Diversity", "Vert_Herb_Abundance",
          "Vert_Carn_Diversity", "Vert_Carn_Abundance",
          "Mammal_Abundance", "Mammal_Diversity",
          "Small_Mammal_Abundance", "Mamm_SmallHerb_Abundance",
          "TerrestrialBird_Abundance",
          "Bird_Diversity", "Bird_Abundance", "Bird_Carnivore_Abundance",
          "Bird_Omnivore_Abundance", "Herpetofauna_Abundance")
lvls <- unique(lvls)

# Let's report the test for bias (just the term) and the corrected estimate:
tab_1 <- ms.tidy.dat.mrg[term == "inv_sqrt_eff_N", ]
tab_2 <- ms.tidy.dat.mrg[term == "intercept" & type == "Corrected estimate", ]

tidy_table <- rbind(tab_1, tab_2)
tidy_table

tidy_table <- tidy_table[, .(analysis_group_category, analysis_group, nativeness_var, type, term, 
                             estimate, 
                             std.error, ci.lb, ci.ub, statistic, p.value, bias)]

# Load original estimates:
paths <- unique(ms.tidy.dat.mrg$model_path_null)

intercepts <- lapply(paths, readRDS)
intercepts <- lapply(intercepts, tidy_with_CIs)

length(intercepts) == length(paths)

names(intercepts) <- unique(ms.tidy.dat.mrg$model_id_null)

intercepts <- rbindlist(intercepts, idcol = "model_id_null")

intercepts <- merge(intercepts,
                    unique(ms.tidy.dat.mrg[, .(model_id_null, analysis_group, nativeness_var,
                                                  analysis_group_category)]),
                    by = "model_id_null")

intercepts

unique(intercepts$analysis_group)
#
setdiff(names(intercepts), names(tidy_table))
setdiff(names(tidy_table), names(intercepts))
intercepts[, type := "Uncorrected estimate"]

tidy_table[type == "Corrected estimate", bias := ""]
intercepts[, bias := ""]


tidy_table <- rbind(tidy_table, intercepts[, !c("model_id_null"), with = F])
# Bind:

tidy_table$analysis_group_category <- factor(tidy_table$analysis_group_category,
                                             levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

tidy_table$analysis_group <- factor(tidy_table$analysis_group,
                                             levels = lvls)
#
tidy_table[, analysis_group_label := gsub("_", " ", analysis_group)]
tidy_table[, nativeness_var_label := fcase(nativeness_var == "Herbivore_nativeness", "Nativeness",
                                           nativeness_var == "Invasive", "Invasiveness",
                                           nativeness_var == "Africa_Comparison", "Africa comparison")]

tidy_table[term == "inv_sqrt_eff_N", term := "1/sqrt effective N"]

#
gt_table <- tidy_table |>
  mutate(estimate = round(estimate, 2),
         std.error = round(std.error, 2),
         ci.lb = round(ci.lb, 2),
         ci.ub = round(ci.ub, 2),
         statistic = round(statistic, 2),
         p.value = round(p.value, 4)) %>%
  select(-term) %>%
  arrange(analysis_group_category, analysis_group, nativeness_var) |>
  group_by(analysis_group_label, nativeness_var_label) |>
  gt() |>
  cols_hide(columns = c("analysis_group_category", "nativeness_var",
                       "analysis_group")) |>
  cols_label(std.error = "SE",
             ci.lb = "lower CI",
             ci.ub = "upper CI",
             statistic = "t",
             p.value = "p") |>
  tab_header(md("**Table S5**. Multilevel publication bias tests revealed two 
                models (soil pH and vertebrate diversity in models with African megafauna versus 
                introduced megafauna) with significant evidence of publication bias. 
                Publication bias tests were based on (Nakagawa et al. 2022) and used 
                multilevel error structures to test whether effect sizes were a 
                function of the inverse of the square root of effective sample size. 
                We then used a similar model with 1/effective N to estimate bias-corrected estimates (reported in second row).
                We also report intercept-only model results to provide comparison (from main text models).")) |>
  opt_align_table_header(align = c("left")) 
  
gt_table

gtsave(gt_table, "figures/revision/supplement/Table SX Publication bias.pdf")



