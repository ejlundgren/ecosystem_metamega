#' *March 22nd, 2025*
#
#
#' [Influence analysis with Cook's distance]
#
#
#
# Prepare workspace ---------------------------------------
#
rm(list = ls())
gc()

library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
           "dplyr",
          "stringr",
          "foreach",
          "parallel", "doSNOW")#"ggh4x", "ggstance"
groundhog.library(libs, groundhog.day)


# >>> Helper functions ----------------------------------------------------


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


# This calls rma_predictions for a series of models, just to expedite plotting.
load_and_tidy_model_series <- function(model_paths,
                                       var = "Herbivore_nativeness",
                                       has_intercept = T){
  library("broom")
  library("metafor")
  
  mods <- lapply(model_paths, readRDS)
  
  #
  test <- lapply(mods, coef) |> lapply(length)
  unique(test)
  
  # unfortunately tidy doesnt give us the predictions we want...which is a real shame
  newgrid <- list(Herbivore_nativeness = c("Native", "Introduced"),
                  Invasive = c("Invasive", "Native"),
                  Africa_Comparison = c("Intact_Africa", "Introduced"))
  
  if(!var %in% names(newgrid)){
    warning("specified var is not in grid options")
    break()
  }
  
  newgrid <- data.table(var = newgrid[[var]])
  setnames(newgrid, "var", var)
  
  #
  preds <- lapply(mods, rma_predictions, newgrid, has_intercept)
  
  return(preds)
  
}
tidy_with_CIs <- function(m){
  x <- tidy(m) %>%
    mutate(ci.lb = m$ci.lb, ci.ub = m$ci.ub)
  return(x)
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

# *** Set plotting constants ----------------------------------------------

quaternary_palette <- c("Introduced" = "#57b7db", "African assemblage" = "#d7a4a3",  
                        "Invasive" = "#faae6b", "Native" = "#a7928c")

theme_lundy <-   theme_bw()+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(color = "black"),  
        axis.text = element_text(color = "black"), 
        panel.grid = element_blank())

# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# Load datasets and model guide -----------------------------------------------

master_dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")
master_guide <- fread("outputs/revision/summaries/model_comparison_table.csv")
master_guide

master_guide <- master_guide[preferred_model == "yes", ]
nrow(master_guide)

master_guide[, obs_level_three_mean_path := paste0("outputs/influence_analysis/revision/models/",
                                         model_id_nativeness, 
                                         "__obs_lvl_3_mean_exclusion.Rds")]

master_guide[, obs_level_four_n_path := paste0("outputs/influence_analysis//revision/models/",
                                     model_id_nativeness, 
                                     "__obs_lvl_4_N_exclusion.Rds")]


master_guide[, study_level_three_mean_path := paste0("outputs/influence_analysis/revision/models/",
                                                   model_id_nativeness, 
                                                   "__study_lvl_3_mean_exclusion.Rds")]

master_guide[, study_level_four_n_path := paste0("outputs/influence_analysis//revision/models/",
                                               model_id_nativeness, 
                                               "__study_lvl_4_N_exclusion.Rds")]
file.remove(list.files("outputs/influence_analysis/revision/models/", full.names = T))

master_guide[, outlier_dat_path := paste0("outputs/influence_analysis/revision/cook_distance/",
                                                 model_id_nativeness, 
                                                 ".Rds")]

master_guide

# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------

# Calculate Cook's distance and rerun models ------------------------------

#' [Unlike in initial submission, I think we should do this by data point not Citation.]

rerun <- F
if(rerun){
  file.remove(list.files("outputs/influence_analysis/revision/cook_distance/", full.names = T))
  file.remove(list.files("outputs/influence_analysis/revision/summaries", full.names = T))
  file.remove(list.files("outputs/influence_analysis/revision/models", full.names = T))
  
  
  # >>> Encapsulate ---------------------------------------------------------
  citation_outliers <- function(model_path){
    
    m <- model_path |>
      readRDS()
    m
    dat <- m$data
    
    dat$obs_level_cook_dist <- cooks.distance(m)
    
    dat[, obs_level_threshold_3meancook := ifelse(obs_level_cook_dist > (3 * mean(obs_level_cook_dist)),
                                         "exclude", "keep")]

    dat[, obs_level_threshold_4.N := ifelse(obs_level_cook_dist > (4 / .N),
                                        "exclude", "keep")]
    
    
    # ----------- BY CITATION --------------------!
    cits <- unique(dat[, .(Citation)])

    cits$study_level_cook_dist <- cooks.distance(m,
                                     cluster = Citation)

    cits[, study_level_threshold_3meancook := ifelse(study_level_cook_dist > (3 * mean(study_level_cook_dist)),
                                         "exclude", "keep")]

    cits[, study_level_threshold_4.N := ifelse(study_level_cook_dist > (4 / .N),
                                        "exclude", "keep")]
    # In this case these are identical.
    dat.mrg <- merge(dat,
                     cits[, .(Citation, study_level_cook_dist, study_level_threshold_3meancook, study_level_threshold_4.N)],
                     by = "Citation",
                     all.x = T)
    
    return(dat.mrg)
    
  }

  # >>> Run locally -------------------------------------------------------------
  #' [This will be time consuming]

  clust_out <- prepare_cluster(n=nrow(master_guide))
  
  i <- 1
  
  success <- foreach(i = 1:nrow(master_guide), 
                      .options.snow = clust_out$opts,
                      .errorhandling = "pass",
                      .packages = c("data.table", "metafor", 
                                    "dplyr", "broom")) %dopar% {
      
    if(!file.exists(master_guide[i, ]$outlier_dat_path) ){
      
      dat <- citation_outliers(master_guide[i, ]$model_path_nativeness)
      
      saveRDS(dat, master_guide[i, ]$outlier_dat_path)
               
    }
    setTxtProgressBar(clust_out$progress, i)
                                      
  }
  stopCluster(clust_out$cluster)

}

# master_guide
master_guide[!file.exists(outlier_dat_path), ]
# dat <- citation_outliers(master_guide[!file.exists(outlier_dat_path), ]$model_path_nativeness)
# 
# saveRDS(dat, master_guide[!file.exists(outlier_dat_path), ]$outlier_dat_path)
# 

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------

# Run Models with excluded data points ------------------------------------
rerun <- F
if(rerun){
  
  clust_out <- prepare_cluster(n=nrow(master_guide))
  
  i <- 1
  
  success <- foreach(i = 1:nrow(master_guide), 
                     .options.snow = clust_out$opts,
                     .errorhandling = "pass",
                     .packages = c("data.table", "metafor", 
                                   "dplyr", "broom")) %dopar% {
    
                                     
     dat <- master_guide[i, ]$outlier_dat_path |> readRDS()

     # 3 mean study-level
     if(!file.exists(master_guide[i, ]$study_level_three_mean_path) ){
       sub.dat <- dat[study_level_threshold_3meancook == "keep"]
       
       m <- rma.mv(yi = yi,
                   V = vi,
                   mods = as.formula(gsub("yi_smd", "", master_guide[i, ]$formula_nativeness)),
                   random = eval(parse(text = master_guide[i, ]$random_effect)),
                   dfs = "contain",
                   test = "t",
                   method = "ML",
                   struct = "AR",
                   data = sub.dat)
       
       saveRDS(m, master_guide[i, ]$study_level_three_mean_path)
       
     }
     # 3 mean observation level
     if(!file.exists(master_guide[i, ]$obs_level_three_mean_path) ){
       sub.dat <- dat[obs_level_threshold_3meancook == "keep"]
       
       m <- rma.mv(yi = yi,
                   V = vi,
                   mods = as.formula(gsub("yi_smd", "", master_guide[i, ]$formula_nativeness)),
                   random = eval(parse(text = master_guide[i, ]$random_effect)),
                   dfs = "contain",
                   test = "t",
                   method = "ML",
                   struct = "AR",
                   data = sub.dat)
       
       saveRDS(m, master_guide[i, ]$obs_level_three_mean_path)
       
     }
                                     
     # 4/N study level
     if(!file.exists(master_guide[i, ]$study_level_four_n_path) ){
       sub.dat <- dat[study_level_threshold_4.N == "keep"]
       
       m <- rma.mv(yi = yi,
                   V = vi,
                   mods = as.formula(gsub("yi_smd", "", master_guide[i, ]$formula_nativeness)),
                   random = eval(parse(text = master_guide[i, ]$random_effect)),
                   dfs = "contain",
                   test = "t",
                   method = "ML",
                   struct = "AR",
                   data = sub.dat)
       
       saveRDS(m, master_guide[i, ]$study_level_four_n_path)
       
     }       
     
     # 4/N obs level
     if(!file.exists(master_guide[i, ]$obs_level_four_n_path) ){
       sub.dat <- dat[obs_level_threshold_4.N == "keep"]
       
       m <- rma.mv(yi = yi,
                   V = vi,
                   mods = as.formula(gsub("yi_smd", "", master_guide[i, ]$formula_nativeness)),
                   random = eval(parse(text = master_guide[i, ]$random_effect)),
                   dfs = "contain",
                   test = "t",
                   method = "ML",
                   struct = "AR",
                   data = sub.dat)
       
       saveRDS(m, master_guide[i, ]$obs_level_four_n_path)
       
     }                 
     setTxtProgressBar(clust_out$progress, i)
                                     
   }
  stopCluster(clust_out$cluster)
  

}


# ~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------------
# Load and do posthoc tests on outlier removed models  -------------------------------------------------


rerun <- F
if(rerun){
  
  # Make dataset long for this
  outlier_guide <- melt(master_guide,
                        measure.vars = c("obs_level_three_mean_path", "obs_level_four_n_path",
                                         "study_level_three_mean_path", "study_level_four_n_path"),
                        variable.name = "outlier_threshold",
                        value.name = "model_path")
  outlier_guide[!file.exists(model_path), ]
  
  outlier_guide <- outlier_guide[file.exists(model_path), ]
  
  saveRDS(outlier_guide, "outputs/influence_analysis/revision/summaries/outlier_model_guide.Rds")
  

  outlier_guide
  i <- 1#
  clust <- prepare_cluster(n = nrow(outlier_guide))
  
  posthoc_comp_out <- foreach(i = 1:nrow(outlier_guide), 
                              .packages = c("metafor", "data.table", "dplyr",
                                            "tidyr", "broom", "multcomp"),
                              .options.snow = clust$options,
                              .errorhandling = "pass") %dopar% {
                                
                                # Load model
                                m <- readRDS(outlier_guide[i, ]$model_path)
                                dfs <- m$ddf

                                out <- m %>%
                                  tidy_with_CIs() %>%
                                  filter(term != "intercept") %>%
                                  mutate(intercept_df = dfs["intrcpt"],
                                         contrast_df = dfs[!names(dfs) %in% "intrcpt"],
                                         model_id_nativeness = outlier_guide[i, ]$model_id_nativeness,
                                         model_path_nativeness = outlier_guide[i, ]$model_path_nativeness,
                                         outlier_threshold = outlier_guide[i, ]$outlier_threshold,
                                         outlier_path = outlier_guide[i, ]$model_path,
                                         preferred_model = outlier_guide[i, ]$preferred_model,
                                         nativeness_var = outlier_guide[i, ]$nativeness_var,
                                         contrast = paste0("intercept-", term),
                                         analysis_group = outlier_guide[i, ]$analysis_group,
                                         analysis_group_category = outlier_guide[i, ]$analysis_group_category,
                                         min_refs = outlier_guide[i, ]$min_refs,
                                         max_refs = outlier_guide[i, ]$max_refs,
                                         total_studies = length(unique(m$data$Citation)))
                                
                                setDT(out)
                                return(out)
                                
                              }
  stopCluster(clust$cluster)
  
  posthoc_comp_out
  
  posthoc_comp_out <- rbindlist(posthoc_comp_out)
  
  # This one does not flip:
  posthoc_comp_out[contrast == "intercept-Africa_ComparisonIntroduced", ]
  posthoc_comp_out[contrast == "intercept-Africa_ComparisonIntroduced", 
                `:=` (contrast = "Introduced-Intact_Africa",
                      estimate = estimate,
                      ci.lb = ci.lb,
                      ci.ub = ci.ub)]
  
  # These flip:
  posthoc_comp_out[contrast == "intercept-InvasiveNative", ]
  posthoc_comp_out[contrast == "intercept-InvasiveNative", 
                `:=` (contrast = "Invasive-Native",
                      estimate = -estimate,
                      ci.lb = -ci.lb,
                      ci.ub = -ci.ub)]
  
  posthoc_comp_out[contrast == "intercept-Herbivore_nativenessNative", ]
  posthoc_comp_out[contrast == "intercept-Herbivore_nativenessNative", 
                `:=` (contrast = "Introduced-Native",
                      estimate = -estimate,
                      ci.lb = -ci.lb,
                      ci.ub = -ci.ub)]
  
  posthoc_comp_out
  
  saveRDS(posthoc_comp_out, "outputs/influence_analysis/revision/summaries/outlier_posthoc_comparisons_t_test.Rds")
  
  posthocs <- copy(posthoc_comp_out)
  
}else{
  posthocs <- readRDS("outputs/influence_analysis/revision/summaries/outlier_posthoc_comparisons_t_test.Rds")
}

posthocs[analysis_group_category %in% c("Soil", "Microbes"), analysis_group_category := "Ecosystem"]

# ~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------------
# Plot new models ----------------------------------------------------
#' [Only plotting the posthoc differences, not the overall results]
# >>> Interpret results ---------------------------------------------------

posthocs <- posthocs[preferred_model == "yes"]

posthocs[p.value < 0.05 & analysis_group_category %in% c("Vertebrates", "Invertebrates") &
           nativeness_var != "Africa_Comparison", 
         .(analysis_group, outlier_threshold, contrast, estimate, 
            statistic, contrast_df, ci.lb, ci.ub, p.value)]

range(posthocs[p.value < 0.05 & analysis_group_category %in% c("Vertebrates", "Invertebrates") &
           nativeness_var != "Africa_Comparison", 
         .(analysis_group, outlier_threshold, contrast, estimate, 
           statistic, ci.lb, ci.ub, p.value)]$statistic)
range(posthocs[p.value < 0.05 & analysis_group_category %in% c("Vertebrates", "Invertebrates") &
                 nativeness_var != "Africa_Comparison", 
               .(analysis_group, outlier_threshold,contrast_df, contrast, estimate, 
                 statistic, ci.lb, ci.ub, p.value)]$contrast_df)
range(posthocs[p.value < 0.05 & analysis_group_category %in% c("Vertebrates", "Invertebrates") &
                 nativeness_var != "Africa_Comparison", 
               .(analysis_group, outlier_threshold, contrast, estimate, 
                 statistic, ci.lb, ci.ub, p.value)]$p.value)

posthocs[p.value < 0.05 & analysis_group_category == "Ecosystem" &
           nativeness_var != "Africa_Comparison", 
         .(analysis_group, outlier_threshold, contrast, contrast_df, estimate, 
           statistic, ci.lb, ci.ub, p.value)]


posthocs[p.value < 0.05 & #analysis_group_category == "Ecosystem" &
           nativeness_var == "Africa_Comparison", 
         .(analysis_group, outlier_threshold, contrast, estimate, 
           statistic, contrast_df, ci.lb, ci.ub, p.value)]


posthocs[p.value < 0.05 & #analysis_group_category == "Ecosystem" &
           nativeness_var == "Africa_Comparison", .(min_stat = min(statistic),
                                                    max_stat = max(statistic),
                                                    min_p = min(p.value),
                                                    max_p = max(p.value),
                                                    min_df = min(contrast_df),
                                                    max_df = max(contrast_df)),
         by = .(analysis_group)]


# >>> Prepare to plot -----------------------------------------------------

posthocs[, p.value.rounded := as.character(round(p.value, 2))]
posthocs[p.value.rounded == 0, ]$p.value
posthocs[p.value.rounded == 0, p.value.rounded := "<0.001"]

unique(posthocs$contrast)
posthocs$contrast <- factor(posthocs$contrast,
                            levels = rev(c("Introduced-Native",
                                           "Invasive-Native",
                                           "Introduced-Intact_Africa")))


lvls <- c("Growth_Rates",
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
          "Soil_Total_Ca", "Soil_Mg", "Soil_K",
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
setdiff(posthocs$analysis_group, lvls)

posthocs$analysis_group <- factor(posthocs$analysis_group,
                                  rev(lvls))

posthocs$analysis_group <- droplevels(posthocs$analysis_group)


group_labels <- unique(posthocs$analysis_group)
group_labels <- gsub("_", " ", group_labels)
group_labels <- gsub("Invert ", "", group_labels)
names(group_labels) <- unique(posthocs$analysis_group)
group_labels <- gsub("Vert Carn ", "Carnivore ", group_labels)
group_labels <- gsub("Vert Herb ", "Herbivore ", group_labels)

group_labels[group_labels == "Growth Rates"] <- "Plant Growth Rates"


posthocs[, outlier_scale := ifelse(grepl("study_level", outlier_threshold), "Study level", "Observation level")]
posthocs[, outlier_type := ifelse(grepl("three_mean", outlier_threshold), 
                                       "Excluding Cook's d > 3 * mean(Cook's d)", 
                                       "Excluding Cook's d > 4 / N")]

# >>> Figures -------------------------------------------------------------
unique(posthocs$analysis_group_category)

unique(posthocs$outlier_threshold)

p.post.vert <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  # annotate(geom = "label", x = -1.5, y = Inf, label = "More negative",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  # annotate(geom = "label", x = 1.5, y = Inf, label = "More positive",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  geom_errorbar(data = posthocs[analysis_group_category == "Vertebrates" &
                                  nativeness_var != "Africa_Comparison"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75))+
  geom_point(data = posthocs[analysis_group_category == "Vertebrates" &
                               nativeness_var != "Africa_Comparison"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[analysis_group_category == "Vertebrates" &
                              nativeness_var != "Africa_Comparison"], 
            aes(x = 1.5, y = analysis_group,
                color = contrast, 
                label = paste0("p=", p.value.rounded)),
            vjust = -1,
            position = position_dodgev(height = .75),
            size = 2.5)+
  scale_fill_manual("Contrast",
                    values = c("Introduced-Native" = "#57b7db",
                               "Invasive-Native" = "#faae6b"),
                    guide = guide_legend(reverse = TRUE))+
  scale_color_manual(values = c("Introduced-Native" = "#57b7db",
                                "Invasive-Native" = "#faae6b"))+
  guides(color = "none")+
  facet_grid(outlier_type~outlier_scale)+
  scale_y_discrete(labels = group_labels)+
  ylab(NULL)+
  xlab("Difference relative to native megafauna\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-3, 3), clip = "off")+
  theme_lundy
p.post.vert

ggsave("figures/revision/supplement/Vertebrates influential studies excluded.pdf", 
       width = 10, height = 8)
ggsave("figures/revision/supplement/Vertebrates influential studies excluded.png", 
       width = 10, height = 8,
       dpi = 300)

p.post.invert <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  # annotate(geom = "label", x = -1.5, y = Inf, label = "More negative",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  # annotate(geom = "label", x = 1.5, y = Inf, label = "More positive",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  geom_errorbar(data = posthocs[analysis_group_category == "Invertebrates" &
                                  nativeness_var != "Africa_Comparison"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75))+
  geom_point(data = posthocs[analysis_group_category == "Invertebrates" &
                               nativeness_var != "Africa_Comparison"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[analysis_group_category == "Invertebrates" &
                              nativeness_var != "Africa_Comparison"], 
            aes(x = 1.5, y = analysis_group,
                color = contrast, 
                label = paste0("p=", p.value.rounded)),
            vjust = -1,
            position = position_dodgev(height = .75),
            size = 2.5)+
  scale_fill_manual("Contrast",
                    values = c("Introduced-Native" = "#57b7db",
                               "Invasive-Native" = "#faae6b"),
                    guide = guide_legend(reverse = TRUE))+
  scale_color_manual(values = c("Introduced-Native" = "#57b7db",
                                "Invasive-Native" = "#faae6b"))+
  guides(color = "none")+
  facet_grid(outlier_type~outlier_scale)+
  scale_y_discrete(labels = group_labels)+
  ylab(NULL)+
  xlab("Difference relative to native megafauna\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-3, 3), clip = "off")+
  theme_lundy
p.post.invert

ggsave("figures/revision/supplement/Invertebrates influential studies excluded.pdf", 
       width = 10, height = 8)
ggsave("figures/revision/supplement/Invertebrates influential studies excluded.png", 
       width = 10, height = 8,
       dpi = 300)

p.post.ecos <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  # annotate(geom = "label", x = -1.5, y = Inf, label = "More negative",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  # annotate(geom = "label", x = 1.5, y = Inf, label = "More positive",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  geom_errorbar(data = posthocs[analysis_group_category == "Ecosystem" &
                                  nativeness_var != "Africa_Comparison"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75))+
  geom_point(data = posthocs[analysis_group_category == "Ecosystem" &
                               nativeness_var != "Africa_Comparison"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[analysis_group_category == "Ecosystem" &
                              nativeness_var != "Africa_Comparison"], 
            aes(x = 3, y = analysis_group,
                color = contrast, 
                label = paste0("p=", p.value.rounded)),
            vjust = -.75,
            position = position_dodgev(height = .75),
            size = 2.5)+
  scale_fill_manual("Contrast",
                    values = c("Introduced-Native" = "#57b7db",
                               "Invasive-Native" = "#faae6b"),
                    guide = guide_legend(reverse = TRUE))+
  scale_color_manual(values = c("Introduced-Native" = "#57b7db",
                                "Invasive-Native" = "#faae6b"))+
  guides(color = "none")+
  facet_grid(outlier_type~outlier_scale)+
  scale_y_discrete(labels = group_labels)+
  ylab(NULL)+
  xlab("Difference relative to native megafauna\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-4, 4), clip = "off")+
  theme_lundy
p.post.ecos

ggsave("figures/revision/supplement/Ecosystem influential studies excluded.pdf", 
       width = 10, height = 14)
ggsave("figures/revision/supplement/Ecosystem influential studies excluded.png", 
       width = 10, height = 14,
       dpi = 300)

p.post.africa.1 <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  # annotate(geom = "label", x = -1, y = Inf, label = "More negative",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  # annotate(geom = "label", x = 1, y = Inf, label = "More positive",
  #          size = 2.75, label.size = NA, fontface = "italic")+
  geom_errorbar(data = posthocs[nativeness_var == "Africa_Comparison" &
                                  analysis_group_category == "Vertebrates"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75))+
  geom_point(data = posthocs[nativeness_var == "Africa_Comparison" &
                               analysis_group_category == "Vertebrates"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[nativeness_var == "Africa_Comparison" &
                              analysis_group_category == "Vertebrates"], 
            aes(x = 3, y = analysis_group,
                color = contrast, 
                label = paste0("p=", p.value.rounded)),
            vjust = -1,
            position = position_dodgev(height = .75),
            size = 2.5)+
  scale_fill_manual("Contrast",
                    values = c("Introduced-Intact_Africa" = "#57b7db"),
                    guide = guide_legend(reverse = TRUE))+
  scale_color_manual(values = c("Introduced-Intact_Africa" = "#57b7db"))+
  guides(color = "none")+
  scale_y_discrete(labels = group_labels)+
  facet_grid(outlier_type~outlier_scale)+
  ylab(NULL)+
  xlab("Difference relative to intact African assemblages\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-4, 4), clip = "off")+
  theme_lundy+
  theme(strip.placement = "outside")

p.post.africa.1

p.post.africa.2 <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  geom_errorbar(data = posthocs[nativeness_var == "Africa_Comparison" &
                                  analysis_group_category == "Invertebrates"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75))+
  geom_point(data = posthocs[nativeness_var == "Africa_Comparison" &
                               analysis_group_category == "Invertebrates"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[nativeness_var == "Africa_Comparison" &
                              analysis_group_category == "Invertebrates"], 
            aes(x = 3, y = analysis_group,
                color = contrast, 
                label = paste0("p=", p.value.rounded)),
            vjust = -1,
            position = position_dodgev(height = .75),
            size = 2.5)+
  scale_fill_manual("Contrast",
                    values = c("Introduced-Intact_Africa" = "#57b7db"),
                    guide = guide_legend(reverse = TRUE))+
  scale_color_manual(values = c("Introduced-Intact_Africa" = "#57b7db"))+
  guides(color = "none")+
  scale_y_discrete(labels = group_labels)+
  facet_grid(outlier_type~outlier_scale)+
  ylab(NULL)+
  xlab("Difference relative to intact African assemblages\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-4, 4), clip = "off")+
  theme_lundy+
  theme(strip.placement = "outside")

p.post.africa.2


p.post.africa.3 <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  geom_errorbar(data = posthocs[nativeness_var == "Africa_Comparison" &
                                  analysis_group_category == "Ecosystem"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75))+
  geom_point(data = posthocs[nativeness_var == "Africa_Comparison" &
                               analysis_group_category == "Ecosystem"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[nativeness_var == "Africa_Comparison" &
                              analysis_group_category == "Ecosystem"], 
            aes(x = 3, y = analysis_group,
                color = contrast, 
                label = paste0("p=", p.value.rounded)),
            vjust = -1,
            position = position_dodgev(height = .75),
            size = 2.5)+
  scale_fill_manual("Contrast",
                    values = c("Introduced-Intact_Africa" = "#57b7db"),
                    guide = guide_legend(reverse = TRUE))+
  scale_color_manual(values = c("Introduced-Intact_Africa" = "#57b7db"))+
  guides(color = "none")+
  scale_y_discrete(labels = group_labels)+
  facet_grid(outlier_type~outlier_scale)+
  ylab(NULL)+
  xlab("Difference relative to intact African assemblages\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-4, 4), clip = "off")+
  theme_lundy+
  theme(strip.placement = "outside")
p.post.africa.3



posthocs[nativeness_var == "Africa_Comparison", .(n = uniqueN(analysis_group)),
         by = .(analysis_group_category)]

p.post.africa.1 + theme(axis.text.x = element_blank(),
                        axis.title.x = element_blank(),
                        axis.ticks.x = element_blank(),
                        legend.position = "none")+
  p.post.africa.2 + theme(axis.text.x = element_blank(),
                          axis.title.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          legend.position = "none",
                          strip.text.x = element_blank()) +
  p.post.africa.3 + theme(strip.text.x = element_blank(),
                          legend.position = "none") +
  plot_layout(ncol = 1, heights = c(4/16, 4/16, 8/16))


ggsave("figures/revision/supplement/Africa influential studies excluded.pdf", 
       width = 10, height = 12)
ggsave("figures/revision/supplement/Africa influential studies excluded.png", 
       width = 10, height = 12,
       dpi = 300)
