# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# March 20th, 2025
#
#
#' [Plot results and produce summaries for text]
#
#
#
# Prepare workspace ---------------------------------------
#
#
#

rm(list = ls())
gc()

library("groundhog")
groundhog.day <- "2025-04-15"

libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "shades", "patchwork", "dplyr",
          "scico", "beepr", "gt",
          "ggtext", "ggstance")
groundhog.library(libs, groundhog.day)

# >>> Helper functions  -------------------------------------

getVars <- function(formula_str){
  formula_str <- gsub("yi ~ ", "", formula_str)
  formula_str <- gsub("- 1", "", formula_str)
  
  formula_str <- gsub("[*]", "+", formula_str)
  vars <- unlist(strsplit(formula_str, split = "[+]"))
  vars <- trimws(vars)
  vars
  return(vars)
}

tidy_with_CIs <- function(m){
  x <- tidy(m) %>%
    mutate(ci.lb = m$ci.lb, ci.ub = m$ci.ub)
  return(x)
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

I2 <- function(mod){
  
  W <- diag(1/mod$vi)
  X <- model.matrix(mod)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  return(100 * sum(mod$sigma2) / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P))))
  
}

# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# Load datasets and model guide -----------------------------------------------

master_dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")

unique(master_dat$analysis_group_category)
unique(master_dat$analysis_group)

master_dat[is.na(yi)]
master_dat[, .(n = uniqueN(data_point_ID), refs = uniqueN(Citation))]

master_guide <- fread("outputs/revision/summaries/model_comparison_table.csv")
master_guide
sort(unique(master_guide$analysis_group))

master_posthocs <- fread("outputs/revision/summaries/posthoc_comparisons.csv")

sort(unique(master_guide$analysis_group))

#
sub_guide <- master_guide[preferred_model == "yes", ]
posthocs <- master_posthocs[preferred_model == "yes", ]
sort(unique(master_guide$analysis_group))

dat <- copy(master_dat)
dat[yi > 10, ]

file_epithet <- "smd_no_filtering"

unique(dat$analysis_group_category)
dat[analysis_group_category %in% c("Microbes", "Ecosystem", "Soil", "Plant_Nutrients"), analysis_group_category := "Ecosystem"]
posthocs[analysis_group_category %in% c("Microbes", "Ecosystem", "Soil", "Plant_Nutrients"), analysis_group_category := "Ecosystem"]
sub_guide[analysis_group_category %in% c("Microbes", "Ecosystem", "Soil", "Plant_Nutrients"), analysis_group_category := "Ecosystem"]

length(unique(sub_guide$analysis_group))

# *** Set plotting constants ----------------------------------------------

tertiary_palette <- c("Introduced" = "#57b7db",
                      "Invasive" = "#faae6b", 
                      "Native" = "#a7928c")

africa_palette <- c("Introduced" = "#57b7db",
                    "Intact_Africa" = "#d7a4a3")

quaternary_palette <- c("Introduced" = "#57b7db",
                      "Invasive" = "#faae6b", 
                      "Native" = "#a7928c",
                      "Intact_Africa" = "#d7a4a3")

theme_lundy <-   theme_bw()+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(color = "black"),  
        axis.text = element_text(color = "black"), 
        panel.grid = element_blank(),
        panel.border = element_blank())

dat[, wi := 1/sqrt(vi)]
# For SMD:
dat[, pt_size := 10 * (wi-min(wi, na.rm=T)) / (max(wi, na.rm = T) - min(wi, na.rm=T)) + 0.01]

# For ROM:
# dat[, pt_size :=  (wi-min(wi, na.rm=T)) / (max(wi, na.rm = T) - min(wi, na.rm=T)) + 2]

# dat[, pt_size := 1/sqrt(vi)]
range(dat$pt_size)

# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# Main text figures ----------------------------------------------------

# >>> Prepare data ----------------------------------------------------------------

sort(unique(sub_guide$analysis_group))
sort(unique(sub_guide$analysis_group_category))

unique(sub_guide$preferred_model)

sub_guide[duplicated(model_comparison_id)]
#' *should be 0 rows*

# >>> Load and tidy models -----------------------------------------------------
# Do this for each nativeness_var separately and then rbind (after standardizing names)

mods1 <- load_and_tidy_model_series(sub_guide[nativeness_var == "Herbivore_nativeness", ]$model_path_nativeness,
                                   var = "Herbivore_nativeness",
                                   has_intercept = TRUE)
names(mods1) <- sub_guide[nativeness_var == "Herbivore_nativeness", ]$model_id_nativeness

mods2 <- load_and_tidy_model_series(sub_guide[nativeness_var == "Invasive", ]$model_path_nativeness,
                           var = "Invasive",
                           has_intercept = TRUE)
names(mods2) <- sub_guide[nativeness_var == "Invasive", ]$model_id_nativeness

mods3 <- load_and_tidy_model_series(sub_guide[nativeness_var == "Africa_Comparison", ]$model_path_nativeness,
                                    var = "Africa_Comparison",
                                    has_intercept = TRUE)
names(mods3) <- sub_guide[nativeness_var == "Africa_Comparison", ]$model_id_nativeness

mods2 <- lapply(mods2,
                function(x){
                  setnames(x, "Invasive", "Herbivore_nativeness")
                })
mods3 <- lapply(mods3,
                function(x){
                  setnames(x, "Africa_Comparison", "Herbivore_nativeness")
                })


mods <- rbindlist(c(mods1, mods2, mods3), idcol = "model_id_nativeness")
mods
mods <- merge(mods,
              sub_guide[, .(model_id_nativeness, analysis_group, 
                            analysis_group_category, nativeness_var,
                            min_refs,
                            max_refs, min_obs, max_obs)])
mods
setnames(mods, "Herbivore_nativeness", "nativeness_comparison")

sort(unique(mods$analysis_group))
unique(mods$nativeness_comparison)

# Melt dataset so the 3 comparison columns are in 1 column:
sub_dat.mlt <- melt(dat[analysis_group %in% sub_guide$analysis_group, 
                            .(data_point_ID, Citation, yi, pt_size,
                                analysis_group, analysis_group_category,
                                Herbivore_nativeness, Invasive, Africa_Comparison)],
                    measure.vars = c("Herbivore_nativeness", "Invasive", "Africa_Comparison"),
                    value.name = "nativeness_comparison",
                    variable.name = "nativeness_var")

# Drop redundant 'Native' for Invasive models
sub_dat.mlt <- sub_dat.mlt[!(nativeness_var == "Invasive" &
                               nativeness_comparison == "Native")]

mods <- mods[!(nativeness_var == "Invasive" &
               nativeness_comparison == "Native")]

# >>> Prepare for plotting ------------------------------------------------

unique(mods$analysis_group)

lvls <- c("Growth_Rates",
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
setdiff(mods$analysis_group, lvls)
setdiff(sub_dat.mlt$analysis_group, lvls)
setdiff(lvls, sub_dat.mlt$analysis_group)

# 
N <- sub_dat.mlt[, .(obs = .N, refs = uniqueN(Citation)),
             by = .(analysis_group, analysis_group_category, 
                    nativeness_var,
                    nativeness_comparison)]
N[, n_lab := paste0(obs, "(", refs, ")")]
N

#
mods$analysis_group <- factor(mods$analysis_group,
                              levels = rev(lvls))
sub_dat.mlt$analysis_group <- factor(sub_dat.mlt$analysis_group,
                                 levels = rev(lvls))
N$analysis_group <- factor(N$analysis_group,
                           levels = rev(lvls))

unique(mods$analysis_group)
unique(sub_dat.mlt$analysis_group)

group_labels <- unique(mods$analysis_group)
group_labels <- gsub("_", " ", group_labels)
group_labels <- gsub("Invert ", "", group_labels)
names(group_labels) <- unique(mods$analysis_group)
group_labels <- gsub("Vert Carn ", "Carnivore ", group_labels)
group_labels <- gsub("Vert Herb ", "Herbivore ", group_labels)
group_labels[group_labels == "Growth Rates"] <- "Plant Growth Rates"

unique(mods$nativeness_comparison)
mods$nativeness_comparison <- factor(mods$nativeness_comparison,
                                    levels = rev(c("Native", "Intact_Africa", "Introduced", "Invasive")))

sub_dat.mlt$nativeness_comparison <- factor(sub_dat.mlt$nativeness_comparison,
                                    levels = rev(c("Native", "Intact_Africa", "Introduced", "Invasive")))

N$nativeness_comparison <- factor(N$nativeness_comparison,
                                    levels = rev(c("Native", "Intact_Africa","Introduced", "Invasive")))


unique(mods$analysis_group_category)
mods$analysis_group_category <- factor(mods$analysis_group_category,
                                     levels = rev(c("Ecosystem", "Soil", "Microbes", "Invertebrates", "Vertebrates")))

sub_dat.mlt$analysis_group_category <- factor(sub_dat.mlt$analysis_group_category,
                                              levels = rev(c("Ecosystem", "Soil", "Microbes","Invertebrates", "Vertebrates")))

N$analysis_group_category <- factor(N$analysis_group_category,
                                    levels = rev(c("Ecosystem", "Soil", "Microbes","Invertebrates", "Vertebrates")))

# Drop some combinations that don't have models:
mods[, key := paste(analysis_group, nativeness_comparison, nativeness_var)]
sub_dat.mlt[, key := paste(analysis_group, nativeness_comparison, nativeness_var)]
N[, key := paste(analysis_group, nativeness_comparison, nativeness_var)]
sub_dat.mlt <- sub_dat.mlt[key %in% mods$key]
N <- N[key %in% mods$key]

# >>> Figures -------------------------------------------------------------
# Ecosystems:
eco <- ggplot()+
  annotate(geom = "label", y = -2, x = Inf, label = "Negative effect",
           size = 2.75, label.size = NA, fontface = "italic")+
  annotate(geom = "label", y = 2, x = Inf, label = "Positive effect",
           size = 2.75, label.size = NA, fontface = "italic")+
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")+
  geom_text(data = N[analysis_group_category %in% c("Ecosystem", "Soil", "Microbes") &
                        nativeness_var != "Africa_Comparison"],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[analysis_group_category %in% c("Ecosystem", "Soil", "Microbes") &
                                   nativeness_var != "Africa_Comparison"], 
              aes(y = yi, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[analysis_group_category %in% c("Ecosystem", "Soil", "Microbes") &
                              nativeness_var != "Africa_Comparison"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                     x = analysis_group,
                     group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .5)+
  geom_point(data = mods[analysis_group_category %in% c("Ecosystem", "Soil", "Microbes") &
                                nativeness_var != "Africa_Comparison"], 
                  aes(y = pred, #ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
             size = 3,
                  shape = 21, position = position_dodge(width = .66))+
  scale_size_identity()+
  scale_fill_manual(name = "Megafauna nativeness", 
                    values=rev(tertiary_palette),
                    labels = c("Native" = "Native",
                               "Introduced" = "Introduced",
                               "Invasive" = "'Invasive'"))+
  # try to make text color darker
  scale_color_manual(name = "Megafauna nativeness", 
                     values=rev(tertiary_palette),
                     labels = c("Native" = "Native",
                                "Introduced" = "Introduced",
                                "Invasive" = "'Invasive'"))+
  scale_x_discrete(labels = group_labels)+
  xlab(NULL)+
  ylab("Effect (Hedges' g ± CIs)")+
  guides(size = "none",
         color = guide_legend(reverse=TRUE),
         fill = guide_legend(reverse=TRUE) )+
  theme_lundy+
  theme(axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  coord_flip(ylim = c(-7, 7),
             clip = "off")
eco

inverts <- ggplot()+
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")+
  geom_text(data = N[analysis_group_category == "Invertebrates" &
                        nativeness_var != "Africa_Comparison"],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[analysis_group_category == "Invertebrates" &
                                   nativeness_var != "Africa_Comparison"], 
              aes(y = yi, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[analysis_group_category == "Invertebrates" &
                              nativeness_var != "Africa_Comparison"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                    x = analysis_group,
                    group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .5)+
  geom_point(data = mods[analysis_group_category == "Invertebrates" &
                                nativeness_var != "Africa_Comparison"], 
                  aes(y = pred, #ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
             size = 3)+
  scale_size_identity()+
  scale_fill_manual(name = "Megafauna nativeness", 
                    values=rev(tertiary_palette),
                    labels = c("Native" = "Native",
                               "Introduced" = "Introduced",
                               "Invasive" = "'Invasive'"))+
  # try to make text color darker
  scale_color_manual(name = "Megafauna nativeness", 
                     values=rev(tertiary_palette),
                     labels = c("Native" = "Native",
                                "Introduced" = "Introduced",
                                "Invasive" = "'Invasive'"))+
  scale_x_discrete(labels = group_labels)+
  facet_wrap(~analysis_group_category,
             strip.position = "left")+
  xlab(NULL)+
  ylab("Effect (Hedges' g ± CIs)")+
  guides(size = "none",
         color = guide_legend(reverse=TRUE),
         fill = guide_legend(reverse=TRUE) )+
  theme_lundy+
  theme(axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.placement = "outside")+
  coord_flip(ylim = c(-7, 7),
             clip = "off")
inverts

verts <- ggplot()+
  annotate(geom = "label", y = -2, x = Inf, label = "Negative effect",
           size = 2.75, label.size = NA, fontface = "italic")+
  annotate(geom = "label", y = 2, x = Inf, label = "Positive effect",
           size = 2.75, label.size = NA, fontface = "italic")+
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")+
  geom_text(data = N[analysis_group_category == "Vertebrates" &
                        nativeness_var != "Africa_Comparison"],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[analysis_group_category == "Vertebrates" &
                                   nativeness_var != "Africa_Comparison"], 
              aes(y = yi, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[analysis_group_category == "Vertebrates" &
                              nativeness_var != "Africa_Comparison"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                    x = analysis_group,
                    group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .5)+
  geom_point(data = mods[analysis_group_category == "Vertebrates" &
                                nativeness_var != "Africa_Comparison"], 
                  aes(y = pred, #ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
             size = 3)+
  scale_size_identity()+
  scale_fill_manual(name = "Megafauna nativeness", 
                    values=rev(tertiary_palette),
                    labels = c("Native" = "Native",
                               "Introduced" = "Introduced",
                               "Invasive" = "'Invasive'"))+
  # try to make text color darker
  scale_color_manual(name = "Megafauna nativeness", 
                     values=rev(tertiary_palette),
                     labels = c("Native" = "Native",
                                "Introduced" = "Introduced",
                                "Invasive" = "'Invasive'"))+
  scale_x_discrete(labels = group_labels)+
  facet_wrap(~analysis_group_category,
             strip.position = "left")+
  xlab(NULL)+
  ylab("Effect (Hedges' g ± CIs)")+
  guides(size = "none",
         color = guide_legend(reverse=TRUE),
         fill = guide_legend(reverse=TRUE) )+
  theme_lundy+
  theme(axis.ticks.y = element_blank(),
        plot.title = element_text(hjust = 0.5),
        strip.placement = "outside")+
  coord_flip(ylim = c(-7, 7),
             clip = "off")
verts

africa.p.1 <- ggplot()+
  geom_text(data = data.table(x = c(Inf, Inf), y = c(-2, 2), 
                              analysis_group_category = factor("Vertebrates"),
                              label = c("Negative effect", "Positive effect")),
            aes(y = y, x = x, label = label),
            size = 2.75, fontface = "italic")+
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")+
  geom_text(data = N[nativeness_var == "Africa_Comparison" &
                        analysis_group_category == "Vertebrates"],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[nativeness_var == "Africa_Comparison" &
                                   analysis_group_category == "Vertebrates"], 
              aes(y = yi, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[nativeness_var == "Africa_Comparison" &
                              analysis_group_category == "Vertebrates"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                    x = analysis_group,
                    group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .5)+
  geom_point(data = mods[nativeness_var == "Africa_Comparison" &
                                analysis_group_category == "Vertebrates"], 
                  aes(y = pred, #ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
             size = 3)+
  scale_size_identity()+
  scale_fill_manual(name = NULL, 
                    values= c("Intact_Africa" = "#d7a4a3",
                              "Introduced" = "#57b7db"),
                    labels = c("Intact_Africa" = "African assemblage",
                               "Introduced" = "Introduced"))+
  scale_color_manual(name = NULL, 
                    values= c("Intact_Africa" = "#d7a4a3",
                              "Introduced" = "#57b7db"),
                    labels = c("Intact_Africa" = "African assemblage",
                               "Introduced" = "Introduced"))+
  scale_x_discrete(labels = group_labels)+
  facet_wrap(~analysis_group_category, strip.position = "left",
             scales = "free_y", ncol = 1)+
  xlab(NULL)+
  ylab("Effect (Hedges' g ± CIs)")+
  guides(size = "none",
         color = guide_legend(reverse=TRUE),
         fill = guide_legend(reverse=TRUE) )+
  theme_lundy+
  theme(axis.ticks.y = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5))+
  coord_flip(ylim = c(-7, 7), clip = 'off')
africa.p.1

africa.p.2 <- ggplot()+
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")+
  geom_text(data = N[nativeness_var == "Africa_Comparison" &
                        analysis_group_category == "Invertebrates"],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[nativeness_var == "Africa_Comparison" &
                                   analysis_group_category == "Invertebrates"], 
              aes(y = yi, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[nativeness_var == "Africa_Comparison" &
                              analysis_group_category == "Invertebrates"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                    x = analysis_group,
                    group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .5)+
  geom_point(data = mods[nativeness_var == "Africa_Comparison" &
                                analysis_group_category == "Invertebrates"], 
                  aes(y = pred, #ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
             size = 3)+
  scale_size_identity()+
  scale_fill_manual(name = NULL, 
                    values= c("Intact_Africa" = "#d7a4a3",
                              "Introduced" = "#57b7db"),
                    labels = c("Intact_Africa" = "African assemblage",
                               "Introduced" = "Introduced"))+
  scale_color_manual(name = NULL, 
                     values= c("Intact_Africa" = "#d7a4a3",
                               "Introduced" = "#57b7db"),
                     labels = c("Intact_Africa" = "African assemblage",
                                "Introduced" = "Introduced"))+
  scale_x_discrete(labels = group_labels)+
  facet_wrap(~analysis_group_category, strip.position = "left",
             scales = "free_y", ncol = 1)+
  xlab(NULL)+
  ylab("Effect (Hedges' g ± CIs)")+
  guides(size = "none",
         color = guide_legend(reverse=TRUE),
         fill = guide_legend(reverse=TRUE) )+
  theme_lundy+
  theme(axis.ticks.y = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5))+
  coord_flip(ylim = c(-7, 7), clip = 'off')
africa.p.2

africa.p.3 <- ggplot()+
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")+
  geom_text(data = N[nativeness_var == "Africa_Comparison" &
                        analysis_group_category %in% c("Ecosystem", "Soil", "Microbes")],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[nativeness_var == "Africa_Comparison" &
                                   analysis_group_category %in% c("Ecosystem", "Soil", "Microbes")],
              aes(y = yi, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[nativeness_var == "Africa_Comparison" &
                              analysis_group_category %in% c("Ecosystem", "Soil", "Microbes")],
                aes(ymin = ci.lb, ymax = ci.ub, 
                    x = analysis_group,
                    group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .5)+
  geom_point(data = mods[nativeness_var == "Africa_Comparison" &
                                analysis_group_category %in% c("Ecosystem", "Soil", "Microbes")],
                  aes(y = pred, #ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
             size = 3)+
  scale_size_identity()+
  scale_fill_manual(name = NULL, 
                    values= c("Intact_Africa" = "#d7a4a3",
                              "Introduced" = "#57b7db"),
                    labels = c("Intact_Africa" = "African assemblage",
                               "Introduced" = "Introduced"))+
  scale_color_manual(name = NULL, 
                     values= c("Intact_Africa" = "#d7a4a3",
                               "Introduced" = "#57b7db"),
                     labels = c("Intact_Africa" = "African assemblage",
                                "Introduced" = "Introduced"))+
  scale_x_discrete(labels = group_labels)+
  facet_wrap(~analysis_group_category, strip.position = "left",
             scales = "free_y", ncol = 1)+
  xlab(NULL)+
  ylab("Effect (Hedges' g ± CIs)")+
  guides(size = "none",
         color = guide_legend(reverse=TRUE),
         fill = guide_legend(reverse=TRUE) )+
  theme_lundy+
  theme(axis.ticks.y = element_blank(),
        strip.placement = "outside",
        plot.title = element_text(hjust = 0.5))+
  coord_flip(ylim = c(-7, 7), clip = 'off')
africa.p.3

# ~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------------
# Posthoc differences figure ------------------------------------------------------------

# >>> Prepare for plotting ------------------------------------------------

unique(posthocs$contrast)

posthocs[p.value < 0.05, ]
posthocs[, p.value.rounded := round(p.value, 2)]
posthocs[p.value.rounded == 0, ]$p.value
posthocs[p.value.rounded == 0, p.value.rounded := 0.003]

unique(posthocs$contrast)
posthocs$contrast <- factor(posthocs$contrast,
                            levels = rev(c("Introduced-Native",
                                           "Invasive-Native",
                                           "Introduced-Intact_Africa")))

levels(mods$analysis_group)
posthocs$analysis_group <- factor(posthocs$analysis_group,
                                  levels(mods$analysis_group))

posthocs$analysis_group_category <- factor(posthocs$analysis_group_category,
                                  levels(mods$analysis_group_category))

# >>> Figures -------------------------------------------------------------

p.post.vert <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  annotate(geom = "label", x = -1.5, y = Inf, label = "More negative",
           size = 2.75, label.size = NA, fontface = "italic")+
  annotate(geom = "label", x = 1.5, y = Inf, label = "More positive",
           size = 2.75, label.size = NA, fontface = "italic")+
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
  scale_y_discrete(labels = group_labels)+
  ylab(NULL)+
  xlab("Difference relative to native megafauna\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-3, 3), clip = "off")+
  theme_lundy
p.post.vert

verts + p.post.vert + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  plot_layout(widths = c(.66, .33))+
  plot_annotation(tag_levels = "A")

#
p.post.invert <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
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
            aes(x = 2, y = analysis_group,
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
  scale_y_discrete(labels = group_labels)+
  ylab(NULL)+
  xlab("Difference relative to native megafauna\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-3, 3), clip = "off")+
  theme_lundy
p.post.invert

inverts + p.post.invert + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  plot_layout(widths = c(.66, .33))+
  plot_annotation(tag_levels = "A")

p.post.eco <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  annotate(geom = "label", x = -1, y = Inf, label = "More negative",
           size = 2.75, label.size = NA, fontface = "italic")+
  annotate(geom = "label", x = 1, y = Inf, label = "More positive",
           size = 2.75, label.size = NA, fontface = "italic")+
  geom_errorbar(data = posthocs[analysis_group_category %in% c("Ecosystem", "Soil", "Microbes") &
                                  nativeness_var != "Africa_Comparison"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75),
                shape = 21)+
  geom_point(data = posthocs[analysis_group_category %in% c("Ecosystem", "Soil", "Microbes") &
                               nativeness_var != "Africa_Comparison"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[analysis_group_category %in% c("Ecosystem", "Soil", "Microbes") &
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
  scale_y_discrete(labels = group_labels)+
  ylab(NULL)+
  xlab("Difference relative to native megafauna\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-4, 4), clip = "off")+
  theme_lundy
p.post.eco

p.post.africa.1 <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  annotate(geom = "label", x = -1, y = Inf, label = "More negative",
           size = 2.75, label.size = NA, fontface = "italic")+
  annotate(geom = "label", x = 1, y = Inf, label = "More positive",
           size = 2.75, label.size = NA, fontface = "italic")+
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
  facet_wrap(~analysis_group_category, strip.position = "left",
             scales = "free_y", ncol = 1)+
  ylab(NULL)+
  xlab("Difference relative to African assemblages\n(Hedges' g ± CIs)")+
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
  facet_wrap(~analysis_group_category, strip.position = "left",
             scales = "free_y", ncol = 1)+
  ylab(NULL)+
  xlab("Difference relative to African assemblages\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-4, 4), clip = "off")+
  theme_lundy+
  theme(strip.placement = "outside")

p.post.africa.2


p.post.africa.3 <- ggplot()+
  geom_vline(xintercept = 0, color = "grey50", linetype = "dashed")+
  geom_errorbar(data = posthocs[nativeness_var == "Africa_Comparison" &
                                  analysis_group_category %in% c("Ecosystem", "Soil", "Microbes")],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75))+
  geom_point(data = posthocs[nativeness_var == "Africa_Comparison" &
                               analysis_group_category %in% c("Ecosystem", "Soil", "Microbes")],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[nativeness_var == "Africa_Comparison" &
                              analysis_group_category %in% c("Ecosystem", "Soil", "Microbes")], 
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
  facet_wrap(~analysis_group_category, strip.position = "left",
             scales = "free_y", ncol = 1)+
  ylab(NULL)+
  xlab("Difference relative to African assemblages\n(Hedges' g ± CIs)")+
  coord_cartesian(xlim = c(-4, 4), clip = "off")+
  theme_lundy+
  theme(strip.placement = "outside")
p.post.africa.3

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Patchwork main text figures ---------------------------------------------
N[nativeness_var != "Africa_Comparison", .(n = uniqueN(analysis_group)),
  by = analysis_group_category]

#
fig1 <- verts + theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.x = element_blank(),
                      legend.position = "none")+
  p.post.vert + theme(axis.text.x = element_blank(),
                      axis.ticks.x = element_blank(),
                      axis.title.x = element_blank(),
                      axis.text.y = element_blank(),
                      axis.ticks.y = element_blank(),
                      legend.position = "none")+
  inverts + 
  p.post.invert + theme(axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        legend.position = "none")+
  plot_layout(ncol = 2, 
              heights = c(7/12, 5/12),
              widths = c(.66, .33))+
  plot_annotation(tag_levels = "A")
fig1

ggsave(paste0("figures/revision/main_text/raw/Fig 1A-D raw_", file_epithet, ".pdf"), width = 10, height = 9)

#
eco + p.post.eco + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  plot_annotation(tag_levels = "A")

ggsave(paste0("figures/revision/main_text/raw/Fig 2A & B raw_", file_epithet, ".pdf"), width = 11, height = 10)



#
N[nativeness_var == "Africa_Comparison", .(n = uniqueN(analysis_group)),
  by = analysis_group_category]

africa.p.1 + theme(axis.text.x = element_blank(),
                               axis.title.x = element_blank(),
                               axis.ticks.x = element_blank(),
                               legend.position = "none")+
  p.post.africa.1 + theme(axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          axis.title = element_blank(),
                          strip.text = element_blank(),
                          legend.position = "none") +
  africa.p.2 + theme(axis.text.x = element_blank(),
                     axis.title.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     legend.position = "none")+
  p.post.africa.2 + theme(axis.text = element_blank(),
                          axis.ticks = element_blank(),
                          axis.title = element_blank(),
                          strip.text = element_blank(),
                          legend.position = "none") +
  africa.p.3 + 
  p.post.africa.3 + theme(axis.text.y = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.title.y = element_blank(),
                          strip.text = element_blank())+
  plot_layout(ncol = 2, heights = c(4/16, 4/16, 8/16), widths = c(.66, .33))+
  plot_annotation(tag_levels = "A")

ggsave(paste0("figures/revision/main_text/raw/Fig 3 raw_", file_epithet, ".pdf"), width = 10, height = 8)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Text summaries ------------------------------------------------------

sub_guide[LRT_pval < 0.05 & nativeness_var != "Africa_Comparison", ]

posthocs[p.value < 0.05 & nativeness_var != "Africa_Comparison", ]

# ------------  Animals ------------------------------------------------!
unique(sub_guide$analysis_group_category)

length(unique(sub_guide$analysis_group))

posthocs[analysis_group_category %in% c("Vertebrates", "Invertebrates") &
           p.value < 0.05 
         & nativeness_var != "Africa_Comparison", ]

unique(sub_guide[analysis_group_category %in% c("Vertebrates", "Invertebrates") &
                 nativeness_var != "Africa_Comparison"]$analysis_group)

range(posthocs[analysis_group_category %in% c("Vertebrates", "Invertebrates")
               & nativeness_var != "Africa_Comparison", ]$statistic)
range(posthocs[analysis_group_category %in% c("Vertebrates", "Invertebrates")
               & nativeness_var != "Africa_Comparison", ]$p.value)
#
range(posthocs[analysis_group_category %in% c("Vertebrates", "Invertebrates")
               & nativeness_var != "Africa_Comparison", ]$contrast_df)
range(sub_guide[analysis_group_category %in% c("Vertebrates", "Invertebrates")
                & nativeness_var != "Africa_Comparison", ]$LRT)
range(sub_guide[analysis_group_category %in% c("Vertebrates", "Invertebrates")
                & nativeness_var != "Africa_Comparison", ]$LRT_pval)


# ------------  Vertebrates ------------------------------------------------!
unique(sub_guide$analysis_group_category)

length(unique(sub_guide$analysis_group))

posthocs[analysis_group_category == "Vertebrates" &
           p.value < 0.05 
         & nativeness_var != "Africa_Comparison", ]

unique(sub_guide[analysis_group_category == "Vertebrates" 
                 & nativeness_var != "Africa_Comparison"]$analysis_group)

range(posthocs[analysis_group_category == "Vertebrates"
               & nativeness_var != "Africa_Comparison", ]$statistic)
range(posthocs[analysis_group_category == "Vertebrates"
               & nativeness_var != "Africa_Comparison", ]$p.value)
#
range(posthocs[analysis_group_category == "Vertebrates"
               & nativeness_var != "Africa_Comparison", ]$contrast_df)
range(sub_guide[analysis_group_category == "Vertebrates"
                & nativeness_var != "Africa_Comparison", ]$LRT)
range(sub_guide[analysis_group_category == "Vertebrates"
                & nativeness_var != "Africa_Comparison", ]$LRT_pval)

# ------------  Invertebrates ------------------------------------------------!
unique(sub_guide$analysis_group_category)

length(unique(sub_guide$analysis_group))
range(posthocs[analysis_group_category == "Invertebrates"
               & nativeness_var != "Africa_Comparison", ]$statistic)

range(posthocs[analysis_group_category == "Invertebrates"
               & nativeness_var != "Africa_Comparison", ]$contrast_df)

range(posthocs[analysis_group_category == "Invertebrates"
               & nativeness_var != "Africa_Comparison", ]$p.value)

range(sub_guide[analysis_group_category == "Invertebrates"
                & nativeness_var != "Africa_Comparison", ]$LRT)
range(sub_guide[analysis_group_category == "Invertebrates"
                & nativeness_var != "Africa_Comparison", ]$LRT_pval)

# ------------  Ecosystems ------------------------------------------------!
length(unique(sub_guide$analysis_group))

range(posthocs[analysis_group_category == "Ecosystem"
               & nativeness_var != "Africa_Comparison", ]$statistic)

range(posthocs[analysis_group_category == "Ecosystem"
               & nativeness_var != "Africa_Comparison", ]$contrast_df)

range(posthocs[analysis_group_category == "Ecosystem"
               & nativeness_var != "Africa_Comparison", ]$p.value)

range(sub_guide[analysis_group_category == "Ecosystem"
                & nativeness_var != "Africa_Comparison", ]$LRT)
range(sub_guide[analysis_group_category == "Ecosystem"
                & nativeness_var != "Africa_Comparison", ]$LRT_pval)


 # ----------- AFRICA -----------------------------------------!

length(posthocs[nativeness_var == "Africa_Comparison"]$analysis_group)

posthocs[nativeness_var == "Africa_Comparison" &
           p.value < 0.05, ]
range(posthocs[nativeness_var == "Africa_Comparison" &
                 p.value < 0.05, ]$statistic)
range(posthocs[nativeness_var == "Africa_Comparison" &
                 p.value < 0.05, ]$contrast_df)
range(posthocs[nativeness_var == "Africa_Comparison" &
                 p.value < 0.05, ]$p.value)
sub_guide[nativeness_var == "Africa_Comparison" &
            LRT_pval < 0.05, ]
mods[nativeness_var == "Africa_Comparison" & analysis_group == "Soil_Mg"]
sub_guide[nativeness_var == "Africa_Comparison" & analysis_group == "Primary_Productivity"]


range(posthocs[nativeness_var == "Africa_Comparison" &
                 p.value >= 0.05, ]$statistic)
range(posthocs[nativeness_var == "Africa_Comparison" &
                 p.value>=0.05, ]$contrast_df)
range(posthocs[nativeness_var == "Africa_Comparison" &
                 p.value >= 0.05, ]$p.value)
range(sub_guide[nativeness_var == "Africa_Comparison" &
                  LRT_pval >= 0.05, ]$LRT)
range(sub_guide[nativeness_var == "Africa_Comparison" &
                  LRT_pval >= 0.05, ]$LRT_pval)


mods[model_id_nativeness %in% posthocs[nativeness_var == "Africa_Comparison" &
                                         p.value <= 0.05, ]$model_id_nativeness, ]


# >>> Overall effects from intercept-only models --------------------------
paths <- sub_guide$model_path_null

intercepts <- lapply(paths, function(x){
  m <- readRDS(x)
  out <- m |>
    predict() |>
    as.data.frame() |>
    setDT() 
  out[, pval := m$pval]
  return(out)
  
})

length(intercepts) == length(paths)

names(intercepts) <- sub_guide$model_id_null

intercepts <- rbindlist(intercepts, idcol = "model_id_null")

intercepts <- merge(intercepts,
                    unique(sub_guide[, .(model_id_null, analysis_group, nativeness_var,
                                         analysis_group_category)]),
                    by = "model_id_null")

intercepts

unique(intercepts$analysis_group)
intercepts[analysis_group == "Soil_pH"]
intercepts[analysis_group == "Soil_pH" & nativeness_var == "Africa_Comparison"]

intercepts[pval < 0.05, ]

intercepts[analysis_group_category == "Vertebrates" & pval < 0.05 
           & nativeness_var == "Herbivore_nativeness", ]

intercepts[analysis_group_category == "Invertebrates" & pval < 0.05 
           & nativeness_var == "Herbivore_nativeness", ]


range(intercepts[analysis_group_category == "Invertebrates"
                 & nativeness_var == "Herbivore_nativeness"]$pred)
range(intercepts[analysis_group_category == "Invertebrates"
                 & nativeness_var == "Herbivore_nativeness"]$pval)

intercepts[analysis_group_category == "Ecosystem" & 
             nativeness_var == "Herbivore_nativeness"& pval < 0.05, ]
round(intercepts[analysis_group_category == "Ecosystem" & 
                   nativeness_var == "Herbivore_nativeness" & pval < 0.05, ]$pval, 5)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------
# Table S1 ----------------------------------------------------------------

sub_guide
posthocs
sub_guide

table <- sub_guide[, .(model_id_nativeness, model_path_nativeness,
                       analysis_group_category, analysis_group, nativeness_var,
                       random_effect, min_obs, max_obs, min_refs, max_refs,
                       formula_nativeness, formula_null, i2_nativeness, i2_null,
                       I2_star, sum_sigma_nativeness, sum_sigma_null,
                       R2_cond,
                       LRT, LRT_pval, prop_variance_reduced)]

table.m <- merge(table,
                 posthocs[, .(model_id_nativeness, contrast, estimate, statistic, p.value, ci.lb, ci.ub,
                          intercept_df, contrast_df)],
                 by = "model_id_nativeness")

table.m

#
range(table.m$LRT)
range(table.m$LRT_pval)
range(table.m$i2_nativeness)
range(table.m$prop_variance_reduced)
range(table.m$estimate)
range(table.m$p.value)

#
table.m[, `:=` (LRT = ifelse(round(LRT, 2) == 0, 
                             "<0.01", round(LRT, 2)),
                LRT_pval = ifelse(round(LRT_pval, 3) == 0,
                                  "<0.001", round(LRT_pval, 3)),
                i2_nativeness = ifelse(round(i2_nativeness, 2) == 0,
                                       "<0.01", round(i2_nativeness, 2)),
                i2_null = ifelse(round(i2_null, 2) == 0,
                                 "<0.01", round(i2_null, 2)),
                R2_cond = ifelse(round(R2_cond, 2) == 0,
                                 "<0.01", round(R2_cond, 2)),
                I2_star = ifelse(round(I2_star, 2) == 0, 
                                 "<0.01", round(I2_star, 2)),
                
                sum_sigma_nativeness = ifelse(round(sum_sigma_nativeness, 2) == 0, 
                                              "<0.01", round(sum_sigma_nativeness, 2)),
                sum_sigma_null = ifelse(round(sum_sigma_null, 2) == 0, 
                                        "<0.01", round(sum_sigma_null, 2)),
                
                prop_variance_reduced = ifelse(round(prop_variance_reduced, 2) == 0,
                                               "<0.01", round(prop_variance_reduced, 2)),
                estimate = round(estimate, 2),
                statistic = round(statistic, 2),
                p.value = ifelse(round(p.value, 3) == 0,
                                 "<0.001", round(p.value, 3)),
                ci.lb = round(ci.lb, 2),
                ci.ub = round(ci.ub, 2))]

#
table.m[, model_comparison_string := paste0("LRT=", LRT, ", p=", LRT_pval)]

table.m[, fit_string1 := paste0("sigma2_null=", sum_sigma_null,
                               ", sigma2_nativeness=", sum_sigma_nativeness)] 
table.m[, fit_string2 := paste0("I2_null=", i2_null,
                                ", I2_nativeness=", i2_nativeness)]# I think it makes more sense to call it R2* than I2*...
table.m[, fit_string3 := paste0("R2=", R2_cond,
                                ", R2*=", I2_star)]#
table.m$fit_string1
table.m$fit_string2
table.m[, `:=` (fit_string1 = gsub("=<", "<", fit_string1),
                fit_string2 = gsub("=<", "<", fit_string2),
                fit_string3 = gsub("=<", "<", fit_string3))]

table.m


# ", I2=", i2_nativeness, ", prop. variance reduced=", prop_variance_reduced
table.m[, `Contrast±95%CIs` := paste0(estimate, "±[", ci.lb, ",", ci.ub, "]")]
table.m[, df := paste(intercept_df, contrast_df, sep=",")]
setnames(table.m, 
         c("statistic"), 
         c("t-value"))

#
table.m[, random_effect := gsub("list\\(", "", random_effect)]
table.m[, random_effect := gsub(")", "", random_effect)]
# table.m[, random_effect := paste0("random effect=", random_effect)]
table.m
#
# Get sample size by actual group...
sub.dat <- c()
Ns <- c()
var <- c()
i <- 1

for(i in 1:nrow(table.m)){
  m <- readRDS(table.m[i, ]$model_path_nativeness)
  sub.dat <- m$data
  Ns <- sub.dat[, .(n = .N, refs = uniqueN(Citation)),
          by = c(table.m[i, ]$nativeness_var)]
  setnames(Ns, table.m[i, ]$nativeness_var, "nativeness_var")
  Ns[, string := paste0(nativeness_var, "=", n, "(", refs, ")")]
  # sort...(annoying)
  Ns[, order := ifelse(nativeness_var %in% c("Intact_Africa", "Native"),
                       1, 2)]
  setorder(Ns, order)
  Ns <- Ns[, .(string = paste(string, collapse = ", "))]
  Ns
  table.m[i, N_string := Ns$string]
  
}

table.m


table.m <- table.m[, .(analysis_group_category, analysis_group,
                       nativeness_var, random_effect,
                       model_comparison_string,
                       fit_string1, fit_string2, fit_string3, N_string, 
                       `Contrast±95%CIs`, df, `t-value`, p.value)]
table.m


# Set order
table.m[, nativeness_order := fcase(nativeness_var == "Herbivore_nativeness", 1,
                                    nativeness_var == "Invasive", 2,
                                    nativeness_var == "Africa_Comparison", 3)]


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
setdiff(table.m$analysis_group, lvls)

table.m$analysis_group <- factor(table.m$analysis_group,
                                 levels = lvls)

table.m[, analysis_group_order := as.numeric(analysis_group)]
table.m[, analysis_group := gsub("_", " ", analysis_group)]
table.m[, analysis_group := gsub("Invert ", " ", analysis_group)]
table.m[, analysis_group := gsub("Vert Carn ", "Carnivore ", analysis_group)]
table.m[, analysis_group := gsub("Vert Herb ", "Herbivore ", analysis_group)]
unique(table.m$analysis_group)
table.m[, analysis_group := trimws(analysis_group)]

setorder(table.m, analysis_group_category, analysis_group_order, nativeness_order)

table.m[, nativeness_var := gsub("_", " ", nativeness_var)]
table.m[, N_string := gsub("_", " ", N_string)]

table.m[, random_effect := gsub("data_point_ID", "Observation ID", random_effect)]
table.m[, random_effect := gsub("species_response_tag", "Species ID", random_effect)]
table.m[, random_effect := gsub("time_series_clean", "Time Series", random_effect)]
table.m[, random_effect := gsub("experiment_id", "Experiment ID", random_effect)]

# Add empty rows to make it easier to clean this up in Excel...
nrow(table.m)
# table.m[, scaffold := seq(from=1, to=(69*6), by = 6)]
# 
# table.m2 <- merge(data.table(scaffold = seq(from = 1, to = max(table.m$scaffold))),
#                   table.m,
#                   by = "scaffold",
#                   all.x = T)
# table.m[, random_effect := paste("Random effect:", random_effect)]
# table.m[, N_string := paste("N:", N_string)]


# table.m[, `Contrast±95%CIs` := paste("Contrast±95%CIs:", `Contrast±95%CIs`)]

table.m[, `Contrast±95%CIs` := paste0(`Contrast±95%CIs`, ", df=", df, ", t=", `t-value`, ", p=", p.value)]

table.m

# Going to write this to csv real quick to see what this table can look like...
# fwrite(table.m[, !c("nativeness_order", "analysis_group_order", "df", "t-value", "p.value"), with = F], 
#        na = "",
#        "figures/revision/supplement/Table S1.csv")

# Let's try melting this to make formatting easier
# table.m[, spacer := ""]
# table.m

table.m.mlt <- melt(table.m[, !c("df", "t-value", "p.value")],
                    measure.vars = c("N_string", "random_effect",
                                     "model_comparison_string",
                                     "fit_string1", "fit_string2", "fit_string3", "Contrast±95%CIs"))

unique(table.m.mlt$variable)
table.m.mlt$variable <- factor(table.m.mlt$variable, 
                               levels = c( "N_string", "random_effect","model_comparison_string", 
                                           "fit_string1", "fit_string2", "fit_string3",
                                          "Contrast±95%CIs"))
table.m.mlt[, variable_ord := as.numeric(variable)]
table.m.mlt

setorder(table.m.mlt, analysis_group_order,
         nativeness_order, variable_ord)

table.m.mlt



# >>> Final -------------------------------------------------------------
table.m.mlt

table.m.mlt.cst <- dcast(table.m.mlt[, !c("variable_ord")],
                         ... ~ variable, value.var = "value")

table.m.mlt.cst

gt_table <- table.m.mlt.cst |>
  mutate(fit_string1 = gsub("sigma2_null", "$\u03C3^2_{null}$", fit_string1)) |>
  mutate(fit_string1 = gsub("sigma2_nativeness", "$\u03C3^2_{model}$", fit_string1)) |>
  mutate(fit_string2 = gsub("I2_null", "$I^2_{null}$", fit_string2)) |>
  mutate(fit_string2 = gsub("I2_nativeness", "$I^2_{model}$", fit_string2)) |>
  mutate(fit_string3 = gsub("R2*", "$R^2$*", fit_string3, fixed = TRUE)) |>
  mutate(fit_string3 = gsub("R2", "$R^2$", fit_string3, fixed = TRUE)) |>
  mutate(`Contrast±95%CIs` = gsub("Contrast±95%CIs: ", "", `Contrast±95%CIs`)) |>
  mutate(random_effect = gsub("Random effect: ", "", random_effect)) |>
  mutate(model_comparison_string = gsub("LRT=", "", model_comparison_string)) |>
  mutate(model_comparison_string = gsub("p=", "", model_comparison_string)) |>
  mutate(model_comparison_string = gsub("p<", "", model_comparison_string)) |>
  mutate(N_string = gsub("N: ", "", N_string)) |>
  # mutate(`Contrast±95%CIs` = gsub(", ", "<br>", `Contrast±95%CIs`)) |>
  # mutate(fit_string1 = gsub(", ", "<br>", fit_string1)) |>
  relocate(N_string, random_effect, model_comparison_string, `Contrast±95%CIs`) |>
  dplyr::select(-nativeness_order, -analysis_group_order) |>
  group_by(analysis_group_category, analysis_group) |>
  gt(rowname_col = "nativeness_var") |>
  # cols_hide(columns = c("analysis_group_category", "nativeness_var",
  #                       "analysis_group")) |>
  # tab_header(title="Table S1") |>
  tab_header(#title = ,
    md("**Table S1**. Final model results and statistics for all main text models. 
              Sample sizes are given as number of observations with number of studies in parentheses. 
              Log-likelihood ratio tests (LRT) and p values in comparison to intercept-only null models are provided. 
              Total model variance ($\u03C3^2$), total unexplained heterogeneity ($I^2$) and $R^2$ are provided. 
              Note that some some models had extremely low heterogeneity (particularly Root Biomass), leading to 
              $R^2$ of 100%. We thus calculated an alternative $R^2$*, which is the proportion of total variance
              in $y$ explained by moderator (see Methods). Contrasts between factors (e.g., nativeness levels) are provided
              along with 95% confidence intervals, degrees of freedom, and p-values.")) |>
  opt_align_table_header(align = c("left")) |>
  cols_label(N_string = md("N articles<br/>(n observations)"),
             random_effect = "Random effect",
             model_comparison_string = "Comparison to intercept-only (LRT, p)",
             fit_string1 = "Total variance",
             fit_string2 = "Unexplained heterogeneity",
             fit_string3 = "Explained variance") |>
  fmt_markdown(fit_string1) |>
  fmt_markdown(fit_string2) |>
  fmt_markdown(fit_string3) |>
  tab_options(table.font.size = px(7),
              column_labels.font.size = px(10),
              heading.title.font.size = px(12))
# fmt_markdown(fit_string3) 

gt_table

gtsave(gt_table, "figures/revision/supplement/Table S1 Model results.pdf")
# library("kableExtra")
# gt_table %>%
#   kable(format = "latex", booktabs = TRUE) %>%
#   kableExtra::landscape()

# kable::kableExtra()
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# R2/I2/variance figure ------------------------------------------------
tertiary_palette <- c("Herbivore_nativeness" = "#57b7db",
                      "Invasive" = "#faae6b", 
                      "Africa_Comparison" = "#d7a4a3")
labs <- c("Herbivore_nativeness" = "Nativeness",
          "Invasive" =  "Invasiveness",
          "Africa_Comparison" = "Africa comparison")

lvls
levels(sub_guide$analysis_group)
sub_guide$analysis_group <- factor(sub_guide$analysis_group, lvls)

sub_guide[nativeness_var != "Africa_Comparison"]

unique(sub_guide$analysis_group_category)
sub_guide$nativeness_var <- factor(sub_guide$nativeness_var,
                                   levels = c("Herbivore_nativeness", "Invasive", "Africa_Comparison"))

sub_guide.long <- melt(sub_guide,
                       measure.vars = c("prop_variance_reduced", "I2_star", "i2_nativeness", "R2_cond"))

sub_guide.long

facet_lab <- as_labeller(c("Herbivore_nativeness" = "Nativeness",
                           "Invasive" =  "Invasiveness",
                           "Africa_Comparison" = "Africa comparison"))

library("latex2exp")

sub_guide.long[analysis_group == "Root_Biomass"]

# >>> Prop variance reduced -----------------------------------------------

min(sub_guide.long[value < 0, ]$value)

p1 <- ggplot(data = sub_guide.long[analysis_group_category == "Vertebrates" & variable == "prop_variance_reduced"], 
       aes(x = value, y = analysis_group, fill = nativeness_var))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Proportional reduction in total variance $(\u03C3^2_{intercept-only} - \u03C3^2_{model}) / \u03C3^2_{intercept-only}$"))+
  facet_wrap(~nativeness_var, scales = "free_y",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 1))+
  theme_lundy#+
p1

p2 <- ggplot(data = sub_guide.long[analysis_group_category == "Invertebrates" & variable == "prop_variance_reduced"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Invertebrates")+
  xlab(TeX("Proportional reduction in total variance $(\u03C3^2_{intercept-only} - \u03C3^2_{model}) / \u03C3^2_{intercept-only}$"))+
  facet_wrap(~nativeness_var, scales = "free_y",
             labeller = facet_lab)+  
  coord_cartesian(xlim = c(0, 1))+
  theme_lundy
p2

p3 <- ggplot(data = sub_guide.long[analysis_group_category == "Ecosystem" & variable == "prop_variance_reduced"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Ecosystem")+
  xlab(TeX("Proportional reduction in total variance $(\u03C3^2_{intercept-only} - \u03C3^2_{model}) / \u03C3^2_{intercept-only}$"))+
  facet_wrap(~nativeness_var, scales = "free_y",
             labeller = facet_lab)+  
  coord_cartesian(xlim = c(0, 1))+
  theme_lundy
p3

sub_guide.long[variable == "prop_variance_reduced", .(n = uniqueN(analysis_group)),
               by = .(analysis_group_category)]
length(unique(sub_guide.long$analysis_group))

library("Cairo")

p1 + p2 + p3 + plot_layout(guides = "collect",
                           nrow = 3,
                           heights = c(6/32, 5/32, 21/32)) & 
  theme(legend.position = "bottom")
ggsave("figures/revision/supplement/Proportional variance reduction.pdf", device = cairo_pdf,
       width = 10, height = 9)

ggsave("figures/revision/supplement/Proportional variance reduction.png", dpi = 300, #device = cairo_pdf,
       width = 10, height = 9)

# >>> I2 ------------------------------------------------------------------

p1 <- ggplot(data = sub_guide.long[analysis_group_category == "Vertebrates" & variable == "i2_nativeness"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Total unexplained heterogeneity $(I^2)$"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p1

p2 <- ggplot(data = sub_guide.long[analysis_group_category == "Invertebrates" & variable == "i2_nativeness"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Total unexplained heterogeneity $(I^2)$"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p2

p3 <-ggplot(data = sub_guide.long[analysis_group_category == "Ecosystem" & variable == "i2_nativeness"], 
            aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Total unexplained heterogeneity $(I^2)$"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p3

sub_guide.long[variable == "i2_nativeness", .(n = uniqueN(analysis_group)),
               by = .(analysis_group_category)]
length(unique(sub_guide.long$analysis_group))

library("Cairo")

p1 + p2 + p3 + plot_layout(guides = "collect",
                           nrow = 3,
                           heights = c(6/32, 5/32, 21/32)) & 
  theme(legend.position = "bottom")
ggsave("figures/revision/supplement/i2.pdf", device = cairo_pdf,
       width = 10, height = 9)

ggsave("figures/revision/supplement/i2.png", dpi = 300,#device = cairo_pdf,
       width = 10, height = 9)


# >>> Conditional R2 ------------------------------------------------------------------

p1 <- ggplot(data = sub_guide.long[analysis_group_category == "Vertebrates" & variable == "R2_cond"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Conditional $R^{2}$"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p1

p2 <- ggplot(data = sub_guide.long[analysis_group_category == "Invertebrates" & variable == "R2_cond"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Conditional $R^{2}$"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p2

p3 <-ggplot(data = sub_guide.long[analysis_group_category == "Ecosystem" & variable == "R2_cond"], 
            aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Conditional $R^{2}$"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p3

sub_guide.long[variable == "R2_cond", .(n = uniqueN(analysis_group)),
               by = .(analysis_group_category)]
length(unique(sub_guide.long$analysis_group))

library("Cairo")

p1 + p2 + p3 + plot_layout(guides = "collect",
                           nrow = 3,
                           heights = c(6/32, 5/32, 21/32)) & 
  theme(legend.position = "bottom")
ggsave("figures/revision/supplement/conditional R2.pdf", device = cairo_pdf,
       width = 10, height = 9)

ggsave("figures/revision/supplement/conditional R2.png", dpi = 300, 
       width = 10, height = 9)

# >>> I2/R2* ------------------------------------------------------------------

p1 <- ggplot(data = sub_guide.long[analysis_group_category == "Vertebrates" & variable == "I2_star"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Conditional $R^{2}$*"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p1

p2 <- ggplot(data = sub_guide.long[analysis_group_category == "Invertebrates" & variable == "I2_star"], 
             aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Conditional $R^{2}$*"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p2

p3 <-ggplot(data = sub_guide.long[analysis_group_category == "Ecosystem" & variable == "I2_star"], 
            aes(x = value, y = analysis_group, fill = nativeness_var))+
  # geom_vline(xintercept = 0, linetype = "dashed")+
  geom_col(position = "dodge")+
  scale_fill_manual("Model", values = tertiary_palette, labels = labs)+
  scale_y_discrete(labels = group_labels)+
  ylab("Vertebrates")+
  xlab(TeX("Conditional $R^{2}$*"))+
  facet_wrap(~nativeness_var, scales = "free",
             labeller = facet_lab)+
  coord_cartesian(xlim = c(0, 100))+
  theme_lundy#+
p3

sub_guide.long[variable == "I2_star", .(n = uniqueN(analysis_group)),
               by = .(analysis_group_category)]
length(unique(sub_guide.long$analysis_group))

library("Cairo")

p1 + p2 + p3 + plot_layout(guides = "collect",
                           nrow = 3,
                           heights = c(6/32, 5/32, 21/32)) & 
  theme(legend.position = "bottom")
ggsave("figures/revision/supplement/conditional R2_star.pdf", device = cairo_pdf,
       width = 10, height = 9)

ggsave("figures/revision/supplement/conditional R2_star.png", dpi = 300,#device = cairo_pdf,
       width = 10, height = 9)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------------
# Distribution of research effort -----------------------------------------

dat
dat.long <- melt(dat,
                 measure.vars = c("Herbivore_nativeness", "Invasive", "Africa_Comparison"),
                 value.name = "Megafauna")
dat.long
dat.long[Megafauna == "Native" & variable == "Invasive", Megafauna := NA]
dat.long[Megafauna == "Africa_Comparison" & variable == "Introduced", Megafauna := NA]


dat.long <- dat.long[!is.na(Megafauna), ]
dat.long


# this is total across all analysis groups.
dat.long[, `:=` (total_references = uniqueN(Citation),
                 total_observations = uniqueN(data_point_ID)),
         by = .(Megafauna)]
dat.long


Ns <- dat.long[analysis_group %in% sub_guide$analysis_group, 
          .(Observations = uniqueN(data_point_ID), References = uniqueN(Citation)),
    by = .(analysis_group, analysis_group_category,
           Megafauna, total_references, total_observations)]

Ns[, observation_percent := Observations / total_observations * 100]
Ns[, references_percent := References / total_references * 100]

Ns

# 
# Ns.wide <- dcast(Ns, ... ~ Herbivore_nativeness,
#                  value.var = c("n", "refs"),
#                  fill = 0)
# 
# Ns.wide
# 
# setorder(Ns.wide, refs_Introduced)
# Ns.wide

Ns.long <- melt(Ns,
                measure.vars = c("observation_percent", "references_percent"),
                variable.name = "N_type",
                value = "Percent")

Ns.long[analysis_group == "Invert_Detritivore_Abundance", ]

Ns.long$analysis_group <- factor(Ns.long$analysis_group ,
                                   levels = lvls)
Ns.long

setorder(Ns.long, Megafauna, -Percent)

Ns.long[, head(.SD, 3), by = .(Megafauna)]


Ns.long[analysis_group == "Bare_Ground" & N_type == "references_percent"]
