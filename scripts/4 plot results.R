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
rm(list = ls())
gc()

library("groundhog")
groundhog.day <- "2024-07-15"

libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "shades", "patchwork", "dplyr",
          "scico", "beepr",
          "plotly", "nlme", 
          "ggtext", "ggstance")
groundhog.library(libs, groundhog.day)

# >>> Helper functions  -------------------------------------

getVars <- function(formula_str){
  formula_str <- gsub("yi_smd ~ ", "", formula_str)
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

dat <- fread("data/master_data.csv")

master_guide <- fread("outputs/main_text/summaries/model_comparison_table.csv")
master_guide

posthocs <- fread("outputs/main_text/summaries/posthoc_comparisons.csv")

sub_guide <- master_guide[preferred_model == "yes", ]
posthocs <- posthocs[preferred_model == "yes", ]

m <- readRDS(sub_guide[analysis_group == "Small_Mammal_Abundance" & nativeness_var == "Herbivore_nativeness", ]$model_path_nativeness)
m

rma_predictions(m, 
                newgrid = data.table(Herbivore_nativeness = c("Native", "Introduced")),
                has_intercept = T)

# *** Set plotting constants ----------------------------------------------

tertiary_palette <- c("Introduced" = "#57b7db",
                      "Invasive" = "#faae6b", 
                      "Native" = "#a7928c")

africa_palette <- c("Introduced" = "#57b7db",
                    "Intact_Africa" = "#d7a4a3")

theme_lundy <-   theme_bw()+
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        text = element_text(color = "black"),  
        axis.text = element_text(color = "black"), 
        panel.grid = element_blank(),
        panel.border = element_blank())

dat[, wi := 1/sqrt(vi_smd)]
dat[, pt_size := 10 * (wi-min(wi, na.rm=T)) / (max(wi, na.rm = T) - min(wi, na.rm=T)) + 0.01]

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

mods <- load_and_tidy_model_series(sub_guide[nativeness_var == "Herbivore_nativeness", ]$model_path_nativeness,
                                   var = "Herbivore_nativeness",
                                   has_intercept = TRUE)
names(mods) <- sub_guide[nativeness_var == "Herbivore_nativeness", ]$model_id_nativeness

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


mods <- rbindlist(c(mods, mods2, mods3), idcol = "model_id_nativeness")
mods <- merge(mods,
              sub_guide[, .(model_id_nativeness, var, analysis_group, 
                            analysis_group_category, nativeness_var,
                            min_refs,
                            max_refs, min_obs, max_obs)])
mods
setnames(mods, "Herbivore_nativeness", "nativeness_comparison")

unique(mods$analysis_group)
unique(mods$nativeness_comparison)

unique(mods$analysis_group)

# Melt dataset so the 3 comparison columns are in 1 column:
sub_dat.mlt <- melt(dat[analysis_group %in% sub_guide$analysis_group, 
                            .(data_point_ID, Citation, yi_smd, pt_size,
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

lvls <- c("Primary_Productivity",
          "Dead_Vegetation",
          "Litter_Cover", "Bare_Ground",
          "Soil_Compaction", "Soil_Moisture",
          "Soil_Total_C",
          "Soil_C:N", "Soil_Total_N",
          "Soil_Labile_N", "Soil_Total_P",
          "Soil_Total_Ca", "Soil_Total_Mg",
          
          
          "Invertebrate_Diversity", "Invertebrate_Abundance",
          "Invert_Herbivore_Diversity", "Invert_Herbivore_Abundance",
          "Invert_Predator_Diversity", "Invert_Predator_Abundance",
          "Invert_Detritivore_Abundance",
          
          "Vertebrate_Diversity", "Vertebrate_Abundance",
          "Vert_Herb_Diversity", "Vert_Herb_Abundance",
          "Vert_Carn_Diversity", "Vert_Carn_Abundance",
          "Small_Mammal_Abundance",
          "Bird_Diversity", "Bird_Abundance")
setdiff(mods$analysis_group, lvls)
setdiff(sub_dat.mlt$analysis_group, lvls)

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


unique(mods$nativeness_comparison)
mods$nativeness_comparison <- factor(mods$nativeness_comparison,
                                    levels = rev(c("Native", "Intact_Africa", "Introduced", "Invasive")))

sub_dat.mlt$nativeness_comparison <- factor(sub_dat.mlt$nativeness_comparison,
                                    levels = rev(c("Native", "Intact_Africa", "Introduced", "Invasive")))

N$nativeness_comparison <- factor(N$nativeness_comparison,
                                    levels = rev(c("Native", "Intact_Africa","Introduced", "Invasive")))


unique(mods$analysis_group_category)
mods$analysis_group_category <- factor(mods$analysis_group_category,
                                     levels = rev(c("Ecosystem", "Invertebrates", "Vertebrates")))

sub_dat.mlt$analysis_group_category <- factor(sub_dat.mlt$analysis_group_category,
                                              levels = rev(c("Ecosystem", "Invertebrates", "Vertebrates")))

N$analysis_group_category <- factor(N$analysis_group_category,
                                    levels = rev(c("Ecosystem", "Invertebrates", "Vertebrates")))


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
  geom_text(data = N[analysis_group_category == "Ecosystem" &
                        nativeness_var != "Africa_Comparison"],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[analysis_group_category == "Ecosystem" &
                                   nativeness_var != "Africa_Comparison"], 
              aes(y = yi_smd, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[analysis_group_category == "Ecosystem" &
                              nativeness_var != "Africa_Comparison"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                     x = analysis_group,
                     group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .25)+
  geom_pointrange(data = mods[analysis_group_category == "Ecosystem" &
                                nativeness_var != "Africa_Comparison"], 
                  aes(y = pred, ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
                  size = .75)+
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
              aes(y = yi_smd, x = analysis_group, 
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
                width = .25)+
  geom_pointrange(data = mods[analysis_group_category == "Invertebrates" &
                                nativeness_var != "Africa_Comparison"], 
                  aes(y = pred, ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
                  size = .75)+
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
              aes(y = yi_smd, x = analysis_group, 
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
                width = .25)+
  geom_pointrange(data = mods[analysis_group_category == "Vertebrates" &
                                nativeness_var != "Africa_Comparison"], 
                  aes(y = pred, ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
                  size = .75)+
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
              aes(y = yi_smd, x = analysis_group, 
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
                width = .25)+
  geom_pointrange(data = mods[nativeness_var == "Africa_Comparison" &
                                analysis_group_category == "Vertebrates"], 
                  aes(y = pred, ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
                  size = .75)+
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
              aes(y = yi_smd, x = analysis_group, 
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
                width = .25)+
  geom_pointrange(data = mods[nativeness_var == "Africa_Comparison" &
                                analysis_group_category == "Invertebrates"], 
                  aes(y = pred, ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
                  size = .75)+
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
                        analysis_group_category == "Ecosystem"],
             aes(y = -6.5, x = analysis_group,
                 color = nativeness_comparison,
                 group = nativeness_comparison,
                 label = n_lab),
             label.size = NA, size = 2.5,
             position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[nativeness_var == "Africa_Comparison" &
                                   analysis_group_category == "Ecosystem"], 
              aes(y = yi_smd, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[nativeness_var == "Africa_Comparison" &
                              analysis_group_category == "Ecosystem"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                    x = analysis_group,
                    group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .25)+
  geom_pointrange(data = mods[nativeness_var == "Africa_Comparison" &
                                analysis_group_category == "Ecosystem"], 
                  aes(y = pred, ymin = pi.lb, ymax = pi.ub, 
                      fill = nativeness_comparison,
                      group = nativeness_comparison,
                      x = analysis_group),
                  shape = 21, position = position_dodge(width = .66),
                  size = .75)+
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

posthocs <- fread("outputs/main_text/summaries/posthoc_comparisons.csv")

posthocs <- posthocs[preferred_model == "yes", ]

posthocs

unique(posthocs$contrast)

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
  geom_errorbar(data = posthocs[analysis_group_category == "Ecosystem" &
                                  nativeness_var != "Africa_Comparison"],
                aes(y = analysis_group,
                    xmin = ci.lb, xmax = ci.ub,
                    group = contrast),
                width = .25,
                position = position_dodgev(height = .75),
                shape = 21)+
  geom_point(data = posthocs[analysis_group_category == "Ecosystem" &
                               nativeness_var != "Africa_Comparison"],
             aes(x = estimate, y = analysis_group,
                 fill = contrast),
             size = 3,
             position = position_dodgev(height = .75),
             shape = 21)+
  geom_text(data = posthocs[analysis_group_category == "Ecosystem" &
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
  coord_cartesian(xlim = c(-2, 2), clip = "off")+
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
ggsave("figures/main_text/Fig 1A-D raw.pdf", width = 10, height = 8)

#
eco + p.post.eco + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank()) +
  plot_annotation(tag_levels = "A")

ggsave("figures/main_text/Fig 2A & B raw.pdf", width = 10, height = 8)


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
  plot_layout(ncol = 2, heights = c(4/12, 3/12, 5/12), widths = c(.66, .33))+
  plot_annotation(tag_levels = "A")

ggsave("figures/main_text/Fig 3 raw.pdf", width = 10, height = 8)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Text summaries ------------------------------------------------------

sub_guide[LRT_pval < 0.05 & nativeness_var != "Africa_Comparison", ]

posthocs[p.value < 0.05 & nativeness_var != "Africa_Comparison", ]


# ------------  All animals ------------------------------------------------!
unique(sub_guide$analysis_group_category)

length(unique(sub_guide$analysis_group))

posthocs[analysis_group_category != "Ecosystem" &
           p.value < 0.05 
         & nativeness_var != "Africa_Comparison", ]

unique(sub_guide[analysis_group_category != "Ecosystem" 
                 & nativeness_var != "Africa_Comparison"]$analysis_group)

range(posthocs[analysis_group_category != "Ecosystem"
               & nativeness_var != "Africa_Comparison", ]$statistic)

range(posthocs[analysis_group_category != "Ecosystem"
               & nativeness_var != "Africa_Comparison", ]$contrast_df)

range(posthocs[analysis_group_category != "Ecosystem"
               & nativeness_var != "Africa_Comparison", ]$p.value)

range(sub_guide[analysis_group_category != "Ecosystem"
                & nativeness_var != "Africa_Comparison", ]$LRT)
range(sub_guide[analysis_group_category != "Ecosystem"
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


# >>> Overall effects from intercept-only models --------------------------
paths <- sub_guide[nativeness_var == "Herbivore_nativeness"
                   & nativeness_var != "Africa_Comparison", ]$model_path_null

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

names(intercepts) <- sub_guide[nativeness_var == "Herbivore_nativeness"
                               & nativeness_var != "Africa_Comparison", ]$analysis_group

intercepts <- rbindlist(intercepts, idcol = "analysis_group")
intercepts <- merge(intercepts,
                    sub_guide[nativeness_var == "Herbivore_nativeness"
                              & nativeness_var != "Africa_Comparison", .(analysis_group, nativeness_var,
                                                                          analysis_group_category)],
                    by = "analysis_group")

intercepts

intercepts[analysis_group_category == "Vertebrates" & 
             nativeness_var != "Africa_Comparison" &
             pval < 0.05 &
             !analysis_group %in% c("Mamm_SmallHerb_Abundance"), ]

intercepts[analysis_group_category == "Invertebrates" & pval < 0.05 
           & nativeness_var != "Africa_Comparison", ]
range(intercepts[analysis_group_category == "Invertebrates"
                 & nativeness_var != "Africa_Comparison"]$pred)
range(intercepts[analysis_group_category == "Invertebrates"
                 & nativeness_var != "Africa_Comparison"]$pval)

intercepts[analysis_group_category == "Ecosystem" & 
             nativeness_var != "Africa_Comparison"& pval < 0.05, ]
round(intercepts[analysis_group_category == "Ecosystem" & 
                   nativeness_var != "Africa_Comparison" & pval < 0.05, ]$pval, 5)

