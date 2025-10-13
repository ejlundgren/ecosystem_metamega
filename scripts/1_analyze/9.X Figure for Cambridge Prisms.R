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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# Figure -------------------------------------------------

unique(N$analysis_group)
sub_grps <- c("Vertebrate_Abundance", "Small_Mammal_Abundance", "Litter_Cover", 
              "Soil_Compaction")

#
ggplot()+
  geom_hline(yintercept = 0, color = "grey50", linetype = "dashed")+
  geom_text(data = N[analysis_group %in% sub_grps & nativeness_var == "Herbivore_nativeness" &
                       nativeness_var != "Africa_Comparison"],
            aes(y = -6.5, x = analysis_group,
                color = nativeness_comparison,
                group = nativeness_comparison,
                label = n_lab),
            label.size = NA, size = 2.5,
            position = position_dodge(width = .66))+
  geom_jitter(data = sub_dat.mlt[analysis_group %in% sub_grps & nativeness_var == "Herbivore_nativeness" &
                                   nativeness_var != "Africa_Comparison"], 
              aes(y = yi_smd, x = analysis_group, 
                  fill = nativeness_comparison,
                  group = nativeness_comparison,
                  size = pt_size),
              shape = 21, alpha = .25, 
              position = position_jitterdodge(dodge.width = 0.6,
                                              jitter.width = 0.25))+
  geom_errorbar(data = mods[analysis_group %in% sub_grps & nativeness_var == "Herbivore_nativeness" &
                              nativeness_var != "Africa_Comparison"], 
                aes(ymin = ci.lb, ymax = ci.ub, 
                    x = analysis_group,
                    group = nativeness_comparison),
                position = position_dodge(width = .66),
                width = .25)+
  geom_pointrange(data = mods[analysis_group %in% sub_grps & nativeness_var == "Herbivore_nativeness" &
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
  coord_cartesian(ylim = c(-5, 5),
             clip = "off")
ggsave("../../Cambridge_Prisms_Special_Issue/figures/meta subset.png")
