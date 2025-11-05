
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------

# Correct publication bias? Maybe not.--------------------------------------------

# Yang et al. https://besjournals.onlinelibrary.wiley.com/doi/pdf/10.1111/2041-210x.14377

original <- readRDS(sub_guide[analysis_group == "Vertebrate_Diversity" &
                                nativeness_var == "Herbivore_nativeness", ]$model_path_nativeness)

# typical orchard plot
# Correct for publication bias, apparently...:
VCV <- vcalc(vi = vi,
             cluster = Citation, 
             rho = 0.5, 
             obs = data_point_ID, 
             data = dat[eval(parse(text = sub_guide[analysis_group == "Vertebrate_Diversity" &
                                                      nativeness_var == "Herbivore_nativeness", ]$exclusion))]) 

# Apparently this model is corrected for publication bias:
mod_MLFE <- rma.mv(yi = yi, 
                   V = VCV, 
                   method = "REML", 
                   mods = ~Herbivore_nativeness,
                   test = "t", 
                   dfs = "contain", 
                   data = dat[eval(parse(text = sub_guide[analysis_group == "Vertebrate_Diversity" &
                                                            nativeness_var == "Herbivore_nativeness", ]$exclusion))])
summary(mod_MLFE)
summary(original)
# robust(mod_MLFE, cluster = "Citation", clubSandwich = TRUE)

plot <- orchard_plot(original, mod = "1", 
                     group = "Citation", 
                     xlab = "Standardised mean difference (SMD)") + 
  scale_x_discrete(labels = "Population mean effect estimate") + 
  ylim(-4,4)

# visualize the model estimates of the proposed two-step approach as the sensitivity analyses
pub_bias_plot(plot, mod_MLFE) +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(size = 12, color = "black"),
        axis.title.x = element_text(size = 12, color = "black"),
        axis.text.y = element_text(size = 12, color = "black"))


##
##
##
##

mod_MLFE <- rma.mv(yi = yi, 
                   V = VCV, 
                   mods = ~Africa_Comparison,
                   method = "REML", 
                   test = "t", 
                   dfs = "contain", 
                   data = dat[eval(parse(text = sub_guide[i, ]$exclusion))])
summary(mod_MLFE)
summary(original)

original <- readRDS(sub_guide[i, ]$model_path_nativeness)

# typical orchard plot
plot <- orchard_plot(original, mod = "Africa_Comparison", 
                     group = "Citation", 
                     xlab = "Standardised mean difference (SMD)") +
  ylim(-4,4)
plot

# visualize the model estimates of the proposed two-step approach as the sensitivity analyses
# pub_bias_plot(plot, mod_MLFE) +
#   theme(panel.grid = element_blank(),
#         axis.text.x = element_text(size = 12, color = "black"),
#         axis.title.x = element_text(size = 12, color = "black"),
#         axis.text.y = element_text(size = 12, color = "black"))



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------

# DEPRECATED --------------------------------------------------------------

# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------
# #  Manually make funnel plot -------------------------------------------
# 
# ms <- lapply(sub_guide[analysis_group_category == "Vertebrates", ]$model_path_nativeness,
#              readRDS)
# names(ms) <- sub_guide[analysis_group_category == "Vertebrates", ]$label
# names(ms)
#  
# # >>> Encapsulate ---------------------------------------------------------
# 
# prepare_funnels <- function(m,
#                             residuals_or_effects = "residuals"){
#   
#   tab <- m |>
#     tidy()
#   tab
#   setDT(tab)
#   
#   var <- getVars(m$formula.mods)
#   
#   tab[, variable := gsub(var, "", term)]
#   #
#   if("" %in% tab$variable){
#     tab[variable == "", variable := var]
#   }
#   
#   sub.dat <- m$data
#   
#   if(residuals_or_effects == "effects"){
#     sub.dat$response <- sub.dat$yi_smd
#   }else{
#     sub.dat$response <- residuals(m)
#   }
#   #
#   manifold <- CJ(se = seq(0, max(sqrt(sub.dat$vi)), by = 0.01),
#                  variable = tab$variable)
#   manifold
#   
#   manifold.m1 <- merge(manifold,
#                        tab[, .(variable, estimate, std.error)],
#                        by = "variable")
#   setnames(manifold.m1, "std.error", "est_se")
#   nrow(manifold) == nrow(manifold.m1) # must be TRUE
#   
#   if(residuals_or_effects != "effects"){
#     
#     manifold.m1[, estimate := 0]
#     
#   }
#   #
#   manifold.m1[, ll95 := estimate - (1.96 * se)]
#   manifold.m1[, ul95 := estimate + (1.96 * se)]
#   manifold.m1[, ll99 := estimate - (3.29 * se)]
#   manifold.m1[, ul99 := estimate + (3.29 * se)]
#   manifold.m1[, mean_ll95 := estimate - (1.96 * est_se)]
#   manifold.m1[, mean_ul95 := estimate + (1.96 * est_se)]
#   
#   lines <- melt(manifold.m1[, !c("est_se")], 
#                 id.vars = c("variable", "se"),
#                 variable.name = "linetype")
#   unique(lines$linetype)
#   lines[grepl("95", linetype), linetype_simple := "95% CI"]
#   lines[grepl("99", linetype), linetype_simple := "99% CI"]
#   lines[linetype %in% c("mean_ll95", "mean_ul95"), linetype_simple := "95% CI of estimate"]
#   lines[linetype %in% c("estimate"), linetype_simple := "Model estimate"]
#   unique(lines$linetype_simple)
#   
#   #
#   sub.dat[, se := sqrt(vi)]
#   setnames(sub.dat, var, "variable")
#   # 
#   #
#   lines[, max_se := max(se)]
#   
#   model_est <- lines[linetype_simple %in% c("95% CI of estimate",
#                                             "Model estimate") &
#                        se == 0]
#   
#   model_est <- dcast(model_est, 
#                      variable + se + max_se ~ linetype,
#                      value.var = "value")
#   model_est[, se := 0-(max_se * .05)]
# 
#   # 
#   return(list(lines, model_est, sub.dat))
# }
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------------
# # Funnel plots ------------------------------------------------------------
# 
# ms <- lapply(sub_guide$model_path_nativeness,
#              readRDS)
# names(ms) <- sub_guide$model_id_nativeness
# names(ms)
# 
# out <- lapply(ms, prepare_funnels, residuals_or_effects = "residuals")
# names(out) <- names(ms)
# 
# # ---- Format lines --------------------------------------!
# lines <- lapply(out, "[[", 1)
# names(lines) <- names(out)
# lines
# 
# lines.dat <- rbindlist(lines, idcol = "model_id_nativeness")
# lines.dat
# lines.dat <- merge(lines.dat,
#                    sub_guide[, .(model_id_nativeness, analysis_group_category, 
#                                  analysis_group,
#                                  nativeness_var)])
# lines.dat
# 
# # unique(lines.dat$final_label)
# unique(lines.dat$variable)
# lines.dat[grepl("Invasive", nativeness_var) & variable == "intercept",
#           variable := "Invasive"]
# lines.dat[grepl("nativeness", nativeness_var) & variable == "intercept",
#           variable := "Introduced"]
# lines.dat[grepl("Africa", nativeness_var)]
# lines.dat[grepl("Africa", nativeness_var) & variable == "intercept",
#           variable := "African assemblage"]
# 
# lines.dat[is.na(variable)]
# 
# unique(lines.dat$variable)
# unique(lines.dat$nativeness_var)
# lines.dat[variable == "intercept", ]
# # ---- Format model estimates --------------------------------------!
# 
# model_ests <- lapply(out, "[[", 2)
# names(model_ests) <- names(out)
# model_ests
# 
# model_ests <- rbindlist(model_ests, idcol = "model_id_nativeness")
# model_ests
# 
# model_ests
# model_ests <- merge(model_ests,
#                     sub_guide[, .(model_id_nativeness, analysis_group_category, 
#                                   analysis_group,
#                                   nativeness_var)])
# model_ests
# 
# model_ests[grepl("Invasive", nativeness_var) & variable == "intercept",
#            variable := "Invasive"]
# model_ests[grepl("nativeness", nativeness_var) & variable == "intercept",
#            variable := "Introduced"]
# model_ests[grepl("Africa", nativeness_var) & variable == "intercept",
#           variable := "African assemblage"]
# unique(model_ests$variable)
# model_ests[variable == "intercept"]
# 
# 
# # ---- Format model data --------------------------------------!
# 
# dat <- lapply(out, "[[", 3)
# names(dat) <- names(out)
# dat <- rbindlist(dat, idcol = "model_id_nativeness", fill = T)
# 
# unique(dat$variable)
# dat <- dat[, .(Citation, variable, response, se, model_id_nativeness)]
# 
# dat <-  merge(dat,
#               sub_guide[, .(model_id_nativeness, analysis_group_category,  analysis_group,
#                             nativeness_var)])
# dat
# 
# unique(dat$variable)
# dat[variable == "Intact_Africa", variable := "African assemblage"]
# 
# # >>> Prepare to plot ----------------------------------------------
# # Facet wrapping is not going to work.
# dat[, max_g := max(abs(response)),
#     by = .(analysis_group)]
# Ns <- dat[, .(n_refs = uniqueN(Citation)),
#           by = .(variable,
#                  analysis_group, analysis_group_category,
#                  max_g, model_id_nativeness)]
# Ns
# 
# unique(dat$variable)
# unique(model_ests$variable)
# unique(lines.dat$variable)
# 
# dat$variable <- factor(dat$variable,
#                        levels = c("Native", "African assemblage", "Introduced", "Invasive"))
# model_ests$variable <- factor(model_ests$variable,
#                               levels = c("Native", "African assemblage", "Introduced", "Invasive"))
# lines.dat$variable <- factor(lines.dat$variable,
#                              levels = c("Native","African assemblage", "Introduced", "Invasive"))
# Ns$variable <- factor(Ns$variable,
#                       levels = c("Native", "African assemblage", "Introduced", "Invasive"))
# 
# quaternary_palette <- c("Introduced" = "#57b7db", "African assemblage" = "#d7a4a3",  
#                         "Invasive" = "#faae6b", "Native" = "#a7928c")
# 
# lvls <- c("Primary_Productivity",
#           "Aboveground_Primary_Productivity",
#           "Dead_Vegetation",
#           "Litter_Cover", "Bare_Ground",
#           "Soil_Compaction", "Soil_Moisture",
#           "Soil_Temperature", 
#           "Soil_Respiration", "CO2_Respiration",
#           "Soil_Decomposition_Rate",
#           "Root_Biomass",
#           "Soil_Organic_Matter",
#           "Soil_Organic_C", 
#           "Soil_Total_C",
#           "Soil_C:N", "Soil_Total_N", "Soil_Temperature",
#           "Soil_Labile_N", "Soil_Total_P",
#           "Soil_Total_Ca", "Soil_Total_Mg", "Soil_K",
#           "Soil_pH", "Microbe_Abundance", "Fungi_Abundance",
#           
#           "Plant_C:N", "Plant_C", "Plant_N", 
#           
#           "Invertebrate_Diversity", "Invertebrate_Abundance",
#           "Invert_Herbivore_Diversity", "Invert_Herbivore_Abundance",
#           "Invert_Predator_Diversity", "Invert_Predator_Abundance",
#           "Invert_Detritivore_Abundance",
#           
#           "Vertebrate_Diversity", "Vertebrate_Abundance",
#           "Vert_Herb_Diversity", "Vert_Herb_Abundance",
#           "Vert_Carn_Diversity", "Vert_Carn_Abundance",
#           "Mammal_Abundance", "Mammal_Diversity",
#           "Small_Mammal_Abundance", "Mamm_SmallHerb_Abundance",
#           "TerrestrialBird_Abundance",
#           "Bird_Diversity", "Bird_Abundance", "Bird_Carnivore_Abundance",
#           "Bird_Omnivore_Abundance", "Herpetofauna_Abundance")
# 
# setdiff(model_ests$analysis_group, lvls)
# lvls <- lvls[lvls %in% model_ests$analysis_group]
# 
# model_ests$analysis_group <- factor(model_ests$analysis_group,
#                                     levels = (lvls))
# dat$analysis_group <- factor(dat$analysis_group,
#                                     levels = (lvls))
# Ns$analysis_group <- factor(Ns$analysis_group,
#                              levels = (lvls))
# lines.dat$analysis_group <- factor(lines.dat$analysis_group,
#                              levels = (lvls))
# 
# group_labels <- unique(model_ests$analysis_group)
# group_labels <- gsub("_", " ", group_labels)
# group_labels <- gsub("Invert ", "", group_labels)
# names(group_labels) <- unique(model_ests$analysis_group)
# group_labels <- gsub("Vert Carn ", "Carnivore ", group_labels)
# group_labels <- gsub("Vert Herb ", "Herbivore ", group_labels)
# 
# group_labels2 <- unique(model_ests$variable) |> as.character()
# names(group_labels2) <- unique(model_ests$variable)
# 
# group_labels <- c(group_labels,
#                   group_labels2)
# # >>> Vertebrates ---------------------------------------------------------
# dat[, key := paste(variable, nativeness_var, sep = "_")]
# unique(lines.dat$variable)
# lines.dat[, key := paste(variable, nativeness_var, sep = "_")]
# model_ests[, key := paste(variable, nativeness_var, sep = "_")]
# 
# unique(model_ests$key)
# unique(lines.dat$key)
# to_plot <- c("African assemblage_Africa_Comparison",
#              "Invasive_Invasive", "Introduced_Herbivore_nativeness",
#              "Native_Herbivore_nativeness")
# 
# dat
# 
# patch <- c()
# grps <- levels(droplevels(dat[analysis_group_category == "Vertebrates"]$analysis_group))
# 
# # Need to have free-er scales than facet-grid will allow us. So making plots
# # in a loop.
# for(i in 1:length(grps)){
#   p <- ggplot()+
#     geom_jitter(data = dat[analysis_group_category == "Vertebrates" &
#                              key %in% to_plot &
#                              analysis_group %in% grps[i], ],
#                 aes(x = se,
#                     y = response, #residuals,
#                     fill = variable),
#                 shape = 21,
#                 alpha = 0.5,
#                 size = 2.5)+
#     geom_line(data = lines.dat[linetype_simple != "95% CI of estimate" &
#                                  analysis_group_category == "Vertebrates" &
#                                  key %in% to_plot &
#                                  analysis_group %in% grps[i], ],
#               aes(x = se, y = value,
#                   color = variable,
#                   linetype = linetype_simple,
#                   group = interaction(linetype_simple, variable, linetype)))+
#     scale_linetype_manual(name = NULL,
#                           values = c("95% CI" = "dashed",
#                                      "99% CI" = "dotted",
#                                      "Model estimate" = "solid"))+
#     # Model estimate:
#     geom_errorbar(data = model_ests[analysis_group_category == "Vertebrates" &
#                                       key %in% to_plot  &
#                                       analysis_group %in% grps[i], ],
#                   aes(ymin = mean_ll95, ymax = mean_ul95,
#                       x = se), 
#                   width = (max(model_ests[analysis_group_category == "Vertebrates"  &
#                                             key %in% to_plot  &
#                                             analysis_group %in% grps[i], ]$max_se) * .2))+
#     geom_point(data = model_ests[analysis_group_category == "Vertebrates"  &
#                                    key %in% to_plot  &
#                                    analysis_group %in% grps[i], ],
#                aes(y = estimate,
#                    x = se, fill = variable),
#                shape = 21, size = 4)+
#     xlab("Standard Error")+
#     ylab("Residuals")+
#     scale_fill_manual(name = NULL,
#                       values = quaternary_palette,
#                       drop = F)+
#     scale_color_manual(name = NULL,
#                        values = quaternary_palette,
#                        drop = F)+
#     facet_grid(variable ~ analysis_group,
#                labeller = as_labeller(group_labels))+
#     theme_bw()+
#     theme(strip.background = element_blank(),
#           plot.title = element_text(hjust = 0.5),
#           strip.text.y=element_blank()
#     )+
#     scale_x_reverse()+
#     coord_flip()
#   
#   if(length(patch) == 0){
#     patch <- p
#   }else{
#     patch <- patch + p
#   }
# }
# 
# patch + plot_layout(ncol = 4, guides = "collect") &
#   theme(legend.position = "bottom")
#   
# ggsave("figures/revision/supplement/Vertebrate funnel plots raw.pdf", width = 14, height = 12, dpi = 300)
# 
# # >>> Invertebrates -------------------------------------------------------
# 
# patch <- c()
# grps <- levels(droplevels(dat[analysis_group_category == "Invertebrates"]$analysis_group))
# 
# # Need to have free-er scales than facet-grid will allow us. So making plots
# # in a loop.
# for(i in 1:length(grps)){
#   p <- ggplot()+
#     geom_jitter(data = dat[analysis_group_category == "Invertebrates" &
#                              key %in% to_plot &
#                              analysis_group %in% grps[i], ],
#                 aes(x = se,
#                     y = response, #residuals,
#                     fill = variable),
#                 shape = 21,
#                 alpha = 0.5,
#                 size = 2.5)+
#     geom_line(data = lines.dat[linetype_simple != "95% CI of estimate" &
#                                  analysis_group_category == "Invertebrates" &
#                                  key %in% to_plot &
#                                  analysis_group %in% grps[i], ],
#               aes(x = se, y = value,
#                   color = variable,
#                   linetype = linetype_simple,
#                   group = interaction(linetype_simple, variable, linetype)))+
#     scale_linetype_manual(name = NULL,
#                           values = c("95% CI" = "dashed",
#                                      "99% CI" = "dotted",
#                                      "Model estimate" = "solid"))+
#     # Model estimate:
#     geom_errorbar(data = model_ests[analysis_group_category == "Invertebrates" &
#                                       key %in% to_plot  &
#                                       analysis_group %in% grps[i], ],
#                   aes(ymin = mean_ll95, ymax = mean_ul95,
#                       x = se), 
#                   width = (max(model_ests[analysis_group_category == "Invertebrates"  &
#                                             key %in% to_plot  &
#                                             analysis_group %in% grps[i], ]$max_se) * .2))+
#     geom_point(data = model_ests[analysis_group_category == "Invertebrates"  &
#                                    key %in% to_plot  &
#                                    analysis_group %in% grps[i], ],
#                aes(y = estimate,
#                    x = se, fill = variable),
#                shape = 21, size = 4)+
#     xlab("Standard Error")+
#     ylab("Residuals")+
#     scale_fill_manual(name = NULL,
#                       values = quaternary_palette,
#                       drop = F)+
#     scale_color_manual(name = NULL,
#                        values = quaternary_palette,
#                        drop = F)+
#     facet_grid(variable ~ analysis_group,
#                labeller = as_labeller(group_labels))+
#     theme_bw()+
#     theme(strip.background = element_blank(),
#           plot.title = element_text(hjust = 0.5),
#           strip.text.y=element_blank()
#     )+
#     scale_x_reverse()+
#     coord_flip()
#   if(length(patch) == 0){
#     patch <- p
#   }else{
#     patch <- patch + p
#   }
# }
# 
# patch + plot_layout(ncol = 2, guides = "collect")
# 
# ggsave("figures/revision/supplement/Invertebrate funnel plots raw.pdf", width = 12, height = 12, dpi = 300)
# 
# # >>> Ecosystem -----------------------------------------------------------
# 
# patch <- c()
# grps <- levels(droplevels(dat[analysis_group_category == "Ecosystem"]$analysis_group))
# 
# # Need to have free-er scales than facet-grid will allow us. So making plots
# # in a loop.
# for(i in 1:length(grps)){
#   p <- ggplot()+
#     geom_jitter(data = dat[analysis_group_category == "Ecosystem" &
#                              key %in% to_plot &
#                              analysis_group %in% grps[i], ],
#                 aes(x = se,
#                     y = response, #residuals,
#                     fill = variable),
#                 shape = 21,
#                 alpha = 0.5,
#                 size = 2.5)+
#     geom_line(data = lines.dat[linetype_simple != "95% CI of estimate" &
#                                  analysis_group_category == "Ecosystem" &
#                                  key %in% to_plot &
#                                  analysis_group %in% grps[i], ],
#               aes(x = se, y = value,
#                   color = variable,
#                   linetype = linetype_simple,
#                   group = interaction(linetype_simple, variable, linetype)))+
#     scale_linetype_manual(name = NULL,
#                           values = c("95% CI" = "dashed",
#                                      "99% CI" = "dotted",
#                                      "Model estimate" = "solid"))+
#     # Model estimate:
#     geom_errorbar(data = model_ests[analysis_group_category == "Ecosystem" &
#                                       key %in% to_plot  &
#                                       analysis_group %in% grps[i], ],
#                   aes(ymin = mean_ll95, ymax = mean_ul95,
#                       x = se), 
#                   width = (max(model_ests[analysis_group_category == "Ecosystem"  &
#                                             key %in% to_plot  &
#                                             analysis_group %in% grps[i], ]$max_se) * .2))+
#     geom_point(data = model_ests[analysis_group_category == "Ecosystem"  &
#                                    key %in% to_plot  &
#                                    analysis_group %in% grps[i], ],
#                aes(y = estimate,
#                    x = se, fill = variable),
#                shape = 21, size = 4)+
#     xlab("Standard Error")+
#     ylab("Residuals")+
#     scale_fill_manual(name = NULL,
#                       values = quaternary_palette,
#                       drop = F)+
#     scale_color_manual(name = NULL,
#                        values = quaternary_palette,
#                        drop = F)+
#     facet_grid(variable ~ analysis_group,
#                labeller = as_labeller(group_labels))+
#     theme_bw()+
#     theme(strip.background = element_blank(),
#           plot.title = element_text(hjust = 0.5),
#           strip.text.y=element_blank()
#     )+
#     scale_x_reverse()+
#     coord_flip()
#   if(length(patch) == 0){
#     patch <- p
#   }else{
#     patch <- patch + p
#   }
# }
# 
# patch + plot_layout(ncol = 4, guides = "collect")
# ggsave("figures/revision/supplement/Ecosystem funnel plots raw.pdf", width = 12, height = 12, dpi = 300)



#' *March 21 2025*
#
#
#' [Nativeness-level publication bias. This didn't produce anything useful.]
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
          "scico", "stringr",
          "plotly", "nlme", 
          "ggtext")#"ggh4x", "ggstance"
groundhog.library(libs, groundhog.day)
library("orchaRd")

# 
#' [Tried bias per nativeness group. No go. Ask Shinichi about itneractions of bias with categorical or continuous variables]
#' [BUT, let's redo simpler. Bias should be done with sqrt(1/eff_N) as predictor]
#' [IF the N coefficient is NOT significant, the intercept is an adjusted estimate of impact without bias]
#' [IF the N coefficient IS significant, then do (1/eff_N), whose intercept provides a less bias estimate of overall effect]
#' [REF: Nakagawa 2022 MEE]
#
# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# ~~~~~~~~~~~~~~~~~ -------------------------------------------------------
# Load datasets and model guide -----------------------------------------------
dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")
dat <- dat[eff_type == "smd" & !is.na(yi), ]

master_guide <- fread("outputs/revision/summaries/model_comparison_table.csv")

unique(master_guide$preferred_model)

sub_guide <- master_guide[preferred_model == "yes" &
                            effect_size == "smd" &
                            filter_big_CVs == "no"]

unique(sub_guide$analysis_group)
nrow(sub_guide)

length(unique(sub_guide$analysis_group))
unique(sub_guide$analysis_group)

#
unique(sub_guide$nativeness_var)
unique(sub_guide$analysis_group_category)

sub_guide$analysis_group

sub_guide[, label := gsub("_", " ", analysis_group)]
unique(sub_guide$label)

sub_guide[, label := gsub("Invert ", "", label)]
sub_guide[, label := gsub("Vert Carn", "Carnivore", label)]
sub_guide[, label := gsub("Vert Herb", "Herbivore", label)]
unique(sub_guide$label)

# >>> Create model guide ------------------------------------------------------
# We predict that publication bias is higher for introduced megafauna than native.
# We can consider this an actual hypothesis.

# Since some of these will have very low sample sizes, we should do this based on all possible random effect structures. 
# Let's use full guide as a template

#
#
# NOTES ON HOW TO TEST INTERACTION OF NATIVENESS WITH BIAS
#
#
#

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# Interaction with nativeness ---------------------------------------------

interaction_guide <- sub_guide[, 
                               .(model_id_nativeness, model_path_nativeness, 
                                 nativeness_var, random_effect, exclusion,
                                 analysis_group_category,
                                 analysis_group)]


interaction_guide[, `:=` (predictor1 = "inv_sqrt_eff_N", predictor2 = "inv_eff_N")]
interaction_guide[duplicated(model_id_nativeness), ]
# So we can use model_id_nativeness to compare these two predictors

interaction_guide.mlt <- melt(interaction_guide,
                              measure.vars = c("predictor1", "predictor2"),
                              value.name = "response")
interaction_guide.mlt$variable <- NULL
interaction_guide.mlt

interaction_guide.mlt[, formula := paste("~", response, "*", nativeness_var)]
interaction_guide.mlt

interaction_guide.mlt[, model_id := paste0("interaction_bias_model_", 1:.N)]
interaction_guide.mlt

interaction_guide.mlt[, model_path := file.path("outputs/publication_bias/models", paste0(model_id, ".Rds"))]
interaction_guide.mlt

saveRDS(interaction_guide.mlt, "outputs/publication_bias/data/interaction_bias_guide.Rds")

guide <- copy(interaction_guide.mlt)

# file.remove(file.exists(interaction_guide.mlt$model_path))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------
# Run bias models ---------------------------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------

#  Load guide and data -------------------------------------------------

clust_out <- prepare_cluster(n = nrow(guide))

guide
m <- c()
sub.dat <- c()
i <- 1

# file.remove(list.files("outputs/publication_bias/models/", full.names = T))

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
# Shame.

guide <- guide[file.exists(model_path), ]

ms <- lapply(guide$model_path, readRDS)

names(ms) <- guide$model_id

# >>> Tidy models ---------------------------------------------------

ms.tidy <- lapply(ms, tidy_with_CIs)

ms.tidy.dat <- rbindlist(ms.tidy, idcol = "model_id")

ms.tidy.dat

ms.tidy.dat.mrg <- merge(ms.tidy.dat,
                         guide[, .(model_id, analysis_group_category,
                                   nativeness_var,exclusion,
                                   response, model_id_nativeness, model_path_nativeness,
                                   analysis_group, model_path)],
                         by = "model_id")

ms.tidy.dat.mrg

ms.tidy.dat.mrg[, type := ifelse(response == "inv_sqrt_eff_N", "Testing for bias", "Corrected estimate")]
ms.tidy.dat.mrg


grepl("inv_sqrt_eff_N[:]", "inv_sqrt_eff_N:Africa_ComparisonIntroduced")
grepl("inv_sqrt_eff_N[:]", "inv_sqrt_eff_N")
# dang.

ms.tidy.dat.mrg[, bias := ifelse(.SD[grepl("inv_sqrt_eff_N[:]", term)]$p.value < 0.05, "yes", "no"),
                by = .(model_id_nativeness)]
ms.tidy.dat.mrg

# Whoa, that's surprising huh?
ms.tidy.dat.mrg[bias == "yes", ]

ms.tidy.dat.mrg

corrected_estimates <- ms.tidy.dat.mrg[bias == "yes" & type == "Corrected estimate"]

corrected_estimates

# >>> Create predictions from corrected estimates -------------------------

# This is ugly. THink I need to do it in a loop...
i <- 1
grid <- c()
pred <- list()
ids <- unique(corrected_estimates$model_id)
sub_guide <- c() 
sub_dat <- c()

for(i in 1:length(ids)){
  sub_guide <- corrected_estimates[model_id == ids[i], ]
  sub_dat <- dat[eval(parse(text = unique(sub_guide$exclusion))), ]
  
  grid <- unlist(unique(dat[, unique(sub_guide$nativeness_var), with = F]))
  grid <- grid[!is.na(grid)]
  grid <- data.table(V1= grid)
  names(grid) <- unique(sub_guide$nativeness_var) 
  grid[, inv_eff_N := 0]
  
  pred[[i]] <- rma_predictions(readRDS(unique(corrected_estimates[model_id == ids[i], ]$model_path)),
                               grid)
  pred[[i]][, model_id := ids[i]]
  
  setnames(pred[[i]], unique(sub_guide$nativeness_var) , "nativeness_synth")
  
}

pred.dat <- rbindlist(pred)

pred.dat.mrg <- merge(pred.dat,
                      unique(corrected_estimates[, .(model_id, nativeness_var,
                                                     analysis_group_category, analysis_group)]))

ggplot(data = pred.dat.mrg)+
  geom_vline(xintercept = 0)+
  geom_pointrangeh(aes(y = nativeness_synth, x = pred, xmin = ci.lb, xmax = ci.ub, fill = nativeness_synth),
                   shape = 21, size = 1)+
  facet_wrap(~analysis_group, scales = "free_y")+
  
  
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ---------------------------------------
# Plot --------------------------------------------------------------------
unique(ms.tidy.dat.mrg$nativeness_level)
quaternary_palette <- c("Introduced" = "#57b7db", "Intact_Africa" = "#d7a4a3",
                        "Invasive" = "#faae6b", "Native" = "#a7928c")
labels <- c("Introduced" = "Introduced", "Intact_Africa" = "African assemblages",
            "Invasive" = "'Invasive'", "Native" = "Native")
# We're only interested in the intercepts I believe.

ms.tidy.dat.mrg$nativeness_level <- factor(ms.tidy.dat.mrg$nativeness_level,
                                           levels = rev(c("Native", "Intact_Africa",
                                                          "Introduced", "Invasive")))

# Effective N:
ggplot()+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_errorbarh(data = ms.tidy.dat.mrg[response == "eff_N" &
                                          analysis_group_category == "Vertebrates" &
                                          term == "intercept"],
                 aes(y = analysis_group, xmin = ci.lb, xmax = ci.ub,
                     group = nativeness_level),
                 position = position_dodgev(height = .5))+
  geom_point(data = ms.tidy.dat.mrg[response == "eff_N" &
                                      analysis_group_category == "Vertebrates" &
                                      term == "intercept"],
             aes(y = analysis_group, x = estimate,
                 fill = nativeness_level),
             size = 3,
             shape = 21,
             position = position_dodgev(height = .5))+
  scale_fill_manual(NULL,
                    values = quaternary_palette,
                    labels = labels)+
  xlab("Estimated effect when sample size ≈ Infinity")+
  facet_wrap(~analysis_group_category, scales = "free")+
  theme_lundy


# SE:
ggplot()+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_errorbarh(data = ms.tidy.dat.mrg[response == "se" &
                                          analysis_group_category == "Vertebrates" &
                                          term == "intercept"],
                 aes(y = analysis_group, xmin = ci.lb, xmax = ci.ub,
                     group = nativeness_level),
                 position = position_dodgev(height = .5))+
  geom_point(data = ms.tidy.dat.mrg[response == "se" &
                                      analysis_group_category == "Vertebrates" &
                                      term == "intercept"],
             aes(y = analysis_group, x = estimate,
                 fill = nativeness_level),
             size = 3,
             shape = 21,
             position = position_dodgev(height = .5))+
  scale_fill_manual(NULL,
                    values = quaternary_palette,
                    labels = labels)+
  xlab("Estimated effect when sample size ≈ Infinity")+
  facet_wrap(~analysis_group_category, scales = "free")+
  theme_lundy




# What does non-intercept mean???
ggplot()+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_errorbarh(data = ms.tidy.dat.mrg[response == "eff_N" &
                                          analysis_group_category == "Vertebrates" &
                                          term != "intercept"],
                 aes(y = analysis_group, xmin = ci.lb, xmax = ci.ub,
                     group = nativeness_level),
                 position = position_dodgev(height = .5))+
  geom_point(data = ms.tidy.dat.mrg[response == "eff_N" &
                                      analysis_group_category == "Vertebrates" &
                                      term != "intercept"],
             aes(y = analysis_group, x = estimate,
                 fill = nativeness_level),
             size = 3,
             shape = 21,
             position = position_dodgev(height = .5))+
  scale_fill_manual(NULL,
                    values = quaternary_palette,
                    labels = labels)+
  xlab("Influence of effective sample size")+
  facet_wrap(~analysis_group_category, scales = "free")+
  theme_lundy








