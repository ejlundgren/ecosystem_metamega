#' *March 21st, 2025*
#
#' [Analyze randomized models]
# 1. Load randomized models and summarize
# 2. Calculate same statistics for actual models
# 3. Plot
#
#
# PREPARE WORKSPACE -------------------------------------------------------
rm(list = ls())

library("groundhog")
groundhog.day <- "2024-07-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "shades", "patchwork", "dplyr",
          "plotly", "ggridges", "scico",
          "ggtext", "ggh4x", "ggstance",
          "stringr") #
groundhog.library(libs, groundhog.day)

prop_change_in_variance <- function(mod1, mod2){
  
  return((sum(mod1$sigma2) - sum(mod2$sigma2)) / 
      sum(mod1$sigma2))

}

I2 <- function(mod){
  
  W <- diag(1/mod$vi)
  X <- model.matrix(mod)
  P <- W - W %*% X %*% solve(t(X) %*% W %*% X) %*% t(X) %*% W
  return(100 * sum(mod$sigma2) / (sum(mod$sigma2) + (mod$k-mod$p)/sum(diag(P))))
  
}

tidy_with_CIs <- function(m){
  x <- tidy(m) %>%
    mutate(ci.lb = m$ci.lb, ci.ub = m$ci.ub)
  return(x)
}

# to get what percent is below an observation...
get_percentile <- function(obs, population){
  P <- length(population[population < obs]) / length(population) * 100
  return(P)
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 0. Load data ------------------------------------------------------------
dat <- fread("data/master_data.csv")

# Load model comparison table
working_guide <- fread("outputs/main_text/summaries/model_comparison_table.csv")

working_guide <- working_guide[preferred_model == "yes"]
working_guide

working_guide <- working_guide[nativeness_var != "Africa_Comparison"]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 1. Load randomized model summaries --------------------------------------

randos <- readRDS("outputs/remote_cluster_randomization_mirror/summaries/final_summarized_model_results.Rds")
randos

setdiff(working_guide$model_id_nativeness, randos$model_id_nativeness)
#' [Should be length 0]
# >>> Calculate CIs for each model ----------------------------------------

randos.sum <- randos[, .(var_reduced_lwr.ci = quantile(prop_variance_explained,
                                                       c(0.025)),
                         var_reduced_upper.ci = quantile(prop_variance_explained,
                                                       c(0.975)),
                         BIC_lwr.ci = quantile(BIC,
                                               c(0.025)),
                         BIC_upper.ci = quantile(BIC,
                                                 c(0.975)),
                         I2_total_lwr.ci = quantile(I2_total,
                                               c(0.025)),
                         I2_total_upper.ci = quantile(I2_total,
                                                 c(0.975))),
                     by = .(model_id_nativeness, nativeness_var)]

randos.sum

randos.sum[duplicated(model_id_nativeness)]
#' *should be 0 rows.*

randos.sum.mrg <- merge(randos.sum,
                        working_guide[, .(analysis_group, analysis_group_category,
                                          model_id_nativeness)],
                        by = "model_id_nativeness")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 2. Calculate observed model summaries --------------------------------------

# need to calculate var reduction
observed <- list()
i <- 1
m0 <- c()
m1 <- c()

for(i in 1:nrow(working_guide)){
  m0 <- working_guide[i, ]$model_path_null |> readRDS()
  m1 <- working_guide[i, ]$model_path_nativeness |> readRDS()
  
  observed[[i]] <- cbind(working_guide[i, .(model_id_nativeness, BIC_nativeness, i2_nativeness,
                                            analysis_group, analysis_group_category, nativeness_var)],
                         data.table(obs_var_reduced = prop_change_in_variance(mod1 = m0, mod2 = m1)))
  
  cat(i, "/", nrow(working_guide), "\r")
  
}
observed

observed <- rbindlist(observed)
observed

# >>> Get % of random models below observation ----------------------------
comps <- unique(observed$model_id_nativeness)
x <- comps[1]

percentiles <- lapply(comps,
                      function(x){
                        data.table(model_id_nativeness = x,
                                   percentile_var_reduced = get_percentile(observed[model_id_nativeness == x]$obs_var_reduced,
                                                                           randos[model_id_nativeness == x]$prop_variance_explained),
                                   percentile_BIC = get_percentile(observed[model_id_nativeness == x]$BIC,
                                                                           randos[model_id_nativeness == x]$BIC),
                                   percentile_I2 = get_percentile(observed[model_id_nativeness == x]$i2_nativeness,
                                                                   randos[model_id_nativeness == x]$I2_total)) 
                      })
percentiles <- rbindlist(percentiles)
percentiles

hist(percentiles$percentile_var_reduced)

percentiles.mrg <- merge(percentiles,
                         observed,
                         by = "model_id_nativeness")

percentiles.mrg
percentiles.mrg[percentile_var_reduced > 97.5, ]

# >>> Get % of randomized models for each randomized model (for the fill aesthetic in figure) -------------

model_id_i <- c()
perc <- c()
i <- 1

for(i in 1:nrow(randos)){
  model_id_i <- randos[i, ]$model_id_nativeness
  
  perc <- get_percentile(obs = randos[i, ]$prop_variance_explained,
                 population = randos[model_id_nativeness == model_id_i, ]$prop_variance_explained)
  randos[i, percentile := perc]
  cat(i, "/", nrow(randos), "\r")
  
}

randos

randos.mrg <- merge(randos,
                    working_guide[, .(model_id_nativeness, analysis_group, analysis_group_category)],
                    by = "model_id_nativeness")
randos.mrg

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 3. Figure --------------------------------------


# >>> Prepare -------------------------------------------------------------

lvls <- c("Dead_Vegetation",
          "Litter_Cover", "Bare_Ground",
          "Soil_Compaction", "Soil_Moisture",
          "Soil_Total_C",
          "Soil_C:N", "Soil_Total_N",
          "Soil_Labile_N", "Soil_Total_P",
          "Soil_Total_Ca", "Soil_Total_Mg",
          
          
          "Invertebrate_Diversity", "Invertebrate_Abundance",
          "Invert_Herbivore_Abundance",
          "Invert_Predator_Abundance",
          "Invert_Detritivore_Abundance",
          
          "Vertebrate_Diversity", 
          "Vertebrate_Abundance",
          "Vert_Herb_Abundance",
          "Vert_Carn_Abundance",
          "Small_Mammal_Abundance",
          "Bird_Diversity",
          "Bird_Abundance")
setdiff(percentiles.mrg$analysis_group, lvls)
setdiff(lvls, percentiles.mrg$analysis_group)

percentiles.mrg$analysis_group <- factor(percentiles.mrg$analysis_group,
                              levels = rev(lvls))
randos.sum.mrg$analysis_group <- factor(randos.sum.mrg$analysis_group,
                                 levels = rev(lvls))
randos.mrg$analysis_group <- factor(randos.mrg$analysis_group,
                           levels = rev(lvls))

# ------- Make y axis numeric for rectangles -------------------------------!
percentiles.mrg[, analysis_group_cont := as.numeric(analysis_group)]
setorder(percentiles.mrg, analysis_group_cont)
unique(percentiles.mrg[, .(analysis_group, analysis_group_cont)])
randos.sum.mrg[, analysis_group_cont := as.numeric(analysis_group)]
randos.mrg[, analysis_group_cont := as.numeric(analysis_group)]

#
rects <- unique(percentiles.mrg[, .(analysis_group, analysis_group_cont, analysis_group_category)])
rects[, ymax := analysis_group_cont + 0.5]
rects[, ymin := analysis_group_cont - 0.5]


#
rects[, label := gsub("_", " ", analysis_group)]
rects[, label := gsub("Invert ", "", label)]
rects[, label := gsub("Vert Carn ", "Carnivore ", label)]
rects[, label := gsub("Vert Herb ", "Herbivore ", label)]

#
rects[, plot := ifelse(analysis_group_cont %% 2 == 0, "yes", "no")]


#
randos.mrg$nativeness_var <- factor(randos.mrg$nativeness_var,
                                    levels = rev(c("Herbivore_nativeness", "Invasive")))

randos.sum.mrg$nativeness_var <- factor(randos.sum.mrg$nativeness_var,
                                    levels = rev(c("Herbivore_nativeness", "Invasive")))

percentiles.mrg$nativeness_var <- factor(percentiles.mrg$nativeness_var,
                                    levels = rev(c("Herbivore_nativeness", "Invasive")))

min(randos.mrg$percentile)
#
randos.mrg$analysis_group_category <- factor(randos.mrg$analysis_group_category,
                                             levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

randos.sum.mrg$analysis_group_category <- factor(randos.sum.mrg$analysis_group_category,
                                             levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

percentiles.mrg$analysis_group_category <- factor(percentiles.mrg$analysis_group_category,
                                             levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

rects$analysis_group_category <- factor(rects$analysis_group_category,
                                                  levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

# >>> Simple and compact...? ----------------------------------------------
xlim <- range(randos.mrg$prop_variance_explained)

p1 <- ggplot()+
  geom_rect(data = rects[plot == "yes" &
                           analysis_group_category == "Vertebrates"], aes(xmin = -Inf, xmax = Inf,
                                             ymin = ymin, ymax = ymax),
            fill = "grey80")+
  geom_jitter(data = randos.mrg[analysis_group_category == "Vertebrates"], aes(x = prop_variance_explained, y = analysis_group_cont,
                                     group = nativeness_var, 
                                     color = percentile),
              # color = "grey50",
              alpha = .25,
              position = position_jitterdodgev(dodge.height = .85, 
                                               jitter.width = 0,
                                               jitter.height = .3))+
  geom_errorbar(data = randos.sum.mrg[analysis_group_category == "Vertebrates"], aes(xmin = var_reduced_lwr.ci, xmax = var_reduced_upper.ci,
                                           y = analysis_group_cont, group = nativeness_var),
                position = position_dodgev(height = .85))+
  geom_point(data = percentiles.mrg[analysis_group_category == "Vertebrates"], 
             aes(x = obs_var_reduced,
                 y = analysis_group_cont, 
                 shape = nativeness_var,
                 group = nativeness_var,
                 fill = percentile_var_reduced),
             # shape = 21,
             size = 3,
             position = position_dodgev(height = .85))+
  scale_y_continuous(breaks = rects[analysis_group_category == "Vertebrates"]$analysis_group_cont,
                     labels = rects[analysis_group_category == "Vertebrates"]$label)+
  scale_shape_manual(name = NULL,
                     values = c("Invasive" = 23,
                                "Herbivore_nativeness" = 21),
                     labels = c("Invasive" = "'Invasiveness'",
                                "Herbivore_nativeness" = "Nativeness"),
                     guide = guide_legend(reverse = TRUE))+
  scale_fill_scico(name = "% of randomized\nmodels", 
                   palette = "buda",
                   limits = c(0, 100))+
  scale_color_scico(name = "% of randomized\nmodels",
                    palette = "buda",
                    limits = c(0, 100))+
  facet_wrap(~analysis_group_category,
             ncol = 1,
             strip.position = "left",
             scales = "free_y")+
  coord_cartesian(xlim)+
  ylab(NULL)+
  xlab("Proportion of total variance explained by\nnativeness or 'invasiveness'\n(sigma2_intercept - sigma2_model) / sigma2_intercept")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "right",
        # axis.title.x = element_markdown(),
        strip.placement = "outside",
        strip.background = element_blank(),
        # strip.position = "left",
        axis.ticks.y = element_blank())
p1

p2 <- ggplot()+
  geom_rect(data = rects[plot == "yes" &
                           analysis_group_category == "Invertebrates"], aes(xmin = -Inf, xmax = Inf,
                                                                          ymin = ymin, ymax = ymax),
            fill = "grey80")+
  geom_jitter(data = randos.mrg[analysis_group_category == "Invertebrates"], aes(x = prop_variance_explained, y = analysis_group_cont,
                                                                               group = nativeness_var, 
                                                                               color = percentile),
              # color = "grey50",
              alpha = .25,
              position = position_jitterdodgev(dodge.height = .85, 
                                               jitter.width = 0,
                                               jitter.height = .3))+
  geom_errorbar(data = randos.sum.mrg[analysis_group_category == "Invertebrates"], aes(xmin = var_reduced_lwr.ci, xmax = var_reduced_upper.ci,
                                                                                     y = analysis_group_cont, group = nativeness_var),
                position = position_dodgev(height = .85))+
  geom_point(data = percentiles.mrg[analysis_group_category == "Invertebrates"], 
             aes(x = obs_var_reduced,
                 y = analysis_group_cont, 
                 shape = nativeness_var,
                 group = nativeness_var,
                 fill = percentile_var_reduced),
             # shape = 21,
             size = 3,
             position = position_dodgev(height = .85))+
  scale_y_continuous(breaks = rects[analysis_group_category == "Invertebrates"]$analysis_group_cont,
                     labels = rects[analysis_group_category == "Invertebrates"]$label)+
  scale_shape_manual(name = NULL,
                     values = c("Invasive" = 23,
                                "Herbivore_nativeness" = 21),
                     labels = c("Invasive" = "'Invasiveness'",
                                "Herbivore_nativeness" = "Nativeness"),
                     guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim)+
  scale_fill_scico(name = "% of randomized\nmodels", 
                   palette = "buda",
                   limits = c(0, 100))+
  scale_color_scico(name = "% of randomized\nmodels",
                    palette = "buda",
                    limits = c(0, 100))+
  facet_wrap(~analysis_group_category,
             ncol = 1,
             strip.position = "left",
             scales = "free_y")+
  ylab(NULL)+
  xlab("Proportion of total variance explained by\nnativeness or 'invasiveness'\n(sigma2_intercept - sigma2_model) / sigma2_intercept")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "right",
        # axis.title.x = element_markdown(),
        strip.placement = "outside",
        strip.background = element_blank(),
        # strip.position = "left",
        axis.ticks.y = element_blank())
p2


p3 <- ggplot()+
  geom_rect(data = rects[plot == "yes" &
                           analysis_group_category == "Ecosystem"], aes(xmin = -Inf, xmax = Inf,
                                                                            ymin = ymin, ymax = ymax),
            fill = "grey80")+
  geom_jitter(data = randos.mrg[analysis_group_category == "Ecosystem"], aes(x = prop_variance_explained, y = analysis_group_cont,
                                                                                 group = nativeness_var, 
                                                                                 color = percentile),
              # color = "grey50",
              alpha = .25,
              position = position_jitterdodgev(dodge.height = .85, 
                                               jitter.width = 0,
                                               jitter.height = .3))+
  geom_errorbar(data = randos.sum.mrg[analysis_group_category == "Ecosystem"], aes(xmin = var_reduced_lwr.ci, xmax = var_reduced_upper.ci,
                                                                                       y = analysis_group_cont, group = nativeness_var),
                position = position_dodgev(height = .85))+
  geom_point(data = percentiles.mrg[analysis_group_category == "Ecosystem"], 
             aes(x = obs_var_reduced,
                 y = analysis_group_cont, 
                 shape = nativeness_var,
                 group = nativeness_var,
                 fill = percentile_var_reduced),
             # shape = 21,
             size = 3,
             position = position_dodgev(height = .85))+
  scale_y_continuous(breaks = rects[analysis_group_category == "Ecosystem"]$analysis_group_cont,
                     labels = rects[analysis_group_category == "Ecosystem"]$label)+
  scale_shape_manual(name = NULL,
                     values = c("Invasive" = 23,
                                "Herbivore_nativeness" = 21),
                     labels = c("Invasive" = "'Invasiveness'",
                                "Herbivore_nativeness" = "Nativeness"),
                     guide = guide_legend(reverse = TRUE))+
  coord_cartesian(xlim)+
  scale_fill_scico(name = "% of randomized\nmodels", 
                   palette = "buda",
                   limits = c(0, 100))+
  scale_color_scico(name = "% of randomized\nmodels",
                    palette = "buda",
                    limits = c(0, 100))+
  facet_wrap(~analysis_group_category,
             ncol = 1,
             strip.position = "left",
             scales = "free_y")+
  ylab(NULL)+
  xlab("Proportion of total variance explained by\nnativeness or 'invasiveness'\n(sigma2_intercept - sigma2_model) / sigma2_intercept")+
  theme_bw()+
  theme(panel.grid = element_blank(),
        legend.position = "right",
        # axis.title.x = element_markdown(),
        strip.placement = "outside",
        strip.background = element_blank(),
        # strip.position = "left",
        axis.ticks.y = element_blank())
p3


percentiles.mrg[, .(n_groups = uniqueN(analysis_group)), by = analysis_group_category]
left <- p1 + theme(axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.x = element_blank()) + 
  p2 + 
  plot_layout(heights = c(7/12, 5/12), guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "none")

right <- p3 
right

left & theme(panel.border = element_blank()) | right & theme(panel.border = element_blank(),
                                                             legend.position = "none")

# object is too big, too many points to annotate in Inkscape. So save as both PNG and PDF
ggsave("figures/main_text/Fig 4 raw.png", width = 9, height = 6, dpi = 300)
ggsave("figures/main_text/Fig 4 raw.pdf", width = 9, height = 6)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 4. Summaries for text ---------------------------------------------------
percentiles.mrg[, obs_var_reduced := round(obs_var_reduced, 3)]
range(percentiles.mrg$obs_var_reduced)
median(percentiles.mrg$obs_var_reduced)
mean(percentiles.mrg$obs_var_reduced)

# So this is the percent of random models that the observed models outperformed...
range(percentiles.mrg$percentile_var_reduced)

# and this is the % of randomized models that outperformed observed:
100-range(percentiles.mrg$percentile_var_reduced)
100-mean(percentiles.mrg$percentile_var_reduced)
100-mean(percentiles.mrg$percentile_var_reduced)

median(randos.mrg$prop_variance_explained)
mean(randos.mrg$prop_variance_explained)
range(randos.mrg$prop_variance_explained)

