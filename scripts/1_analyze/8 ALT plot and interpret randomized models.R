#' *original: March 21st, 2025*
#' *revised: October 31st, 2025*
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
groundhog.day <- "2025-04-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "shades", "patchwork", "dplyr",
          "plotly", "ggridges", "scico",
          "ggtext", "ggh4x", "ggstance",
          "ggh4x",
          "stringr") #
groundhog.library(libs, groundhog.day)

prop_change_in_variance <- function(mod1, mod2){
  return( (sum(mod1$sigma2) - sum(mod2$sigma2)) / sum(mod1$sigma2))
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

# get 95% confidence interval
ci_95 <- function(population){
  n <- length(population)
  xbar <- mean(population)
  s <- sd(population)
  margin_of_error <- qt(0.975, df=n-1) * (s/sqrt(n))
  lwr.ci <- xbar - margin_of_error
  upper.ci <- xbar + margin_of_error
  return(c(lwr.ci, upper.ci))
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 0. Load data ------------------------------------------------------------
dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")
unique(dat$analysis_group_category)

# Load model comparison table
master_guide <- fread("outputs/revision/summaries/model_comparison_table.csv")
master_guide[duplicated(model_id_nativeness)]

working_guide <- master_guide[random_effect == "list(~1 | Citation / data_point_ID)" &
                                effect_size == "smd" &
                                filter_big_CVs == "yes", ]
working_guide

working_guide <- working_guide[nativeness_var != "Africa_Comparison"]

unique(working_guide$analysis_group)
unique(working_guide$analysis_group_category)

working_guide <- working_guide[!analysis_group %in% c("Aboveground_Primary_Productivity", "Mamm_SmallHerb_Abundance", "TerrestrialBird_Abundance")]
dat <- dat[!analysis_group %in% c("Aboveground_Primary_Productivity", "Mamm_SmallHerb_Abundance", "TerrestrialBird_Abundance")]


master_guide[analysis_group == "Dead_Vegetation"]

# ~~~~~~~~~~~~~~~~~~~analysis_group# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 1. Load randomized model summaries --------------------------------------
list.files("outputs/remote_cluster_randomization_mirror/summaries/")

randos <- readRDS("outputs/remote_cluster_randomization_mirror/summaries/final_summarized_model_results_simple_RE.Rds")
randos

unique(working_guide[model_id_nativeness %in% randos$model_id_nativeness, ]$analysis_group_category)

setdiff(working_guide$model_id_nativeness, randos$model_id_nativeness)
#' [Should be length 0]
#' 
randos <- randos[model_id_nativeness %in% working_guide$model_id_nativeness]
randos

working_guide[analysis_group_category == "Microbes", analysis_group_category := "Ecosystem"]
# randos[analysis_group_category == "Microbes", analysis_group_category := "Soil"]

# Collapse ecosystem, plant nutrients, and soil:
unique(working_guide$analysis_group_category)

working_guide[analysis_group_category %in% c("Soil", "Plant_Nutrients", "Ecosystem"),
              analysis_group_category := "Ecosystem"]

# OK, let's try this differently...Not sure which is correct though...
working_guide[analysis_group == "Litter_Cover" & nativeness_var == "Herbivore_nativeness"]

randos[model_id_nativeness %in% "model_2937", ]

ci_95(randos[model_id_nativeness %in% "model_2937", ]$prop_variance_explained)
observed[analysis_group == "Litter_Cover" & nativeness_var == "Herbivore_nativeness"]$obs_var_reduced


# >>> Calculate CIs for each model ----------------------------------------

randos.sum <- randos[, .(var_reduced_lwr.ci = ci_95(prop_variance_explained)[1],
                         var_reduced_upper.ci = ci_95(prop_variance_explained)[2]),
                     by = .(model_id_nativeness, nativeness_var)]

range(randos.sum$var_reduced_lwr.ci)

randos.sum[duplicated(model_id_nativeness)]
#' *should be 0 rows.*

randos.sum.mrg <- merge(randos.sum,
                        working_guide[, .(analysis_group, analysis_group_category,
                                          model_id_nativeness)],
                        by = "model_id_nativeness")

randos.sum.mrg[analysis_group == "Dead_Vegetation"]


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



# Merge together ----------------------------------------------------------

randos.sum.mrg2 <- merge(randos.sum.mrg,
                        observed[, .(model_id_nativeness, obs_var_reduced)],
                        by = "model_id_nativeness")

randos.sum.mrg2[obs_var_reduced < var_reduced_lwr.ci, sig := "less explanatory"]
randos.sum.mrg2[obs_var_reduced > var_reduced_upper.ci, sig := "more explanatory"]
randos.sum.mrg2[is.na(sig), sig := "random"]

randos.sum.mrg2[sig == "more explanatory"]
randos.sum.mrg2[sig != "more explanatory"]


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

unique(randos.mrg[analysis_group == "Dead_Vegetation"]$analysis_group_category)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 3. Figure --------------------------------------


# >>> Prepare -------------------------------------------------------------


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
setdiff(percentiles.mrg$analysis_group, lvls)
setdiff(lvls, percentiles.mrg$analysis_group)

percentiles.mrg$analysis_group <- factor(percentiles.mrg$analysis_group,
                              levels = rev(lvls))
randos.sum.mrg$analysis_group <- factor(randos.sum.mrg$analysis_group,
                                 levels = rev(lvls))
randos.mrg$analysis_group <- factor(randos.mrg$analysis_group,
                           levels = rev(lvls))

percentiles.mrg$analysis_group <- droplevels(percentiles.mrg$analysis_group)
randos.sum.mrg$analysis_group <- droplevels(randos.sum.mrg$analysis_group)
randos.mrg$analysis_group <- droplevels(randos.mrg$analysis_group)

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
unique(rects$analysis_group)
rects[analysis_group == "TerrestrialBird_Abundance", label := "Terrestrial Bird Abundance"]
rects[analysis_group == "Mamm_SmallHerb_Abundance", label := "Small Herbivorous Mammal Abundance"]
rects[analysis_group == "Bird_Carnivore_Abundance", label := "Predatory Bird Abundance"]
rects[analysis_group == "Bird_Omnivore_Abundance", label := "Omnivorous Bird Abundance"]
unique(rects$label)

#
rects[, plot := ifelse(analysis_group_cont %% 2 == 0, "no", "yes")]

#
randos.mrg$nativeness_var <- factor(randos.mrg$nativeness_var,
                                    levels = rev(c("Herbivore_nativeness", "Invasive")))

randos.sum.mrg$nativeness_var <- factor(randos.sum.mrg$nativeness_var,
                                    levels = rev(c("Herbivore_nativeness", "Invasive")))

percentiles.mrg$nativeness_var <- factor(percentiles.mrg$nativeness_var,
                                    levels = rev(c("Herbivore_nativeness", "Invasive")))

min(randos.mrg$percentile)

#
unique(randos.mrg$analysis_group_category)
unique(percentiles.mrg$analysis_group_category)
randos.mrg[analysis_group_category == "Plant_Nutrients"]
# dat[analysis_group_category == "Plant_Nutrients"]


randos.mrg$analysis_group_category <- factor(randos.mrg$analysis_group_category,
                                             levels = c("Vertebrates", "Invertebrates", "Ecosystem")) #, "Plant_Nutrients", "Soil"

randos.sum.mrg$analysis_group_category <- factor(randos.sum.mrg$analysis_group_category,
                                             levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

percentiles.mrg$analysis_group_category <- factor(percentiles.mrg$analysis_group_category,
                                             levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

rects$analysis_group_category <- factor(rects$analysis_group_category,
                                                  levels = c("Vertebrates", "Invertebrates", "Ecosystem"))

# Dead_Vegetation is showing up Ecosystem and Soil...wtf.
percentiles.mrg[, .(n = uniqueN(analysis_group_category)), by = .(analysis_group)][n > 1]
randos.mrg[, .(n = uniqueN(analysis_group_category)), by = .(analysis_group)][n > 1]
randos.sum.mrg[, .(n = uniqueN(analysis_group_category)), by = .(analysis_group)][n > 1]
rects[, .(n = uniqueN(analysis_group_category)), by = .(analysis_group)][n > 1]

# >>> Plot ----------------------------------------------
# xlim <- range(randos.mrg$prop_variance_explained)

facet_labs <- (gsub("_", " ", unique(randos.mrg$analysis_group_category)))
names(facet_labs) <- unique(randos.mrg$analysis_group_category)

rando_plot <- function(grp, xlim){
  ggplot()+
    geom_rect(data = rects[plot == "yes" &
                             analysis_group_category %in% grp], 
              aes(xmin = -Inf, xmax = Inf,
                  ymin = ymin, ymax = ymax),
              fill = "grey80")+
    geom_jitter(data = randos.mrg[analysis_group_category  %in% grp], 
                aes(x = prop_variance_explained, y = analysis_group_cont,
                    group = nativeness_var, 
                    color = percentile),
                # color = "grey50",
                alpha = .25,
                position = position_jitterdodgev(dodge.height = .85, 
                                                 jitter.width = 0,
                                                 jitter.height = .3))+
    geom_errorbar(data = randos.sum.mrg[analysis_group_category %in% grp], 
                  aes(xmin = var_reduced_lwr.ci, xmax = var_reduced_upper.ci,
                      y = analysis_group_cont, group = nativeness_var),
                  position = position_dodgev(height = .85))+
    geom_point(data = percentiles.mrg[analysis_group_category %in% grp], 
               aes(x = obs_var_reduced,
                   y = analysis_group_cont, 
                   shape = nativeness_var,
                   group = nativeness_var,
                   fill = percentile_var_reduced),
               # shape = 21,
               size = 3,
               position = position_dodgev(height = .85))+
    scale_y_continuous(breaks = rects[analysis_group_category %in% grp]$analysis_group_cont,
                       labels = rects[analysis_group_category %in% grp]$label)+
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
               scales = "free_y",
               labeller = as_labeller(facet_labs))+
    # force_panelsizes(rows = unit(rev(heights)/2.2, "in"))+ # Example: 3 rows, 3 columns, with specified sizes
    coord_cartesian(xlim)+
    ylab(NULL)+
    xlab("Proportion of variance explained by\nnativeness or 'invasiveness'\n(sigma2_intercept - sigma2_model) / sigma2_intercept")+
    theme_bw()+
    theme(panel.grid = element_blank(),
          legend.position = "right",
          # axis.title.x = element_markdown(),
          strip.placement = "outside",
          strip.background = element_blank(),
          # strip.position = "left",
          axis.ticks.y = element_blank())
}

p1 <- rando_plot(grp = "Vertebrates", c(-1, 1))
p1

p2 <- rando_plot("Invertebrates", c(-1, 1))
p2

p3 <- rando_plot(c("Ecosystem"), c(-1, 1))
p3



#
percentiles.mrg[, .(n_groups = uniqueN(analysis_group)), by = analysis_group_category]
left <- p1 + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(),
                   axis.title.x = element_blank(), legend.position = "none") +
  p2 +  theme(legend.position = "none") +
  plot_layout(heights = c(6/11, 5/11), ncol = 1)+
  plot_annotation(tag_levels = "A")
  

right <- p3 +
  theme(legend.position = "right")

right

left & theme(panel.border = element_blank()) | right & theme(panel.border = element_blank())

# object is too big, too many points to annotate in Inkscape. So save as both PNG and PDF
ggsave("figures/revision/main_text/Fig 4 raw.png", width = 15, height = 8, dpi = 300)
ggsave("figures/revision/main_text/Fig 4 raw.pdf", width = 15, height = 8)

# 
# supp.p5
# ggsave("figures/revision/supplement/fig sx soil randomization.png", width = 5, height = 4)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------
# 4. Summaries for text ---------------------------------------------------
percentiles.mrg
percentiles.mrg[percentile_var_reduced < 95]
range(percentiles.mrg[percentile_var_reduced <95]$obs_var_reduced)

percentiles.mrg[percentile_var_reduced >= 95]


percentiles.mrg[, obs_var_reduced := round(obs_var_reduced, 3)]
range(percentiles.mrg$obs_var_reduced)
median(percentiles.mrg$obs_var_reduced)
mean(percentiles.mrg$obs_var_reduced)

# So this is the percent of random models that the observed models outperformed...
range(percentiles.mrg$percentile_var_reduced)
range(percentiles.mrg[percentile_var_reduced < 95]$obs_var_reduced)


# and this is the % of randomized models that outperformed observed:
100-range(percentiles.mrg$percentile_var_reduced)
100-mean(percentiles.mrg$percentile_var_reduced)
100-mean(percentiles.mrg$percentile_var_reduced)

median(randos.mrg$prop_variance_explained)
mean(randos.mrg$prop_variance_explained)
range(randos.mrg$prop_variance_explained)

