#' *March 21 2025*
#
#
#' [Produce funnel plot figures]
#
#
#
# Prepare workspace ---------------------------------------
#
rm(list = ls())
gc()

# Groundhog makes libraries consistent.
library("groundhog")
groundhog.day <- "2024-07-15"
libs <- c("metafor", "broom", "data.table",
          "ggplot2", "tidyr", "multcomp",
          "shades", "patchwork", "dplyr",
          "scico", "stringr",
          "plotly", "nlme", 
          "ggtext")#"ggh4x", "ggstance"
groundhog.library(libs, groundhog.day)

# >>> Helper functions ----------------------------------------------------

getVars <- function(formula_str){
  formula_str <- gsub("yi_smd ~ ", "", formula_str)
  formula_str <- gsub("- 1", "", formula_str)
  
  formula_str <- gsub("[*]", "+", formula_str)
  vars <- unlist(strsplit(formula_str, split = "[+]"))
  vars <- trimws(vars)
  vars <- vars[vars != "~"]
  return(vars)
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
dat <- fread("data/master_data.csv")

master_guide <- fread("outputs/main_text/summaries/model_comparison_table.csv")

unique(master_guide$preferred_model)

sub_guide <- master_guide[preferred_model == "yes" , ]

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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------
#  Manually make funnel plot -------------------------------------------
dat <- fread("data/master_data.csv")

ms <- lapply(sub_guide[analysis_group_category == "Vertebrates", ]$model_path_nativeness,
             readRDS)
names(ms) <- sub_guide[analysis_group_category == "Vertebrates", ]$label
names(ms)
 
# >>> Encapsulate ---------------------------------------------------------

prepare_funnels <- function(m,
                            residuals_or_effects = "residuals"){
  
  tab <- m |>
    tidy()
  tab
  setDT(tab)
  
  var <- getVars(m$formula.mods)
  
  tab[, variable := gsub(var, "", term)]
  #
  if("" %in% tab$variable){
    tab[variable == "", variable := var]
  }
  
  sub.dat <- m$data
  
  if(residuals_or_effects == "effects"){
    sub.dat$response <- sub.dat$yi_smd
  }else{
    sub.dat$response <- residuals(m)
  }
  #
  manifold <- CJ(se = seq(0, max(sqrt(sub.dat$vi_smd)), by = 0.01),
                 variable = tab$variable)
  manifold
  manifold.m1 <- merge(manifold,
                       tab[, .(variable, estimate, std.error)],
                       by = "variable")
  setnames(manifold.m1, "std.error", "est_se")
  nrow(manifold) == nrow(manifold.m1) # must be TRUE
  
  if(residuals_or_effects != "effects"){
    
    manifold.m1[, estimate := 0]
    
  }
  #
  manifold.m1[, ll95 := estimate - (1.96 * se)]
  manifold.m1[, ul95 := estimate + (1.96 * se)]
  manifold.m1[, ll99 := estimate - (3.29 * se)]
  manifold.m1[, ul99 := estimate + (3.29 * se)]
  manifold.m1[, mean_ll95 := estimate - (1.96 * est_se)]
  manifold.m1[, mean_ul95 := estimate + (1.96 * est_se)]
  
  lines <- melt(manifold.m1[, !c("est_se")], id.vars = c("variable", "se"),
                variable.name = "linetype")
  unique(lines$linetype)
  lines[grepl("95", linetype), linetype_simple := "95% CI"]
  lines[grepl("99", linetype), linetype_simple := "99% CI"]
  lines[linetype %in% c("mean_ll95", "mean_ul95"), linetype_simple := "95% CI of estimate"]
  lines[linetype %in% c("estimate"), linetype_simple := "Model estimate"]
  unique(lines$linetype_simple)
  
  #
  sub.dat[, se := sqrt(vi_smd)]
  setnames(sub.dat, var, "variable")
  # 
  #
  lines[, max_se := max(se)]
  
  model_est <- lines[linetype_simple %in% c("95% CI of estimate",
                                            "Model estimate") &
                       se == 0]
  
  model_est <- dcast(model_est, 
                     variable + se + max_se ~ linetype,
                     value.var = "value")
  model_est[, se := 0-(max_se * .05)]

  # 
  return(list(lines, model_est, sub.dat))
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~ -----------------------------------------------
# By residuals ------------------------------------------------------------

ms <- lapply(sub_guide$model_path_nativeness,
             readRDS)
names(ms) <- sub_guide$model_id_nativeness
names(ms)

out <- lapply(ms, prepare_funnels, residuals_or_effects = "residuals")
names(out) <- names(ms)

# ---- Format lines --------------------------------------!
lines <- lapply(out, "[[", 1)
names(lines) <- names(out)
lines

lines.dat <- rbindlist(lines, idcol = "model_id_nativeness")
lines.dat
lines.dat <- merge(lines.dat,
                   sub_guide[, .(model_id_nativeness, analysis_group_category, 
                                 analysis_group,
                                 nativeness_var)])
lines.dat

unique(lines.dat$final_label)
unique(lines.dat$variable)
lines.dat[grepl("Invasive", nativeness_var) & variable == "intercept",
          variable := "Invasive"]
lines.dat[grepl("Nativeness", nativeness_var) & variable == "intercept",
          variable := "Introduced"]
lines.dat[grepl("Africa", nativeness_var)]
lines.dat[grepl("Africa", nativeness_var) & variable == "intercept",
          variable := "African assemblage"]

# ---- Format model estimates --------------------------------------!

model_ests <- lapply(out, "[[", 2)
names(model_ests) <- names(out)
model_ests

model_ests <- rbindlist(model_ests, idcol = "model_id_nativeness")
model_ests

model_ests
model_ests <- merge(model_ests,
                    sub_guide[, .(model_id_nativeness, analysis_group_category, 
                                  analysis_group,
                                  nativeness_var)])
model_ests

model_ests[grepl("Invasive", nativeness_var) & variable == "intercept",
           variable := "Invasive"]
model_ests[grepl("Nativeness", nativeness_var) & variable == "intercept",
           variable := "Introduced"]
model_ests[grepl("Africa", nativeness_var) & variable == "intercept",
          variable := "African assemblage"]

# ---- Format model data --------------------------------------!

dat <- lapply(out, "[[", 3)
names(dat) <- names(out)
dat <- rbindlist(dat, idcol = "model_id_nativeness", fill = T)

unique(dat$variable)
dat <- dat[, .(Citation, variable, response, se, model_id_nativeness)]

dat <-  merge(dat,
              sub_guide[, .(model_id_nativeness, analysis_group_category,  analysis_group,
                            nativeness_var)])
dat

unique(dat$variable)
dat[variable == "Intact_Africa", variable := "African assemblage"]

# >>> Prepare to plot ----------------------------------------------
# Facet wrapping is not going to work.
dat[, max_g := max(abs(response)),
    by = .(analysis_group)]
Ns <- dat[, .(n_refs = uniqueN(Citation)),
          by = .(variable,
                 analysis_group, analysis_group_category,
                 max_g, model_id_nativeness)]
Ns

unique(dat$variable)
unique(model_ests$variable)

dat$variable <- factor(dat$variable,
                       levels = c("Native", "African assemblage", "Introduced", "Invasive"))
model_ests$variable <- factor(model_ests$variable,
                              levels = c("Native", "African assemblage", "Introduced", "Invasive"))
lines.dat$variable <- factor(lines.dat$variable,
                             levels = c("Native","African assemblage", "Introduced", "Invasive"))
Ns$variable <- factor(Ns$variable,
                      levels = c("Native", "African assemblage", "Introduced", "Invasive"))

quaternary_palette <- c("Introduced" = "#57b7db", "African assemblage" = "#d7a4a3",  
                        "Invasive" = "#faae6b", "Native" = "#a7928c")


lvls <- c("Primary_Productivity",
          "Dead_Vegetation",
          "Litter_Cover", "Bare_Ground",
          "Soil_Compaction", "Soil_Moisture",
          "Soil_Total_C",
          "Soil_C:N", "Soil_Total_N",
          "Soil_Labile_N", "Soil_Total_P",
          "Soil_Total_Ca", "Soil_Total_Mg",
          
          
          "Invertebrate_Diversity", "Invertebrate_Abundance",
          "Invert_Herbivore_Abundance", "Invert_Predator_Abundance",
          "Invert_Detritivore_Abundance",
          
          "Vertebrate_Diversity", "Vertebrate_Abundance",
          "Vert_Herb_Abundance","Vert_Carn_Abundance",
          "Small_Mammal_Abundance",
          "Bird_Diversity", "Bird_Abundance")

setdiff(lvls, model_ests$analysis_group)
model_ests$analysis_group <- factor(model_ests$analysis_group,
                                    levels = (lvls))
dat$analysis_group <- factor(dat$analysis_group,
                                    levels = (lvls))
Ns$analysis_group <- factor(Ns$analysis_group,
                             levels = (lvls))
lines.dat$analysis_group <- factor(lines.dat$analysis_group,
                             levels = (lvls))

group_labels <- unique(model_ests$analysis_group)
group_labels <- gsub("_", " ", group_labels)
group_labels <- gsub("Invert ", "", group_labels)
names(group_labels) <- unique(model_ests$analysis_group)
group_labels <- gsub("Vert Carn ", "Carnivore ", group_labels)
group_labels <- gsub("Vert Herb ", "Herbivore ", group_labels)

group_labels2 <- unique(model_ests$variable) |> as.character()
names(group_labels2) <- unique(model_ests$variable)

group_labels <- c(group_labels,
                  group_labels2)
# >>> Vertebrates ---------------------------------------------------------
dat[, key := paste(variable, nativeness_var, sep = "_")]
lines.dat[, key := paste(variable, nativeness_var, sep = "_")]
model_ests[, key := paste(variable, nativeness_var, sep = "_")]

unique(model_ests$key)
to_plot <- c("African assemblage_Africa_Comparison",
             "Invasive_Invasive", "Introduced_Herbivore_nativeness",
             "Native_Herbivore_nativeness")

dat

patch <- c()
grps <- levels(droplevels(dat[analysis_group_category == "Vertebrates"]$analysis_group))

# Need to have free-er scales than facet-grid will allow us. So making plots
# in a loop.
for(i in 1:length(grps)){
  p <- ggplot()+
    geom_jitter(data = dat[analysis_group_category == "Vertebrates" &
                             key %in% to_plot &
                             analysis_group %in% grps[i], ],
                aes(x = se,
                    y = response, #residuals,
                    fill = variable),
                shape = 21,
                alpha = 0.5,
                size = 2.5)+
    geom_line(data = lines.dat[linetype_simple != "95% CI of estimate" &
                                 analysis_group_category == "Vertebrates" &
                                 key %in% to_plot &
                                 analysis_group %in% grps[i], ],
              aes(x = se, y = value,
                  color = variable,
                  linetype = linetype_simple,
                  group = interaction(linetype_simple, variable, linetype)))+
    scale_linetype_manual(name = NULL,
                          values = c("95% CI" = "dashed",
                                     "99% CI" = "dotted",
                                     "Model estimate" = "solid"))+
    # Model estimate:
    geom_errorbar(data = model_ests[analysis_group_category == "Vertebrates" &
                                      key %in% to_plot  &
                                      analysis_group %in% grps[i], ],
                  aes(ymin = mean_ll95, ymax = mean_ul95,
                      x = se), 
                  width = (max(model_ests[analysis_group_category == "Vertebrates"  &
                                            key %in% to_plot  &
                                            analysis_group %in% grps[i], ]$max_se) * .2))+
    geom_point(data = model_ests[analysis_group_category == "Vertebrates"  &
                                   key %in% to_plot  &
                                   analysis_group %in% grps[i], ],
               aes(y = estimate,
                   x = se, fill = variable),
               shape = 21, size = 4)+
    xlab("Standard Error")+
    ylab("Residuals")+
    scale_fill_manual(name = NULL,
                      values = quaternary_palette,
                      drop = F)+
    scale_color_manual(name = NULL,
                       values = quaternary_palette,
                       drop = F)+
    facet_grid(variable ~ analysis_group,
               labeller = as_labeller(group_labels))+
    theme_bw()+
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.text.y=element_blank()
    )+
    scale_x_reverse()+
    coord_flip()
  if(length(patch) == 0){
    patch <- p
  }else{
    patch <- patch + p
  }
}

patch + plot_layout(ncol = 4, guides = "collect")
  
ggsave("figures/supplement/Vertebrate funnel plots raw.pdf", width = 14, height = 12, dpi = 300)

# >>> Invertebrates -------------------------------------------------------

patch <- c()
grps <- levels(droplevels(dat[analysis_group_category == "Invertebrates"]$analysis_group))

# Need to have free-er scales than facet-grid will allow us. So making plots
# in a loop.
for(i in 1:length(grps)){
  p <- ggplot()+
    geom_jitter(data = dat[analysis_group_category == "Invertebrates" &
                             key %in% to_plot &
                             analysis_group %in% grps[i], ],
                aes(x = se,
                    y = response, #residuals,
                    fill = variable),
                shape = 21,
                alpha = 0.5,
                size = 2.5)+
    geom_line(data = lines.dat[linetype_simple != "95% CI of estimate" &
                                 analysis_group_category == "Invertebrates" &
                                 key %in% to_plot &
                                 analysis_group %in% grps[i], ],
              aes(x = se, y = value,
                  color = variable,
                  linetype = linetype_simple,
                  group = interaction(linetype_simple, variable, linetype)))+
    scale_linetype_manual(name = NULL,
                          values = c("95% CI" = "dashed",
                                     "99% CI" = "dotted",
                                     "Model estimate" = "solid"))+
    # Model estimate:
    geom_errorbar(data = model_ests[analysis_group_category == "Invertebrates" &
                                      key %in% to_plot  &
                                      analysis_group %in% grps[i], ],
                  aes(ymin = mean_ll95, ymax = mean_ul95,
                      x = se), 
                  width = (max(model_ests[analysis_group_category == "Invertebrates"  &
                                            key %in% to_plot  &
                                            analysis_group %in% grps[i], ]$max_se) * .2))+
    geom_point(data = model_ests[analysis_group_category == "Invertebrates"  &
                                   key %in% to_plot  &
                                   analysis_group %in% grps[i], ],
               aes(y = estimate,
                   x = se, fill = variable),
               shape = 21, size = 4)+
    xlab("Standard Error")+
    ylab("Residuals")+
    scale_fill_manual(name = NULL,
                      values = quaternary_palette,
                      drop = F)+
    scale_color_manual(name = NULL,
                       values = quaternary_palette,
                       drop = F)+
    facet_grid(variable ~ analysis_group,
               labeller = as_labeller(group_labels))+
    theme_bw()+
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.text.y=element_blank()
    )+
    scale_x_reverse()+
    coord_flip()
  if(length(patch) == 0){
    patch <- p
  }else{
    patch <- patch + p
  }
}

patch + plot_layout(ncol = 2, guides = "collect")

ggsave("figures/supplement/Invertebrate funnel plots raw.pdf", width = 12, height = 12, dpi = 300)

# >>> Ecosystem -----------------------------------------------------------

patch <- c()
grps <- levels(droplevels(dat[analysis_group_category == "Ecosystem"]$analysis_group))

# Need to have free-er scales than facet-grid will allow us. So making plots
# in a loop.
for(i in 1:length(grps)){
  p <- ggplot()+
    geom_jitter(data = dat[analysis_group_category == "Ecosystem" &
                             key %in% to_plot &
                             analysis_group %in% grps[i], ],
                aes(x = se,
                    y = response, #residuals,
                    fill = variable),
                shape = 21,
                alpha = 0.5,
                size = 2.5)+
    geom_line(data = lines.dat[linetype_simple != "95% CI of estimate" &
                                 analysis_group_category == "Ecosystem" &
                                 key %in% to_plot &
                                 analysis_group %in% grps[i], ],
              aes(x = se, y = value,
                  color = variable,
                  linetype = linetype_simple,
                  group = interaction(linetype_simple, variable, linetype)))+
    scale_linetype_manual(name = NULL,
                          values = c("95% CI" = "dashed",
                                     "99% CI" = "dotted",
                                     "Model estimate" = "solid"))+
    # Model estimate:
    geom_errorbar(data = model_ests[analysis_group_category == "Ecosystem" &
                                      key %in% to_plot  &
                                      analysis_group %in% grps[i], ],
                  aes(ymin = mean_ll95, ymax = mean_ul95,
                      x = se), 
                  width = (max(model_ests[analysis_group_category == "Ecosystem"  &
                                            key %in% to_plot  &
                                            analysis_group %in% grps[i], ]$max_se) * .2))+
    geom_point(data = model_ests[analysis_group_category == "Ecosystem"  &
                                   key %in% to_plot  &
                                   analysis_group %in% grps[i], ],
               aes(y = estimate,
                   x = se, fill = variable),
               shape = 21, size = 4)+
    xlab("Standard Error")+
    ylab("Residuals")+
    scale_fill_manual(name = NULL,
                      values = quaternary_palette,
                      drop = F)+
    scale_color_manual(name = NULL,
                       values = quaternary_palette,
                       drop = F)+
    facet_grid(variable ~ analysis_group,
               labeller = as_labeller(group_labels))+
    theme_bw()+
    theme(strip.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          strip.text.y=element_blank()
    )+
    scale_x_reverse()+
    coord_flip()
  if(length(patch) == 0){
    patch <- p
  }else{
    patch <- patch + p
  }
}

patch + plot_layout(ncol = 4, guides = "collect")
ggsave("figures/supplement/Ecosystem funnel plots raw.pdf", width = 12, height = 12, dpi = 300)
