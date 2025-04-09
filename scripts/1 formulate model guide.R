#' *March 19th, 2025*
#
#
#' [Formulate model guide]
#' *1. Cross join of factors of interest*
#' *2. Formulate formulas and all random effect combinations based on sample size and data structure within analysis_group subset*
#

rm(list = ls())
gc()

library("groundhog")
groundhog.day <- "2024-07-15"
libs <- c("data.table",
          "stringr", "dplyr")
groundhog.library(libs, groundhog.day)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------
# 0. Load data ------------------------------------------------------------

dat <- fread("data/master_data.csv")

# >>> Check something... --------------------------------------------------

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~ --------------------------------------------
# 1. Cross Join  -----------------------------------------------------
#' *Cross join of analysis groups, nativeness vars (the predictors), and random effects*
names(dat)

guide <- CJ(analysis_group = unique(dat$analysis_group), 
            nativeness_var = c("Herbivore_nativeness", "Invasive", "Africa_Comparison"),
            site_id = c("yes", "no"),
            species_response = c("yes", "no"),
            time_series = c("yes", "no"))
guide

guide[, model_comparison_id := paste(analysis_group, 
                                     nativeness_var,
                                     sep = "_"), 
      by = .(analysis_group, nativeness_var)]

guide[duplicated(model_comparison_id), ]
guide

#
guide.m <- merge(guide,
                 unique(dat[, .(analysis_group, analysis_group_category)]),
                 by = "analysis_group")
nrow(guide.m) == nrow(guide)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# 3. Create formulas ------------------------------------------------------

# >>> Fixed effects -------------------------------------------------------

guide.m[, formula_null := "yi_smd ~ 1"]

guide.m[, formula_nativeness := paste("yi_smd ~", nativeness_var)]
guide.m

# >>> Formulate exclusion -----------------------------------------------------

guide.m[, exclusion := paste0("!is.na(", nativeness_var, ") & analysis_group == ", "'", analysis_group, "'")]
unique(guide.m$exclusion)

unique(guide.m[nativeness_var == "Africa_Comparison"]$exclusion)

# >>> Random effect formulas -----------------------------------------------

guide.m[species_response == "no", random_effect := "list(~1 | Citation / data_point_ID"]
guide.m[species_response == "yes", random_effect := "list(~1 | Citation / species_response_tag / data_point_ID"]

guide.m

guide.m[site_id == "yes", random_effect := paste0(random_effect, ", ~1 | site_id")]

guide.m[time_series == "yes", random_effect := paste0(random_effect, ", ~time_series_clean | experiment_id")]

guide.m[, random_effect := paste0(random_effect, ")")]

unique(guide.m$random_effect)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Extract sample sizes  -------------------------------------------------
# Can do this on a simplified guide to save computation

sample_sizes <- list()
guide_simple <- unique(guide.m[, .(model_comparison_id, analysis_group, nativeness_var,
                                   exclusion)])
guide_simple[duplicated(model_comparison_id)]
#' *Should be 0 rows*

sub.dat <- c()
i <- 1

for(i in 1:nrow(guide_simple)){
  
  sub.dat <- dat[eval(parse(text = guide_simple[i, ]$exclusion)), ]
  sample_sizes[[i]] <- sub.dat[, .(n_obs = .N, n_refs = uniqueN(Citation)),
                               by = c(guide_simple[i, ]$nativeness_var)]
  
  # Now calculate min/max sample sizes and number of levels of nativeness_var
  sample_sizes[[i]] <- sample_sizes[[i]][, .(min_obs = min(n_obs),
                                              max_obs = max(n_obs),
                                              min_refs = min(n_refs),
                                              max_refs = max(n_refs),
                                              n_levels = .N)]
  sample_sizes[[i]][, `:=` (model_comparison_id = guide_simple[i, ]$model_comparison_id)]
  cat(i, "/", nrow(guide_simple), "\r")
}

sample_sizes <- rbindlist(sample_sizes)
sample_sizes


guide.m2 <- merge(guide.m,
                  sample_sizes,
                  by = "model_comparison_id")

guide.m2


# >>> Filter to >= 3 references -------------------------------------------
guide.m2 <- guide.m2[min_refs >= 3, ]
guide.m2

range(guide.m2$n_levels)
# Drop those with only 1 level of nativeness_var
guide.m2 <- guide.m2[n_levels == 2, ]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Melt --------------------------------------------------------------------

guide.mlt <- melt(guide.m2,
                  measure.vars = c("formula_null", "formula_nativeness"),
                  variable.name = "model_type",
                  value.name = "formula")
guide.mlt
guide.mlt[, model_type := gsub("formula_", "", model_type)]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Test formulas -----------------------------------------------------------

for(i in 1:nrow(guide.mlt)){
  as.formula(guide.mlt[i, ]$formula)
}
# If no error, then good to go.

unique(guide.mlt$random_effect)
setdiff(c("Citation", "data_point_ID", "species_response_tag", 
          "time_series_clean", "experiment_id", "site_id"), 
        names(dat))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ------------------------------------------
# Add model id & path ------------------------------------------------------------

guide.mlt[, model_id := paste0("model_", seq(1:.N))]
guide.mlt

guide.mlt[, model_path := paste0("outputs/main_text/models/", model_id, ".Rds")]
guide.mlt

saveRDS(guide.mlt, "outputs/main_text/data/master_guide.Rds")

