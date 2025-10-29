
rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          'gsheet', 'metafor'),
                  date = "2025-04-15")


# Load dataset ------------------------------------------------------------

dat <- readRDS("data/final_data/updated_dataset_with_responses.Rds")


# >>> Merge in sidecars ---------------------------------------------------
invasive <- readRDS("builds/sidecars/GRIIS_Invasive_Status.Rds")

africa <- readRDS("builds/sidecars/intact_africa.Rds")

ids <- readRDS("builds/sidecars/non_independence_ids.Rds")

dat.m <- merge(dat, 
               invasive,
               by = "data_point_ID",
               all.x = T) |>
  merge(africa[, .(data_point_ID, Africa_Comparison)],
        by = "data_point_ID",
        all.x = T) |>
  merge(ids[, .(data_point_ID, experiment_id, site_id)],
        by = "data_point_ID",
        all.x = T)
nrow(dat.m) == nrow(dat)

dat.m


# >>> Filter to responses with n >= 3 per native/introduced -------------------------------------
# (more will have to be filtered downstream) 

resp <- dat.m[, .(n_citations = uniqueN(Citation)),
              by = .(analysis_group,
                     Herbivore_nativeness)]

resp[, min_citations := min(n_citations), by = .(analysis_group)]
resp <- resp[min_citations >= 3, ]$analysis_group |> unique()
resp

dat.m <- dat.m[analysis_group %in% resp, ] 
dat.m


# >>> Drop plant responses (reported in Lundgren et al. 2024) -------------

unique(dat[analysis_group_category == "Plants"]$analysis_group)

dat.m <- dat[analysis_group_category != "Plants", ]
dat.m


# >>> Total number of observations and citations --------------------------

dat.m[, .(n = .N, refs = uniqueN(Citation))]

# >>> Save ----------------------------------------------------------------

saveRDS(dat.m, "builds/analysis_ready/analysis_ready_dataset.Rds")
