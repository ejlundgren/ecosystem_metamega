
#
rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          'gsheet', 'metafor'),
                  date = "2025-04-15")


# Load dataset ------------------------------------------------------------

dat <- readRDS("data/final_data/updated_dataset_with_responses.Rds")
dat[, .(n = uniqueN(data_point_ID), refs = uniqueN(Citation))]

# >>> Load sidecars ---------------------------------------------------
invasive <- readRDS("builds/sidecars/GRIIS_Invasive_Status.Rds")
# For invasive we want invasive vs native
invasive[Invasive == "Other", Invasive := NA]

africa <- readRDS("builds/sidecars/intact_africa.Rds")
unique(africa$Africa_Comparison)

ids <- readRDS("builds/sidecars/non_independence_ids.Rds")

effects <- readRDS("builds/sidecars/effect_sizes.Rds")

# >>> Merge ---------------------------------------------------------------


dat.m <- merge(dat, 
               invasive,
               by = "data_point_ID",
               all.x = T) |>
  merge(africa[, .(data_point_ID, Africa_Comparison)],
        by = "data_point_ID",
        all.x = T) |>
  merge(ids[, .(data_point_ID, experiment_id, site_id, species_response_tag)],
        by = "data_point_ID",
        all.x = T) |>
  merge(effects[, .(data_point_ID, yi_smd, vi_smd)],
        by = "data_point_ID",
        all.x = T,
        all.y = F)
nrow(dat.m) == nrow(dat)

# These must be 0 rows:
dat.m[is.na(site_id)]
dat.m[is.na(species_response_tag)]
dat.m[is.na(experiment_id)]

dat.m[is.na(time_series_clean), ]
class(dat.m$time_series_clean)


# Filtering ---------------------------------------------------------------

dat.m <- dat.m[!is.na(yi_smd), ]

dat.m[grepl("Pollinator", analysis_group)]$analysis_group

# >>> Total number of observations and citations --------------------------

dat.m[, .(n = uniqueN(data_point_ID), refs = uniqueN(Citation))]

length(unique(dat.m$analysis_group))

unique(dat.m$Lit_Source)
dat.m[Lit_Source != "Web of Science October 2025", .(n = uniqueN(Citation), refs = uniqueN(data_point_ID))]

# >>> Filter out low inference experimental mechanisms --------------------

dat.m[experimental_mechanism %in% c("unknown mechanism", "landscape scale density gradient",
                                         "before after seasonal migration", "before after exclosure",
                                         "distance to water", "on trail vs off trail",
                                         "used vs unused patches", "before after range expansion") , 
      .(n = uniqueN(data_point_ID), refs = uniqueN(Citation))]
# Shame

unique(dat.m$experimental_mechanism)
dat.m <- dat.m[!experimental_mechanism %in% c("unknown mechanism", "landscape scale density gradient",
                                              "before after seasonal migration", "before after exclosure",
                                              "distance to water", "on trail vs off trail",
                                              "distal comparison with and without",
                                              "used vs unused patches", "before after range expansion")]
unique(dat.m$experimental_mechanism)
# unique(dat.m[experimental_mechanism == "distal comparison with and without"]$Citation)
# dat.m[experimental_mechanism == "distal comparison with and without"]
# dat.m[Citation == ]

# Lundgren et al 2022 was at a limiting resource (desert wetlands), which have already been excluded from primary dataset:
dat.m[Citation %in% "Lundgren et al 2022 Journal of Animal Ecology"]
dat.m <- dat.m[!Citation %in% "Lundgren et al 2022 Journal of Animal Ecology"]

dat.m <- dat.m

unique(dat.m$EXCLUSION)
nrow(dat.m)
dat.m <- dat.m[is.na(EXCLUSION)]
nrow(dat.m)

# We reviewed studies that don't match inclusion criteria:

"Effects of Heavy Browsing on a Bird Community in Deciduous Forest" # SUPPLEMENTAL FEEDING
dat.m <- dat.m[!Citation %in% "Effects of Heavy Browsing on a Bird Community in Deciduous Forest" ]

"Effect of white-tailed deer on songbirds within managed forests in Pennsylvania" # Small enclosures (65 hectares)
dat.m <- dat.m[!Citation %in% "Effect of white-tailed deer on songbirds within managed forests in Pennsylvania" ]

"A generalist rodent benefits from logging regardless of deer density" # 10 hectare enclosures
dat.m <- dat.m[!Citation %in% "A generalist rodent benefits from logging regardless of deer density" ]

"Influence of Grazing by Bison and Cattle on Deer Mice in Burned Tallgrass Prairie" # 5ha enclosures. Maybe need to exclude all the Konza work
dat.m <- dat.m[!Citation %in% "Influence of Grazing by Bison and Cattle on Deer Mice in Burned Tallgrass Prairie" ]

"Vegetational and Faunal Changes in an Area of Heavily Grazed Woodland Following Relief of Grazing" # 5.6 ha enclosure
dat.m <- dat.m[!Citation %in% "Vegetational and Faunal Changes in an Area of Heavily Grazed Woodland Following Relief of Grazing" ]

dat.m[grepl("Kansas", Site)]$Site
unique(dat.m[grepl("Kansas", Site)]$Citation)
dat.m <- dat.m[Site != "Konza Prairie Biological Station, Kansas, USA"]
# All of Konza is in small paddocks

# This should be excluded:
nrow(dat.m[Response == "Kvalseth odds diversity" & 
        Title == "Termites, Large Herbivores, and Herbaceous Plant Dominance Structure Small Mammal Communities in Savannahs"])
nrow(dat.m) - nrow(dat.m[!(Response == "Kvalseth odds diversity" & 
             Title == "Termites, Large Herbivores, and Herbaceous Plant Dominance Structure Small Mammal Communities in Savannahs")])
dat.m <- dat.m[!(Response == "Kvalseth odds diversity" & 
          Title == "Termites, Large Herbivores, and Herbaceous Plant Dominance Structure Small Mammal Communities in Savannahs")]


dat.m
nrow(dat.m[Title == "The effect of ungulate grazing on a small mammal community in southeastern Botswana" &
        Response == "Biomass"]) # This is actually body mass, not biomass.

nrow(dat.m) - nrow(dat.m[!(Title == "The effect of ungulate grazing on a small mammal community in southeastern Botswana" &
        Response == "Biomass")])
dat.m <- dat.m[!(Title == "The effect of ungulate grazing on a small mammal community in southeastern Botswana" &
          Response == "Biomass")]

# This paper is dominantly domestic horses and livestock
dat.m <- dat.m[Citation != "Putman et al. 1989 Biological Conservation"]

# Exclude < 1 year -------------------------------------------
dat.m$treatment_duration_days

dat.m[treatment_duration_days < 365, ]$treatment_duration_days

dat.m[is.na(treatment_duration_days), treatment_duration_days := Inf]

nrow(dat.m) - nrow(dat.m[treatment_duration_days < 365, ]) == nrow(dat.m[treatment_duration_days >= 365, ])
# must be true

dat.m <- dat.m[treatment_duration_days >= 365, ]

# >>> Filter out seeds ---------------------------------------------------

dat.m <- dat.m[!age_class %in% c("Seeds", "seed", "Seed", "seeds", "Seedbank", "seedbank")]

# >>> Drop plant responses (reported in Lundgren et al. 2024) -------------

unique(dat.m[analysis_group_category == "Plants"]$analysis_group)


dat.m[Lit_Source == "Web of Science October 2025", .(n = uniqueN(data_point_ID),
                                                     refs = uniqueN(Citation))]

dat.m[, .(n = uniqueN(data_point_ID),
          refs = uniqueN(Citation))]


# >>> Save full dataset (including plants) --------------------------------

saveRDS(dat.m, "builds/analysis_ready/full_meta_analysis_including_plants.Rds")


dat.m <- dat.m[analysis_group_category != "Plants", ]
dat.m


dat.m[, .(n = uniqueN(data_point_ID),
          refs = uniqueN(Citation))]


# >>> Rename yi/vi columns -----------------------------------

setnames(dat.m, c("yi_smd", "vi_smd"), c("yi", "vi"))

# >>> Save ----------------------------------------------------------------
saveRDS(dat.m, "builds/analysis_ready/analysis_ready_dataset.Rds")
