
#
#
#
rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                           'metafor'),
                  date = "2025-04-15")

full_dat <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")

# Create a response file for manual tagging of responses ------------------


responses <- unique(full_dat[, c("data_point_ID", "Response_Highest_Level", "Response_Cat", "Response_Sphere", 
                                     "Species_Class", "Species_Nativeness", "Strata_or_soil_depth", 
                                     "woody_height_class", "age_class", "Species_Level", "Species", 
                                     "Response", "Units", "Response_Dimension", "Response_Living", 
                                     "analysis_group_category", "eco_response_coarse", "eco_response_fine", 
                                     "eco_response_fine2", "eco_response_fine3", "eco_response_fine4"), with = F])
responses[duplicated(data_point_ID)]
# Must be 0 rows

setorder(responses, analysis_group_category, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3)

fwrite(responses, "builds/temp/responses_for_double_checking.csv")

# Read in workup --------------------------------------------------------------------

dat <- read.csv("builds/temp/responses_for_double_checking_workup.csv")
#' [fread() is skipping a line of the csv for some reason]

setDT(dat)

full_dat[!data_point_ID %in% dat$data_point_ID]

full_dat[data_point_ID %in% dat$data]
# Thank god.


dat[, `:=` (analysis_group_category = gsub(" ", "_", analysis_group_category),
            eco_response_coarse = gsub(" ", "_", eco_response_coarse),
            eco_response_fine = gsub(" ", "_", eco_response_fine),
            eco_response_fine2 = gsub(" ", "_", eco_response_fine2),
            eco_response_fine3 = gsub(" ", "_", eco_response_fine3),
            eco_response_fine4 = gsub(" ", "_", eco_response_fine4))]

# Check levels in R -------------------------------------------------------

unique(dat[Response_Highest_Level == "DIVERSITY", .(analysis_group_category, eco_response_coarse,
                                                 eco_response_fine, eco_response_fine2,
                                                 eco_response_fine3, eco_response_fine4)])
dat[Response_Highest_Level == "DIVERSITY" & is.na(eco_response_coarse)]

# Not analyzing functional diversity or evenness

unique(dat[Response_Highest_Level == "ABUNDANCE", .(
  analysis_group_category, eco_response_coarse,
  eco_response_fine, eco_response_fine2,
  eco_response_fine3, eco_response_fine4)])
dat[Response_Highest_Level == "ABUNDANCE" & is.na(eco_response_coarse)]


#
dat[analysis_group_category == "Soil" & is.na(eco_response_coarse)]

sort(unique(dat[analysis_group_category == "Soil" ]$eco_response_coarse))


# Merge -------------------------------------------------------------------
setdiff(full_dat$data_point_ID, dat$data_point_ID)
#
full_dat.mrg <- merge(full_dat[, !c("Response_Highest_Level", "Response_Cat", "Response_Sphere", 
                                 "Species_Class", "Species_Nativeness", "Strata_or_soil_depth", 
                                 "woody_height_class", "age_class", "Species_Level", "Species", 
                                 "Response", "Units", "Response_Dimension", "Response_Living", 
                                 "analysis_group_category", "eco_response_coarse", "eco_response_fine", 
                                 "eco_response_fine2", "eco_response_fine3", "eco_response_fine4"), with = F],
                      dat[, c("data_point_ID", "Response_Highest_Level", "Response_Cat", "Response_Sphere", 
                               "Species_Class", "Species_Nativeness", "Strata_or_soil_depth", 
                               "woody_height_class", "age_class", "Species_Level", "Species", 
                               "Response", "Units", "Response_Dimension", "Response_Living", 
                               "analysis_group_category", "eco_response_coarse", "eco_response_fine", 
                               "eco_response_fine2", "eco_response_fine3", "eco_response_fine4"), with = F],
                      by = "data_point_ID",
                      all.x = T)
full_dat.mrg
nrow(full_dat) == nrow(full_dat.mrg)
# Must be TRUE

full_dat.mrg[eco_response_coarse == "Fungi_Abundance"]

full_dat.mrg[eco_response_coarse %in% c("Microbe_Abundance", "Fungi_Abundance"), 
             .(n = .N, refs = uniqueN(Citation)), by = .(eco_response_coarse, eco_response_fine)]


# >>> Final checks ----------------------------------------------------------------

full_dat.mrg

x <- unique(full_dat.mrg[analysis_group_category == "Plants", .(Response_Highest_Level,
                                                                Response_Cat,
                                                                eco_response_coarse,
                                                           eco_response_fine,
                                                           eco_response_fine2,
                                                           eco_response_fine3,
                                                           eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
# View(x)


x <- unique(full_dat.mrg[analysis_group_category == "Plant_Nutrients", .(eco_response_coarse,
                                                                eco_response_fine,
                                                                eco_response_fine2,
                                                                eco_response_fine3,
                                                                eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
# View(x)

full_dat.mrg[eco_response_coarse == "Plant_N_P", eco_response_coarse := "Plant_N:P"]
full_dat.mrg[eco_response_coarse == "Plant_Total_N", eco_response_coarse := "Plant_N"]

full_dat.mrg[analysis_group_category == "Plant_Nutrients", eco_response_fine := NA ]

x <- unique(full_dat.mrg[analysis_group_category == "Soil", .(eco_response_coarse,
                                                                eco_response_fine,
                                                                eco_response_fine2,
                                                                eco_response_fine3,
                                                                eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
# View(x)


x <- unique(full_dat.mrg[analysis_group_category == "Ecosystem", .(eco_response_coarse,
                                                              eco_response_fine,
                                                              eco_response_fine2,
                                                              eco_response_fine3,
                                                              eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
# View(x)



x <- unique(full_dat.mrg[analysis_group_category == "Vertebrates", .(eco_response_coarse,
                                                                   eco_response_fine,
                                                                   eco_response_fine2,
                                                                   eco_response_fine3,
                                                                   eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
# View(x)


x <- unique(full_dat.mrg[analysis_group_category == "Invertebrates", .(eco_response_coarse,
                                                                     eco_response_fine,
                                                                     eco_response_fine2,
                                                                     eco_response_fine3,
                                                                     eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
# View(x)


sort(unique(full_dat.mrg$eco_response_coarse))
sort(unique(full_dat.mrg$eco_response_fine))
full_dat.mrg[eco_response_fine == "Non_vascular_abundance", eco_response_fine := "Non_Vascular_Abundance"]

sort(unique(full_dat.mrg$eco_response_fine2))
sort(unique(full_dat.mrg$eco_response_fine3))
sort(unique(full_dat.mrg$eco_response_fine4))


full_dat.mrg
full_dat.mrg[grepl("Moon", Citation) & eco_response_fine == "Litter_Cover", ]

# >>> Melt ----------------------------------------------------------------
names(full_dat.mrg)

full_dat.mrg.mlt <- data.table::melt(full_dat.mrg[, .(EXCLUSION, data_point_ID, Citation, Title,
                                                      Lit_Source, Continent, island,
                                                      experimental_mechanism, Latitude, Longitude,
                                                      Site, Herbivores_Manipulated, Herbivore_nativeness,
                                                      treatment_duration_days, impact_mechanism,
                                                      Fig_Num, High_value_equals_high_response,
                                                      Response_Dimension, Response_Living,
                                                      Response_Highest_Level, Strata_or_soil_depth,
                                                      Species_Class, Species, Response, Units, age_class, woody_height_class,
                                                      Plot_Covariate, time_series_clean,
                                                      Mean_High_Megafauna, Mean_Low_Megafauna,
                                                      SD_High_Megafauna, SD_Low_Megafauna,
                                                      N_High_Megafauna, N_Low_Megafauna,
                                                      analysis_group_category,
                                                      eco_response_coarse, eco_response_fine, 
                                                      eco_response_fine2, eco_response_fine3,
                                                      eco_response_fine4)],
                                     measure.vars = c("eco_response_coarse", "eco_response_fine", 
                                                      "eco_response_fine2", "eco_response_fine3",
                                                      "eco_response_fine4"),
                                     value.name = "analysis_group")


full_dat.mrg.mlt


full_dat.mrg.mlt <- full_dat.mrg.mlt[!is.na(analysis_group), ]
full_dat.mrg.mlt$variable <- NULL

full_dat.mrg.mlt[analysis_group_category == "", ]

full_dat.mrg.mlt <- full_dat.mrg.mlt[analysis_group != "", ]
full_dat.mrg.mlt


unique(full_dat.mrg.mlt$analysis_group_category)
unique(full_dat.mrg.mlt[is.na(analysis_group_category), ]$analysis_group)


# >>> More checks --------------------------------------------------------

test <- unique(full_dat.mrg.mlt[, .(analysis_group, analysis_group_category)])
test[, .(uniqueN(analysis_group_category)), analysis_group][V1 > 1, ]

unique(full_dat.mrg.mlt[analysis_group == "N_Mineralization_Rate", ]$analysis_group_category)
full_dat.mrg.mlt[analysis_group %in% c("N_Mineralization_Rate",
                                       "Nitrification_Rate", 
                                       "Denitrification_Rate",
                                       "CO2_Respiration"), analysis_group_category := "Soil"]

unique(full_dat.mrg.mlt[analysis_group == "Native_Plant_Abundance", ]$analysis_group_category)
full_dat.mrg.mlt[analysis_group == "Native_Plant_Abundance" & analysis_group_category == "Microbes", ]
full_dat.mrg.mlt[analysis_group == "Ecosystem_Respiration", ]$analysis_group_category


unique(full_dat.mrg.mlt[analysis_group == "Plant_N"]$Response)
unique(full_dat.mrg.mlt[analysis_group == "Plant_Total_N"]$Response)
full_dat.mrg.mlt[analysis_group == "Plant_Total_N", analysis_group := "Plant_N"]

test <- unique(full_dat.mrg.mlt[, .(analysis_group, analysis_group_category)])
test[, .(uniqueN(analysis_group_category)), analysis_group][V1 > 1, ]


unique(full_dat.mrg.mlt[analysis_group == 'Dead_Vegetation', ]$analysis_group_category)
full_dat.mrg.mlt[analysis_group == "Dead_Vegetation", analysis_group_category := "Ecosystem"]

full_dat.mrg.mlt[analysis_group == "Root_Biomass", analysis_group_category := "Soil"]
full_dat.mrg.mlt[analysis_group == "Primary_Productivity", analysis_group_category := "Ecosystem"]
full_dat.mrg.mlt[analysis_group == "Soil_Respiration", analysis_group_category := "Soil"]
full_dat.mrg.mlt[analysis_group == "Net_Primary_Productivity", analysis_group_category := "Ecosystem"]
full_dat.mrg.mlt[analysis_group == "Belowground_Primary_Productivity", analysis_group_category := "Ecosystem"]
full_dat.mrg.mlt[analysis_group == "Primary_Productivity", analysis_group_category := "Ecosystem"]

full_dat.mrg.mlt <- full_dat.mrg.mlt[!grepl("Relative", analysis_group)]

sort(unique(full_dat.mrg.mlt$analysis_group))

# >>> Drop some duplicate responses: --------------------------------------
# These are basically identical data points as broader categories
full_dat.mrg.mlt <- full_dat.mrg.mlt[!analysis_group %in% c("Aboveground_Primary_Productivity", "Mamm_SmallHerb_Abundance", "TerrestrialBird_Abundance"), ]


# >>> Save ----------------------------------------------------------------

saveRDS(full_dat.mrg.mlt, "data/final_data/updated_dataset_with_responses.Rds")


full_dat.mrg.mlt[grepl("Mg", analysis_group), ]$analysis_group

# >>> Test ----------------------------------------------------------------

full_dat.mrg.mlt[analysis_group == "Small_Mammal_Abundance", .(n = uniqueN(data_point_ID), refs = uniqueN(Citation)),
                 by = .(Herbivore_nativeness)]
full_dat.mrg.mlt

unique(full_dat.mrg.mlt$EXCLUSION)


