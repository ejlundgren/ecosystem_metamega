
rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          'gsheet', 'metafor'),
                  date = "2025-04-15")

full_dat <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")

dat <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1z1AqBHsy8_nYz22PIE5-hiKGLpFzZ2pGQ1zjK8pN2P4/edit?gid=1082046001#gid=1082046001")

dat
setDT(dat)
#
#
unique(dat[Response_Highest_Level == "DIVERSITY", .(response_key,
                                                  analysis_group_category, eco_response_coarse,
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

# 

# View(unique(dat[grepl("shrub", Species_Class, ignore.case = T) & Response_Highest_Level %in% c("DIVERSITY", "ABUNDANCE") &
#       Response_Sphere == "vegetation", 
#       .(response_key, Species_Class, woody_height_class, Strata_or_soil_depth,
#         analysis_group_category, eco_response_coarse,
#         eco_response_fine, eco_response_fine2,
#         eco_response_fine3, eco_response_fine4)]))

saveRDS(dat, "data/literature_update/preliminary/responses_categorized.Rds")

dat[, `:=` (analysis_group_category = gsub(" ", "_", analysis_group_category),
            eco_response_coarse = gsub(" ", "_", eco_response_coarse),
            eco_response_fine = gsub(" ", "_", eco_response_fine),
            eco_response_fine2 = gsub(" ", "_", eco_response_fine2),
            eco_response_fine3 = gsub(" ", "_", eco_response_fine3),
            eco_response_fine4 = gsub(" ", "_", eco_response_fine4))]

dput(names(dat))

full_dat <- full_dat[, !c("Response_Highest_Level", "Response_Cat", "Response_Sphere", 
                     "Species_Class", "Species_Nativeness", "Strata_or_soil_depth", 
                     "woody_height_class", "age_class", "Species_Level", "Species", 
                     "Response", "Units", "Response_Dimension", "Response_Living", 
                     "analysis_group_category", "eco_response_coarse", "eco_response_fine", 
                     "eco_response_fine2", "eco_response_fine3", "eco_response_fine4")]

full_dat.mrg <- merge(full_dat,
                      dat,
                      by = "response_key",
                      all.x = T)
full_dat.mrg
nrow(full_dat) == nrow(full_dat.mrg)


# >>> Exclude some responses (seedlings and seeds) ------------------------

full_dat.mrg <- full_dat.mrg[!grepl("seed", Response, ignore.case = T) & !grepl("seedling", Response, ignore.case = T)]
full_dat.mrg

# >>> Final checks ----------------------------------------------------------------

full_dat.mrg

x <- unique(full_dat.mrg[analysis_group_category == "Plants", .(eco_response_coarse,
                                                           eco_response_fine,
                                                           eco_response_fine2,
                                                           eco_response_fine3,
                                                           eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
View(x)



x <- unique(full_dat.mrg[analysis_group_category == "Plant_Nutrients", .(eco_response_coarse,
                                                                eco_response_fine,
                                                                eco_response_fine2,
                                                                eco_response_fine3,
                                                                eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
View(x)

x <- unique(full_dat.mrg[analysis_group_category == "Soil", .(eco_response_coarse,
                                                                eco_response_fine,
                                                                eco_response_fine2,
                                                                eco_response_fine3,
                                                                eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
View(x)


x <- unique(full_dat.mrg[analysis_group_category == "Ecosystem", .(eco_response_coarse,
                                                              eco_response_fine,
                                                              eco_response_fine2,
                                                              eco_response_fine3,
                                                              eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
View(x)



x <- unique(full_dat.mrg[analysis_group_category == "Vertebrates", .(eco_response_coarse,
                                                                   eco_response_fine,
                                                                   eco_response_fine2,
                                                                   eco_response_fine3,
                                                                   eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
View(x)


x <- unique(full_dat.mrg[analysis_group_category == "Invertebrates", .(eco_response_coarse,
                                                                     eco_response_fine,
                                                                     eco_response_fine2,
                                                                     eco_response_fine3,
                                                                     eco_response_fine4)])
setorder(x, eco_response_coarse, eco_response_fine, eco_response_fine2, eco_response_fine3, eco_response_fine4)
View(x)


sort(unique(full_dat.mrg$eco_response_coarse))
sort(unique(full_dat.mrg$eco_response_fine))
full_dat.mrg[eco_response_fine == "Non_vascular_abundance", eco_response_fine := "Non_Vascular_Abundance"]

sort(unique(full_dat.mrg$eco_response_fine2))
sort(unique(full_dat.mrg$eco_response_fine3))
sort(unique(full_dat.mrg$eco_response_fine4))


# >>> Melt ----------------------------------------------------------------
full_dat.mrg[, response_key := NULL]


full_dat.mrg.mlt <- data.table::melt(full_dat.mrg[, .(data_point_ID, Citation, Title,
                                          Lit_Source, Continent, island,
                                          experimental_mechanism, Latitude, Longitude,
                                          Site, Herbivores_Manipulated, Herbivore_nativeness,
                                          treatment_duration_days, impact_mechanism,
                                          Fig_Num, High_value_equals_high_response,
                                          Response_Dimension, Response_Living,
                                          Response_Highest_Level, Species_Class, Species, Response, Units,
                                          Plot_Covariate, time_series_clean,
                                          Mean_High_Megafauna, Mean_Low_Megafauna,
                                          SD_High_Megafauna, SD_Low_Megafauna,
                                          N_High_Megafauna, N_Low_Megafauna,
                                          yi_smd, vi_smd, analysis_group_category,
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


unique(full_dat.mrg.mlt$analysis_group_category)
unique(full_dat.mrg.mlt[is.na(analysis_group_category), ]$analysis_group)

saveRDS(full_dat.mrg.mlt, "data/final_data/updated_dataset_with_responses.Rds")
