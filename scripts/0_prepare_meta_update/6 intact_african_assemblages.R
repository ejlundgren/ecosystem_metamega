#
#
# Classify systems as intact african assemblages
#
#
#

rm(list = ls())

library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          "sf","mapview"),
                  date = "2025-04-15")

dat <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")

unique(dat$Continent)

africa <- unique(dat[Continent == "Africa", .(data_point_ID, Site, Latitude, 
                                              Longitude, Herbivores_Manipulated, 
                                              Herbivore_manipulation_notes)])
herbivores <- readRDS("builds/sidecars/herbivore_traits.Rds")

herbivores

# >>> Tag whether megaherbivores are present -----------------------------------
herbivores <- herbivores[data_point_ID %in% africa$data_point_ID, ]

unique(herbivores[, .(Species, Mass.kg)])

herbivores[, megaherbivores_present := ifelse(any(.SD$Mass.kg >= 1000), "yes", "no"),
           by = .(data_point_ID)]

range(herbivores$no_species)

# >>> Tag whether <= 200kg animals are also present ------------------------

herbivores[, mesoherbivores_present := ifelse(any(.SD$Mass.kg <= 200), "yes", "no"),
           by = .(data_point_ID)]

herbivores[mesoherbivores_present == "no"]
herbivores[mesoherbivores_present == "yes"]


herbivores[mesoherbivores_present == "yes" & megaherbivores_present == "yes", Africa_Comparison := "Intact_Africa"]

sidecar <- unique(herbivores[!is.na(Africa_Comparison), .(data_point_ID, Africa_Comparison)])
sidecar


sidecar.final <- merge(dat[, .(data_point_ID, Herbivore_nativeness)],
                       sidecar,
                       all.x = T,
                       all.y = T,
                       by = 'data_point_ID')
sidecar.final[Herbivore_nativeness == "Introduced", Africa_Comparison := "Introduced"]
sidecar.final[, .(n = .N), by = Africa_Comparison]


saveRDS(sidecar.final, "builds/sidecars/intact_africa.Rds")

