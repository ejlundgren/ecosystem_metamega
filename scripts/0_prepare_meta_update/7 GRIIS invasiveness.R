#
#
# Query TDWG GRIIS invasiveness designations
#
#

rm(list = ls())


# Load and clean workspace ------------------------------------------------
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          "sf","mapview"),
                  date = "2025-04-15")
source("scripts/helper_scripts/helper scripts.R")

# Load herbivore data:
dat <- readRDS("builds/sidecars/herbivore_traits.Rds")

# Load master sheet:
master <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")
master


# >>> Merge in lat/longs & make spatial --------------------------------------------------
dat.mrg <- merge(dat,
                 master[, .(data_point_ID, Site, Latitude, Longitude)],
                 by = "data_point_ID",
                 all.x = T)
nrow(dat.mrg) == nrow(dat)

# Make a simplified subset of herbivores:
herbs <- unique(dat.mrg[Herbivore_nativeness == "Introduced", 
                        .(data_point_ID, Site, Latitude, Longitude, Species, Herbivore_nativeness, relative_species_biomass_loss)])
herbs[is.na(Latitude), ]

# Make spatial:
herbs.sf <- st_as_sf(herbs,
                         coords = c("Longitude", "Latitude"), crs = 4326)
herbs.sf

# Load & filter GRIIS --------------------------------------------------------------

griis <- fread("data/GRIIS/GRIIS - Country Compendium V1_0.csv")
griis
unique(griis$kingdom)

griis <- griis[kingdom == "Animalia", ]
unique(griis$phylum)
griis <- griis[phylum == "Chordata", ]
griis
unique(griis$class)
griis <- griis[class == "Mammalia", ]
griis
unique(griis$isInvasive)
griis <- griis[isInvasive == "INVASIVE", ]
griis

griis <- unique(griis[, .(species,country, countryCode_alpha2, countryCode_alpha3,
                          isInvasive)])

griis[, .(n = .N), by = .(countryCode_alpha3, species, isInvasive)][n > 1, ]

# Merge country code into herbivores---------------------------------------------------
library("mapview")
countries <- st_read("data/GRIIS/world_administrative_boundaries.gpkg")

countries <- countries %>% st_make_valid()
mapview(countries)
unique(countries$region)
countries <- countries %>%
  rename(cont = continent)

sf_use_s2(TRUE)
herbs.sf.jnd <- herbs.sf %>%
  st_buffer(1000) %>%
  st_join(countries,
          left = TRUE)
herbs.sf.jnd

# Check NAs
x <- herbs.sf.jnd %>% filter(is.na(iso3)) 
unique(x)

# Convert back to point to make this more tractable...
x <- x %>%
  st_centroid()
mapview(x[1])

griis %>% 
  filter(grepl("Rapa", country))

sort(unique(griis$country))
herbs.sf.jnd <- herbs.sf.jnd %>%
  mutate(iso3 = ifelse(is.na(iso3) &
                         Site %in% c("Assateague Island, North Carolina", 
                                     "Asseateague Island, North Carolina",
                                     "Shackleford Banks", "Shackleford Banks",
                                     "Cumberland Island, Georgia, USA"),
                       "USA", iso3)) %>%
  mutate(iso3 = ifelse(is.na(iso3) &
                         Site %in% c("Mona Island, Caribbean"),
                       "PRI", iso3)) %>%
  mutate(iso3 = ifelse(is.na(iso3) &
                         Site %in% c("Sable Island"),
                       "CAN", iso3)) %>%
  mutate(iso3 = ifelse(is.na(iso3) &
                         Site %in% c("Rapa Iti"),
                       "PYF", iso3)) 
herbs.sf.jnd %>% filter(is.na(iso3))

setdiff(herbs.sf.jnd$iso3, griis$countryCode_alpha3)
setdiff(griis$countryCode_alpha3, herbs.sf.jnd$iso3)

herbs.sf.jnd.dat <- herbs.sf.jnd %>%
  as.data.frame() %>%
  mutate(geometry = NULL)
setDT(herbs.sf.jnd.dat)

# Check differences in taxonomy -------------------------------------------
herbs.sf.jnd.dat %>% filter(grepl("Equus", Species))
griis %>% filter(grepl("Equus", species))

setdiff(herbs.sf.jnd.dat$Species,
        griis$species)
# these are all solid species, ok, so not listed as invasive.

# >>> Convert from trinomial names to binomial ----------------------------

herbs.sf.jnd.dat[, Species_key := Species]
herbs.sf.jnd.dat[, Species_key := convertFeralNamesVec(species_vec = Species,
                                                       from = "trinomial",
                                                       to = "feral_binomial")]
herbs.sf.jnd.dat

setdiff(herbs.sf.jnd.dat$Species_key,
        griis$species)
unique(herbs.sf.jnd.dat$Species_key)


# Merge invasive status in ------------------------------------------------
griis[, key := paste0(species, countryCode_alpha3)]
herbs.sf.jnd.dat[, key := paste0(Species_key, iso3)]

herbs.sf.jnd.dat

herbs.sf.jnd.dat %>% filter(key %in% griis$key) %>%
  pull(Species_key) %>%
  unique()

herbs.sf.jnd.dat[, Invasive := ifelse(key %in% griis$key,
                                        "Invasive", "Other")] 
herbs.sf.jnd.dat

#
herbs.sf.jnd.dat

# Reconcile to be one Invasive value per data point. If possible or necessary. ----------------------------------------------------------------

herbs.sf.jnd.dat[, .(lvls = uniqueN(Invasive)),
             by = data_point_ID][lvls > 1, ]

herbs.sf.jnd.dat[, lvls := uniqueN(Invasive),
             by = data_point_ID]
herbs.sf.jnd.dat[lvls > 1, ]

# >>> Cast wide, filling by relative biomass ------------------------------
herbs.sf.jnd.dat[, has_biomass := ifelse(all(is.na(relative_species_biomass_loss)), "no", "yes"),
                 by = .(data_point_ID)]

biomass_summary <- herbs.sf.jnd.dat[, .(total_biomass_loss = sum(relative_species_biomass_loss, na.rm = T),
                                        n_species = uniqueN(Species)),
                                    by = .(has_biomass, data_point_ID, Invasive)]

biomass_summary.wide <- dcast(biomass_summary,
                               data_point_ID + has_biomass ~ Invasive,
                               value.var = c("total_biomass_loss", "n_species"))
#
biomass_summary.wide[has_biomass == "no", `:=` (total_biomass_loss_Invasive = NA,
                                                total_biomass_loss_Other = NA,
                                                n_species_Other = ifelse(is.na(n_species_Other), 0, n_species_Other),
                                                n_species_Invasive = ifelse(is.na(n_species_Invasive), 0, n_species_Invasive))]

biomass_summary.wide[has_biomass == "yes", `:=` (total_biomass_loss_Invasive = ifelse(is.na(total_biomass_loss_Invasive), 0, total_biomass_loss_Invasive),
                                                total_biomass_loss_Other = ifelse(is.na(total_biomass_loss_Other), 0, total_biomass_loss_Other),
                                                n_species_Other = NA,
                                                n_species_Invasive = NA)]
biomass_summary.wide

biomass_summary.wide[, Invasive := fcase(has_biomass == "yes" & total_biomass_loss_Invasive >= total_biomass_loss_Other, "Invasive",
                                         has_biomass == "yes" & total_biomass_loss_Invasive < total_biomass_loss_Other, "Other",
                                         has_biomass == "no" & n_species_Invasive >= n_species_Other, "Invasive",
                                         has_biomass == "no" & n_species_Invasive < n_species_Other, "Other")]
unique(biomass_summary.wide$Invasive)

biomass_summary.wide[, .(n = .N), by = .(Invasive)]

biomass_summary.wide

# Merge into dataset to create an Invasive sidecar ------------------------

invasive_sidecar <- merge(master[, .(data_point_ID, Herbivore_nativeness)],
                          biomass_summary.wide[, .(data_point_ID, Invasive)],
                          all.x = T,
                          by = "data_point_ID")
nrow(invasive_sidecar) == nrow(master)
invasive_sidecar[is.na(Invasive), Invasive := Herbivore_nativeness]
unique(invasive_sidecar$Herbivore_nativeness)

invasive_sidecar
# Save --------------------------------------------------------------------

saveRDS(invasive_sidecar[, .(data_point_ID, Invasive)],
        "builds/sidecars/GRIIS_Invasive_Status.Rds")
