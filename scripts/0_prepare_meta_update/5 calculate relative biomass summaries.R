
#
#
# Process the concatenated herbivore densities into relative biomass for downstream variables (e.g., invasiveness) 
#
#
rm(list = ls())

library('data.table')
library("tidyr")

# Process new Web of Science 2025 data and bind with the old long herbivore dataset

# Load data:
dat <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")
unique(dat$Lit_Source)
dat <- dat[Lit_Source == "Web of Science October 2025"]

original <- readRDS("data/Trepel_2024/herbivore_relative_biomass.Rds")
original

traits <- fread("data/HerbiTraits_1.1/HerbiTraits_1.1.csv")

# Plan:
# 1. Separate and melt Herbivores_Manipulated
# 2. Separate and melt DENSITY_High
# 3. Separate and melt DENSITY_Low
# Merge together.

# 1. Make Herbivores_Manipulated LONG ------------------------------------------------
unique(dat$Herbivore_nativeness)
herbs <- unique(dat[, .(data_point_ID, Herbivore_nativeness, Site, Continent,
                      Herbivores_Manipulated)])
herbs
#
unique(herbs$DENSITY_Low_Megafauna)
# Ok, only 1 study that had just reduced densities in the low treatment.

herbs.mlt <- herbs %>%
  separate_longer_delim(cols = Herbivores_Manipulated,
                        delim = "; ") %>%
  mutate(Herbivores_Manipulated = trimws(Herbivores_Manipulated))

herbs.mlt
setDT(herbs.mlt)

herbs.mlt
setnames(herbs.mlt, "Herbivores_Manipulated", "Species")
herbs.mlt

# 2. Make High DENSITY long ----------------------------------------------------

high_dens <-  unique(dat[!is.na(DENSITY_High_Megafauna), .(data_point_ID,
                                                            DENSITY_High_Megafauna,
                                                           animal_density_units, areal_density_units)])
high_dens[is.na(animal_density_units) ]
high_dens[is.na(areal_density_units) ]

unique(high_dens$DENSITY_High_Megafauna)

high_dens.mlt <- high_dens %>%
  separate_longer_delim(cols = DENSITY_High_Megafauna,
                        delim = "; ") %>%
  mutate(DENSITY_High_Megafauna = trimws(DENSITY_High_Megafauna))

high_dens.mlt
setDT(high_dens.mlt)

# Split out the units from the species.
unique(high_dens.mlt$DENSITY_High_Megafauna)

high_dens.mlt[, Species := word(DENSITY_High_Megafauna,
                                2, 3, " ")]
high_dens.mlt
high_dens.mlt[, DENSITY_High_Megafauna := word(DENSITY_High_Megafauna,
                                               start = 1, end = 1, " ")]
unique(high_dens.mlt$DENSITY_High_Megafauna)
high_dens.mlt[, DENSITY_High_Megafauna := as.numeric(DENSITY_High_Megafauna)]

# 3. Make DENSITY-low long ---------------------------------------------------

low_dens <-  unique(dat[!is.na(DENSITY_High_Megafauna), .(data_point_ID,
                                                           DENSITY_Low_Megafauna)])

unique(low_dens$DENSITY_Low_Megafauna)


low_dens.mlt <- low_dens %>%
  separate_longer_delim(cols = DENSITY_Low_Megafauna,
                        delim = "; ") %>%
  mutate(DENSITY_Low_Megafauna = trimws(DENSITY_Low_Megafauna))

low_dens.mlt
setDT(low_dens.mlt)

# Split out the units from the species.
unique(low_dens.mlt$DENSITY_Low_Megafauna)

low_dens.mlt[, Species := word(DENSITY_Low_Megafauna,
                                2, 3, " ")]
low_dens.mlt
low_dens.mlt[, DENSITY_Low_Megafauna := word(DENSITY_Low_Megafauna,
                                               start = 1, end = 1, " ")]
unique(low_dens.mlt$DENSITY_Low_Megafauna)
low_dens.mlt[, DENSITY_Low_Megafauna := as.numeric(DENSITY_Low_Megafauna)]
low_dens.mlt

low_dens.mlt <- low_dens.mlt[!is.na(Species), ]


# 4. Merge ----------------------------------------------------------------
setdiff(high_dens.mlt$Species, herbs.mlt$Species)
setdiff(low_dens.mlt$Species, herbs.mlt$Species)
# Good. Must be length 0

# Create key:
herbs.mlt[, key := paste(data_point_ID, Species, sep = "__")]
high_dens.mlt[, key := paste(data_point_ID, Species, sep = "__")]
low_dens.mlt[, key := paste(data_point_ID, Species, sep = "__")]

herbivores_long <- merge(herbs.mlt,
                         high_dens.mlt[, .(key, DENSITY_High_Megafauna,
                                           animal_density_units, areal_density_units)],
                         by = "key",
                         all.x = T) |>
  merge(low_dens.mlt[, .(key, DENSITY_Low_Megafauna)],
        by = "key",
        all.x = T)

herbivores_long[!is.na(DENSITY_High_Megafauna) & is.na(DENSITY_Low_Megafauna),
                DENSITY_Low_Megafauna := 0]
herbivores_long


# 5. Clean up species names / Merge in traits -----------------------------
unique(herbivores_long$Species)
unique(traits$Binomial)

setdiff(herbivores_long$Species,
        traits$Binomial)


# >>> Drop subspecies epithets --------------------------------------------

unique(herbivores_long[str_count(Species, pattern = " ") > 1]$Species)
herbivores_long[, Species := word(Species, 1, 2, sep = " ")]

setdiff(herbivores_long$Species,
        traits$Binomial)

# >>> Manual tweaks -------------------------------------------------------
traits[grepl("Bos", Binomial)]$Binomial
herbivores_long[Species == "Bos grunniens", Species := "Bos mutus"]

traits[grepl(" rufus", Binomial)]$Binomial
herbivores_long[Species == "Osphranter rufus", Species := "Macropus rufus"]

herbivores_long[Species == "Equus burchellii", Species := "Equus quagga"]

traits[grepl("Madoqua", Binomial)]$Binomial
traits[grepl("kirk", Binomial)]$Binomial

# Madoqua kirkii is too small for herbitraits and should be dropped:
herbivores_long <- herbivores_long[!Species %in% c("Madoqua kirkii", "Madoqua cavendishi", "Madoqua guentheri")]
herbivores_long

traits[grepl("oryx", Binomial)]$Binomial
herbivores_long[Species == "Taurotragus oryx", Species := "Tragelaphus oryx"]

traits[grepl("Gazella", Binomial)]$Binomial
herbivores_long[Species == "Gazella granti", Species := "Nanger granti"]


traits[grepl("thomsoni", Binomial)]$Binomial
herbivores_long[Species == "Gazella thomsoni", Species := "Eudorcas thomsonii"]


traits[grepl("Tragelaphus", Binomial)]$Binomial
herbivores_long[Species == "Tragelaphus spekei", Species := "Tragelaphus spekii"]


traits[grepl("robustus", Binomial)]$Binomial
herbivores_long[Species == "Osphranter robustus", Species := "Macropus robustus"]


traits[grepl("Ovis", Binomial)]$Binomial
herbivores_long[Species == "Ovis gmelini", Species := "Ovis orientalis"]


setdiff(herbivores_long$Species,
        traits$Binomial)
# All feral

setdiff(herbivores_long$Species,
        traits$Binomial)

traits[grepl("Equus", Binomial)]$Binomial
herbivores_long[Species == "Capra hircus", Species := "Capra aegagrus hircus"]
herbivores_long[Species == "Bos taurus", Species := "Bos primigenius taurus"]
herbivores_long[Species == "Ovis aries", Species := "Ovis orientalis aries"]
herbivores_long[Species == "Equus caballus", Species := "Equus ferus caballus"]
herbivores_long[Species == "Equus asinus", Species := "Equus africanus asinus"]
herbivores_long[Species == "Bubalus bubalis", Species := "Bubalus arnee bubalis"]

setdiff(herbivores_long$Species,
        traits$Binomial)

# >>> Merge ---------------------------------------------------------------
traits[, Mass.kg := Mass.g / 1000]


herbivores_long.mrg <- merge(herbivores_long, 
                             traits[, .(Binomial, Class, Order, Family, Mass.kg)],
                             by.x = "Species",
                             by.y = "Binomial",
                             all.x = T)
nrow(herbivores_long.mrg) == nrow(herbivores_long)
herbivores_long.mrg[is.na(Mass.kg), ]


herbivores_long.mrg

# 6. Convert to hectare & then biomass ----------------------------------------------------
unique(herbivores_long$animal_density_units)

unique(herbivores_long$areal_density_units)
herbivores_long[areal_density_units == "hectare", areal_density_units := "1 hectare"]

herbivores_long[areal_density_units == "km2", `:=` (DENSITY_High_Megafauna = DENSITY_High_Megafauna / 100,
                                                    DENSITY_Low_Megafauna = DENSITY_Low_Megafauna / 100)]
herbivores_long[areal_density_units == "1 hectare",]
herbivores_long[areal_density_units == "1 hectare", areal_density_units := "1 hectare"]

#
unique(herbivores_long$animal_density_units)
# Cant do anything with relative abundance yet. Ultimately everything will be converted to relative abundance

herbivores_long.mrg[animal_density_units == "individuals",
                    `:=` (high_total_kg_ha = DENSITY_High_Megafauna * Mass.kg,
                          low_total_kg_ha = DENSITY_Low_Megafauna * Mass.kg)]
herbivores_long.mrg[animal_density_units == "kg",
                    `:=` (high_total_kg_ha = DENSITY_High_Megafauna,
                          low_total_kg_ha = DENSITY_Low_Megafauna)]


herbivores_long.mrg[, total_loss_kg_ha := high_total_kg_ha - low_total_kg_ha]


# 7. Calculate relative biomass loss-------------------------------------------
unique(herbivores_long.mrg$animal_density_units)
herbivores_long.mrg[, total_community_biomass_loss := sum(total_loss_kg_ha, na.rm = T),
                    by = .(data_point_ID)]

herbivores_long.mrg[total_community_biomass_loss == 0, total_community_biomass_loss := NA]

herbivores_long.mrg[, relative_species_biomass_loss :=  total_loss_kg_ha / total_community_biomass_loss]

herbivores_long.mrg$total_community_biomass_loss <- NULL


unique(herbivores_long.mrg[animal_density_units == "relative abundance"]$DENSITY_Low_Megafauna)
herbivores_long.mrg[animal_density_units == "relative abundance",
                    relative_species_biomass_loss := DENSITY_High_Megafauna]

herbivores_long.mrg

# 8. Tweak species-level nativeness values. 2 studies have mixed communities ----------------------------------------------
# In both cases, the authors emphasized the non-native status of the species, suggesting that
# the native components are minorities ecologically. Unfortunately, they did not report densities.
# But we'll still correct the species level designations

unique(herbivores_long.mrg[Herbivore_nativeness == "Introduced", .(Continent, Site, Species)])
herbivores_long.mrg[Species == "Cervus elaphus" & Continent == "Europe",
                    Herbivore_nativeness := "Native"]

herbivores_long.mrg[Species %in% c("Vombatus ursinus", "Wallabia bicolor") & Continent == "Australia",
                    Herbivore_nativeness := "Native"]

# 9. Bind with earlier dataset --------------------------------------------
herbivores_long.mrg

original

setdiff(names(herbivores_long.mrg), names(original))
herbivores_long.mrg <- herbivores_long.mrg[, !c("key", "Site", "Continent"), with = F]

setdiff(names(original), names(herbivores_long.mrg))
herbivores_long.mrg[, no_species := uniqueN(Species),
                    by = .(data_point_ID)]
setdiff(names(original), names(herbivores_long.mrg))


herbivores_long.final <- rbind(original, herbivores_long.mrg)


# 10. Save ----------------------------------------------------------------

saveRDS(herbivores_long.final, "builds/sidecars/herbivore_traits.Rds")


