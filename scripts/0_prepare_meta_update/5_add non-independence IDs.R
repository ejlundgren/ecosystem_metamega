

rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          'gsheet', 'metafor',
                          "sf","mapview",
                          "metaDigitise"),
                  date = "2025-04-15")

dat <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")


# 1. Add experiment_id ----------------------------------------------------


dat[, experiment_id := paste0("experiment_", as.character(.GRP)),
    by = .(Citation, Latitude, Longitude, Species_Class, Species, Response_Sphere,
           age_class, Strata_or_soil_depth,
           Species_Nativeness,
           Herbivores_Manipulated,
           DENSITY_High_Megafauna, DENSITY_Low_Megafauna,
           Response_Cat, Response, Plot_Covariate, Site,
           experimental_mechanism)]
dat


# 2. Species response tag -------------------------------------------------

dat[, species_response_tag := paste(Species_Class, Species, Response)]


# 3. Site ID --------------------------------------------------------------
dat[is.na(Latitude), ]
dat[is.na(Longitude), ]

range(dat$Longitude)
# 335??

range(dat$Latitude)

# Create a site-key and subset dataset to unique longs/lats:
dat[, site_key := .GRP, by = .(Longitude, Latitude)]

loc_dat <- unique(dat[, .(site_key, Latitude, Longitude)])

# convert data frame to 'sf'
loc_dat.sf <- st_as_sf(loc_dat, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  mutate(Longitude = loc_dat$Longitude,
         Latitude = loc_dat$Latitude)

#
mapview(loc_dat.sf)

sf_use_s2(TRUE)
loc_dat.sf.buff <- loc_dat.sf %>%
  st_buffer(100000) # 1000 km

loc_dat.sf.buff

loc_dat.sf.buff.summarize <- loc_dat.sf.buff %>%
  summarize()
loc_dat.sf.buff.summarize


loc_dat.sf.buff.summarize <- loc_dat.sf.buff.summarize %>%
  st_cast("POLYGON") #%>%
 # mutate(site_id = seq(1:nrow(.)))
loc_dat.sf.buff.summarize

loc_dat.sf.buff.summarize <- loc_dat.sf.buff.summarize %>%
  mutate(site_id = seq(1:nrow(.)))
loc_dat.sf.buff.summarize

loc_dat.sf.buff.summarize
length(unique(loc_dat.sf.buff.summarize$site_id))
nrow(loc_dat.sf)

# Now extract site_id:
sf_use_s2(FALSE)
loc_dat.sf.jnd <- st_join(loc_dat.sf,
                          loc_dat.sf.buff.summarize,
                          left = TRUE)
nrow(loc_dat.sf.jnd) == nrow(loc_dat.sf)
loc_dat.sf.jnd


loc_dat.sf.jnd.df <- loc_dat.sf.jnd %>%
  as.data.frame() %>%
  mutate(geometry = NULL)

setDT(loc_dat.sf.jnd.df)

loc_dat.sf.jnd.df[, .(n = uniqueN(site_id)), by = .(site_key)][n>1]
# So very few unique lat / longs get lumped together unless we go to 1,000 km...
# Because a lot of lat/longs from the same study sites were copy and pasted (e.g., Africa)
loc_dat.sf.jnd.df

ID_side_car <- merge(dat[, .(site_key, Citation, data_point_ID, experiment_id, species_response_tag)],
                     loc_dat.sf.jnd.df[, .(site_key, site_id)],
                     by = "site_key",
                     all.x = T)

ID_side_car[is.na(site_id)]
unique(ID_side_car$site_id)

ID_side_car[, .(n_citations = uniqueN(Citation)), by = .(site_id)]
ID_side_car$site_key <- NULL
ID_side_car$Citation <- NULL

saveRDS(ID_side_car, "builds/sidecars/non_independence_ids.Rds")

