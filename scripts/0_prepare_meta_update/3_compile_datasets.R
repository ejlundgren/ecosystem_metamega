
rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          'gsheet', 'metafor'),
                  date = "2025-04-15")

dat <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1z1AqBHsy8_nYz22PIE5-hiKGLpFzZ2pGQ1zjK8pN2P4/edit?gid=1046776742#gid=1046776742')
problems(dat)

# View(problems(dat))
dat[, 36]

setDT(dat)
nrow(dat)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------

# 1. Preliminary cleaning -------------------------------------------------
# Data extracted from figures have been converted to Mean SD. Make sure our columns reflect that:
refs <- dat[grepl("fig", Fig_Num) & Error_bar != "SD", ]$Citation |> unique()
refs

testing <- data.table(path = list.files(file.path("data/literature_update/extraction", refs), full.names = T))
testing[, ref := word(path, 4, sep = "/")]
testing

testing[, figure_extraction := ifelse(any(grepl("caldat", path)), "yes", "no"),
        by = .(ref)]
testing[figure_extraction == "no", ]
# Should be empty.

dat[grepl("fig", Fig_Num, ignore.case = T) & Error_bar != "SD", 
    `:=` (Mean_Type = "Mean", Error_bar = "SD")]

# Drop empty rows and excluded data points:
dat <- dat[is.na(EXCLUSION) & !is.na(Citation)]
nrow(dat)
length(unique(dat$Citation))
# 91 new studies and 1821 effect sizes

dat$data_point_ID <- NULL
dat[, data_point_ID := paste(Citation, seq(1:.N)), by = .(Citation)]
dat
unique(dat$LIMITING_RESOURCE_OR_BIASED_SCOPE)

dat[, Lit_Source := "Web of Science October 2025"]

unique(dat[is.na(time_series_clean), ]$Citation)

dat[is.na(High_value_equals_high_response)]
# Must be 0 rows

unique(dat[is.na(Herbivore_nativeness), ]$Citation)
unique(dat$Herbivore_nativeness)

# Check lat/long:
dat[is.na(as.numeric(Latitude)), ]
dat[is.na(as.numeric(Longitude)), ]$Longitude
dat[, Longitude := gsub("−", "-", Longitude)]
dat[, Longitude := gsub("–", "-", Longitude)]
# Why are there so many dashes...

dat[is.na(as.numeric(Longitude)), ]$Longitude
dat[is.na(as.numeric(Latitude)), ]$Latitude

dat[, `:=` (Latitude = as.numeric(Latitude),
            Longitude = as.numeric(Longitude))]
dat[Longitude > 180, ]
dat[Latitude > 90, ]

# Drop unnecessary columns
names(dat)
dat <- dat[, .(EXCLUSION, data_point_ID, Citation, Title, Lit_Source, Megafauna_Type, Continent,
               island, experimental_mechanism, Latitude, Longitude, Coord_Quality,
               Site, Herbivores_Manipulated, Herbivore_manipulation_notes,
               Herbivore_nativeness, DENSITY_notes, DENSITY_High_Megafauna, DENSITY_Low_Megafauna,
               animal_density_units, areal_density_units,
               years_since_treatment, treatment_duration_days, time_series_clean, data_year,
               impact_mechanism, Fig_Num, 
               Response_Highest_Level, Response_Cat, Response_Sphere, Species_Class,
               Species_Nativeness, Strata_or_soil_depth, age_class, Species_Level,
               Species, Response, Units, High_value_equals_high_response, Mean_Type, Error_bar,
               Raw_Mean_High_Megafauna, Error_High_Megafauna, N_High_Megafauna,
               Raw_Mean_Low_Megafauna, Error_Low_Megafauna, N_Low_Megafauna,
               Plot_Covariate, Digitization_NOTES, initially_entered_by)]
dat[is.na(initially_entered_by), initially_entered_by := "EJL"]

unique(dat$EXCLUSION)

dat[Error_bar == "IQR", ]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------

# 2. Convert Errors to SD ------------------------------------------------
dat[Error_bar == "SD" & Mean_Type != "Mean", Mean_Type := "Mean"]

# >>> First filter out rows where both errors/means are 0 ------------------------
dat[Raw_Mean_High_Megafauna == 0 & Raw_Mean_Low_Megafauna == 0, ]

dat <- dat[!(Raw_Mean_High_Megafauna == 0 & Raw_Mean_Low_Megafauna == 0), ]

unique(dat$Error_bar)

# >>> Format out some text characters -------------------------------------

dat[Error_High_Megafauna == "<0.1", Error_High_Megafauna := "0.05"]
dat[Error_Low_Megafauna == "<0.1", Error_Low_Megafauna := "0.05"]

dat[grepl("<", Raw_Mean_High_Megafauna)]
dat[Raw_Mean_High_Megafauna == "<0.1", Raw_Mean_High_Megafauna := "0.05"]
dat[Raw_Mean_Low_Megafauna == "<0.1", Raw_Mean_Low_Megafauna := "0.05"]
dat[grepl("<", Raw_Mean_Low_Megafauna)]


dat[is.na(as.numeric(Raw_Mean_High_Megafauna)), ]
dat[is.na(as.numeric(Raw_Mean_Low_Megafauna)), ]
dat[is.na(as.numeric(Error_High_Megafauna)), ]$Error_High_Megafauna
dat[is.na(as.numeric(Error_Low_Megafauna)), ]$Error_Low_Megafauna


dat[, `:=` (Mean_High_Megafauna = as.numeric(Raw_Mean_High_Megafauna),
            Mean_Low_Megafauna = as.numeric(Raw_Mean_Low_Megafauna))]

# >>> Convert 95% CIs (which make things character) ------------------------------------------------------------------
convert_95_CI <- function(lower_bound = NULL,
                          upper_bound = NULL,
                          n = NULL){
  margin_of_error = (upper_bound - lower_bound) / 2
  t_value = qt(0.975, df = n - 1)
  standard_error <- margin_of_error / t_value
  SD <- standard_error * sqrt(n)
  return(SD)
}

# First, split off the asymmetrical CIs:
sub_dat <- dat[Error_bar == "95% CIs" & grepl(",", Error_High_Megafauna), ]
dat.filt <- dat[!(Error_bar == "95% CIs" & grepl(",", Error_High_Megafauna)), ]

sub_dat[, .(Error_High_Megafauna, Error_Low_Megafauna)]
sub_dat[, c("High_Megafauna_lowerCI", "High_Megafauna_upperCI") := tstrsplit(Error_High_Megafauna, ",")]
sub_dat[, c("Low_Megafauna_lowerCI", "Low_Megafauna_upperCI") := tstrsplit(Error_Low_Megafauna, ",")]
sub_dat[, `:=` (#Error_High_Megafauna = as.numeric(Error_High_Megafauna),
                #Error_Low_Megafauna = as.numeric(Error_Low_Megafauna),
                High_Megafauna_lowerCI = as.numeric(High_Megafauna_lowerCI),
                High_Megafauna_upperCI = as.numeric(High_Megafauna_upperCI),
                Low_Megafauna_lowerCI = as.numeric(Low_Megafauna_lowerCI),
                Low_Megafauna_upperCI = as.numeric(Low_Megafauna_upperCI))]

#
# Now process the single-value 95% CIs...
sub_dat2 <- dat.filt[Error_bar == "95% CIs"]
sub_dat2

any(sub_dat2$data_point_ID %in% sub_dat$data_point_ID)
any(sub_dat$data_point_ID %in% sub_dat2$data_point_ID)
# Must be FALSE

dat.filt2 <- dat.filt[Error_bar != "95% CIs"]
# Add or subtract from the Mean


sub_dat2[, `:=` (Error_High_Megafauna = as.numeric(Error_High_Megafauna),
                 Error_Low_Megafauna = as.numeric(Error_Low_Megafauna))]

sub_dat2[, `:=` (High_Megafauna_lowerCI = Mean_High_Megafauna - Error_High_Megafauna,
                                              High_Megafauna_upperCI = Mean_High_Megafauna + Error_High_Megafauna,
                                              Low_Megafauna_lowerCI = Mean_Low_Megafauna - Error_Low_Megafauna,
                                              Low_Megafauna_upperCI = Mean_Low_Megafauna + Error_Low_Megafauna)]

sub_dat2

# Now convert:
CIs <- rbind(sub_dat, sub_dat2)

CIs[, SD_High_Megafauna := convert_95_CI(lower_bound = High_Megafauna_lowerCI,
                                             upper_bound = High_Megafauna_upperCI,
                                             n = N_High_Megafauna)]

CIs[, SD_Low_Megafauna := convert_95_CI(lower_bound = Low_Megafauna_lowerCI,
                                             upper_bound = Low_Megafauna_upperCI,
                                             n = N_Low_Megafauna)]
CIs

CIs[, `:=` (High_Megafauna_lowerCI = NULL,
                High_Megafauna_upperCI = NULL,
                Low_Megafauna_lowerCI = NULL,
                Low_Megafauna_upperCI = NULL)]

# Recombine:
dat.recom <- rbind(dat.filt2,
                   CIs,
                   fill = T)

dat.recom


# >>> SE ------------------------------------------------------------------
dat.recom[Error_bar == "SEM", Error_bar := "SE"]

dat.recom[Error_bar == "SE" &
            is.na(as.numeric(Error_High_Megafauna)), ]$Error_High_Megafauna
dat.recom[Error_bar == "SE" &
            is.na(as.numeric(Error_Low_Megafauna)), ]$Error_Low_Megafauna

dat.recom[Error_bar == "SE", `:=` (SD_High_Megafauna = as.numeric(Error_High_Megafauna) * sqrt(N_High_Megafauna),
                                   SD_Low_Megafauna = as.numeric(Error_Low_Megafauna) * sqrt(N_Low_Megafauna))]

dat.recom[Error_bar == "SE" & is.na(SD_High_Megafauna), .(Mean_High_Megafauna, SD_High_Megafauna)]
# TRUE NAs

dat.recom[Error_bar == "SE" & is.na(SD_Low_Megafauna), .(Citation,
                                                         Mean_Low_Megafauna,
                                                         SD_Low_Megafauna) ]
#

# >>> Move Mean/SDs to final columns: ------------------------------------------
dat.recom[Error_bar == "SD", `:=` (SD_High_Megafauna = as.numeric(Error_High_Megafauna),
                             SD_Low_Megafauna = as.numeric(Error_Low_Megafauna))]
dat.recom[is.na(SD_High_Megafauna), .(Mean_High_Megafauna, SD_High_Megafauna)]
# TRUE NAs

dat.recom[is.na(SD_Low_Megafauna), .(Mean_Low_Megafauna, SD_Low_Megafauna)]
# TRUE NAs

dat.recom[SD_High_Megafauna < 0, ]
dat.recom[SD_Low_Megafauna < 0, ]


# NA SDs will be dropped, following testing during last dataset. Adding constants 
# Can lead to giant effect sizes. Dropping the NA values, loses:
nrow(dat.recom[is.na(SD_High_Megafauna) | is.na(SD_Low_Megafauna)])
# 46 effect sizes, which is minimal.


dat.recom[data_point_ID == "Murray et al. 2010 Ecology_38"]


dat.recom[Mean_High_Megafauna == 0 & Mean_Low_Megafauna == 0]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------

# 4. Rbind with original dataset ----------------------------------------------
trepel <- readRDS("data/Trepel_2024/Trepel_2024.Rds")

trepel[is.na(Latitude), ]
trepel[is.na(Longitude), ]


# >>> Bind ----------------------------------------------------------------

setdiff(names(trepel), names(dat.recom))

dat.recom[, `:=` (Response_Living = "",
                      Response_Dimension = "",
                      woody_height_class = "")]
dat.recom

setdiff(names(dat.recom), names(trepel))

responses.bind <- rbind(trepel, dat.recom, fill = TRUE)

responses.bind

responses.bind[, .(n_refs = uniqueN(Citation), n_obs = .N)]


# >>> Flag studies that are problematic and need to be excluded- or tweaked----------------------------
"Creating multi-functional landscapes: Using exclusion fences to frame feral ungulate management preferences in remote Aboriginal-owned northern Australia"
# This reports 'smooth ground' not bare ground or litter cover. Meaningless
responses.bind[Title == "Creating multi-functional landscapes: Using exclusion fences to frame feral ungulate management preferences in remote Aboriginal-owned northern Australia"
               & Response_Cat == "Bare_Ground" & Response_Sphere == "soil",]

responses.bind[Title == "Creating multi-functional landscapes: Using exclusion fences to frame feral ungulate management preferences in remote Aboriginal-owned northern Australia"
               & Response_Cat == "Bare_Ground" & Response_Sphere == "soil",
               EXCLUSION := "Meaningless response; exclude"]

# wrong sample size. OK to change these here because Error is SD not SE:
responses.bind[Title == "Beyond the browse line: complex cascade effects mediated by white-tailed deer" &
                 Fig_Num == "calculated from Table 3",]

responses.bind[Title == "Beyond the browse line: complex cascade effects mediated by white-tailed deer" &
                 Fig_Num == "calculated from Table 3",
                `:=` (N_High_Megafauna = 3, N_Low_Megafauna = 3)]

# Same: OK to change these here because Error is SD not SE:
responses.bind[Title == "Effects of Heavy Browsing on a Bird Community in Deciduous Forest" &
                 Fig_Num == "Table 3",]

responses.bind[Title == "Effects of Heavy Browsing on a Bird Community in Deciduous Forest" &
                 Fig_Num == "Table 3",
               `:=` (N_High_Megafauna = 3, N_Low_Megafauna = 3)]

unique(responses.bind$EXCLUSION)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------

# 7. Save dataset for further processes -----------------------------------

saveRDS(responses.bind, "data/literature_update/preliminary/updated_dataset.Rds")


