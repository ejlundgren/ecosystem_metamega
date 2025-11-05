


rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          'gsheet', 'metafor'),
                  date = "2025-04-15")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ -------------------------------------

# 1. Load data ------------------------------------------------------------

dat <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")


# 2. Calculate CV ---------------------------------------

dat[, CV_High_Megafauna := SD_High_Megafauna / Mean_High_Megafauna]
dat[, CV_Low_Megafauna := SD_Low_Megafauna / Mean_Low_Megafauna]

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------
# 3. Adjust extreme values -----------------------------------------------

# >>> Means of 0 in both groups
nrow(dat[(Mean_High_Megafauna == 0 & Mean_Low_Megafauna == 0),])

# For some reason, Adding '!' in front of that conditional excludes HUNDREDS OF STUDIES.
nrow(dat)
nrow(dat[!(Mean_High_Megafauna == 0 & Mean_Low_Megafauna == 0),])
nrow(dat[(Mean_High_Megafauna != 0 & Mean_Low_Megafauna != 0),])

# What the hell right?
pts <- dat[(Mean_High_Megafauna == 0 & Mean_Low_Megafauna == 0),]$data_point_ID
length(pts)

dat <- dat[!data_point_ID %in% pts]
nrow(dat)

# >>> Add a small constant to Means & SDs of 0 ... ----------------------------------
dat[Mean_High_Megafauna == 0, Mean_High_Megafauna := Mean_Low_Megafauna * 0.01]

dat[Mean_Low_Megafauna == 0,]

dat[Mean_Low_Megafauna == 0, Mean_Low_Megafauna := Mean_High_Megafauna * 0.01]


dat[SD_High_Megafauna == 0 | is.na(SD_High_Megafauna)]$SD_High_Megafauna

# Add a small constant to SD's of 0/NA
pts <- dat[SD_High_Megafauna == 0 | is.na(SD_High_Megafauna)]$data_point_ID
pts

dat[SD_High_Megafauna == 0 | is.na(SD_High_Megafauna), SD_High_Megafauna := SD_Low_Megafauna / 2]
dat[data_point_ID %in% pts, .(data_point_ID, Mean_High_Megafauna, Mean_Low_Megafauna, SD_High_Megafauna, SD_Low_Megafauna)]

#
dat[SD_Low_Megafauna == 0 | is.na(SD_Low_Megafauna)]$SD_Low_Megafauna

dat[SD_Low_Megafauna == 0 | is.na(SD_Low_Megafauna), SD_Low_Megafauna := SD_High_Megafauna / 2]
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------

# 4. Fix some values ------------------------------------------------------
dat[Title == "Pinus halepensis invasion in mountain pampean grassland: effects of feral horses grazing on seedling establishment"]

dat[Title == "Pinus halepensis invasion in mountain pampean grassland: effects of feral horses grazing on seedling establishment" &
      Response_Cat == "Bare_Ground"]

# Mistakenly recorded N as 20 for each group But it looks like it was 20 total.
# And the N of 5 is from an experimental manipulation (clipping), not herbivory.
nrow(dat[!(Title == "Pinus halepensis invasion in mountain pampean grassland: effects of feral horses grazing on seedling establishment" &
        N_High_Megafauna == 5)])
nrow(dat)
dat <- dat[!(Title == "Pinus halepensis invasion in mountain pampean grassland: effects of feral horses grazing on seedling establishment" &
        N_High_Megafauna == 5)]

dat[N_High_Megafauna == 20 & N_Low_Megafauna == 20 &
      Title == "Pinus halepensis invasion in mountain pampean grassland: effects of feral horses grazing on seedling establishment",
    `:=` (N_High_Megafauna = 10, N_Low_Megafauna = 10)]


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------

# 5. Calculate SMD and lnROM ----------------------------------------------------------

nrow(dat)

dat.out <- escalc(measure = "SMDH",
                             m1i = Mean_High_Megafauna,
                             m2i = Mean_Low_Megafauna,
                             sd1i = SD_High_Megafauna,
                             sd2i = SD_Low_Megafauna,
                             n1i = N_High_Megafauna,
                             n2i = N_Low_Megafauna,
                             data = dat)
setDT(dat.out)
dat.out

setnames(dat.out, 
         c("yi", "vi"),
         c("yi_smd", "vi_smd"))

dat.out[is.na(yi_smd)]
dat.out[is.na(vi_smd)]

# dat.out <- dat.out[!is.na(yi_smd), ]
# dat.out

range(dat.out$yi_smd, na.rm = T)

dat.out[, filter := ifelse(abs(yi_smd) > 10, "exclude_smd10", "keep")]
dat.out

# Now ROM:
dat.out2 <- escalc(measure = "ROM",
                  m1i = Mean_High_Megafauna,
                  m2i = Mean_Low_Megafauna,
                  sd1i = SD_High_Megafauna,
                  sd2i = SD_Low_Megafauna,
                  n1i = N_High_Megafauna,
                  n2i = N_Low_Megafauna,
                  data = dat.out)
setDT(dat.out2)
dat.out2

setnames(dat.out2, 
         c("yi", "vi"),
         c("yi_rom", "vi_rom"))

nrow(dat.out2[is.na(yi_rom)])
dat.out2[is.na(yi_smd)]

dat.out2 <- dat.out2[!is.na(yi_rom) & !is.na(yi_smd)]
nrow(dat.out2)
nrow(dat.out)

range(dat.out2$yi_rom, na.rm = T)
# More reasonable range of numbers

dat.out2[yi_rom < -10]

# >>> Flip signs where needed: --------------------------------------------
#
dat.out2[is.na(High_value_equals_high_response),]
dat.out2[is.na(High_value_equals_high_response), High_value_equals_high_response := TRUE]
dat.out2[Response == "Distance to compacted layer", High_value_equals_high_response := FALSE]

#
dat.out2[High_value_equals_high_response == FALSE, yi_smd := -yi_smd]
dat.out2[High_value_equals_high_response == FALSE, yi_rom := -yi_rom]

dat.out2

# 6. Save effect side car -----------------------------------------------------------------

effects <- dat.out2[, .(data_point_ID, CV_High_Megafauna, CV_Low_Megafauna, filter, yi_smd, vi_smd, yi_rom, vi_rom)]

effects[CV_High_Megafauna >= 10 & CV_Low_Megafauna >= 10, ]

saveRDS(effects, "builds/sidecars/effect_sizes.Rds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ----------------------------------------
# OLD ---------------------------------------------------------------------

# 
# dat[CV_High_Megafauna < 10 & CV_Low_Megafauna < 10, ]
# dat[, CV_Filter := ifelse(CV_High_Megafauna < 10 & CV_Low_Megafauna < 10,
#                           "keep", "exclude")]
# 
# dat[CV_High_Megafauna == Inf | CV_High_Megafauna == -Inf, ]
# dat[CV_Low_Megafauna == Inf | CV_Low_Megafauna == -Inf, ]
# 
# 
# dat[is.na(CV_High_Megafauna)]
# dat[is.na(CV_Low_Megafauna)]
# 
# # This retains 9777 rows
# nrow(dat) # out of 10,639
# 
# # Using the Geary-Lajeunesse rule....
# # Lajeunesse 2015 Ecology
# # This is for lnRR, so we won't use. But out of curiosity:
# dat[, Geary_Test_High_Megafauna := Mean_High_Megafauna / SD_High_Megafauna * ( (4*N_High_Megafauna^(3/2)) / (1+(4*N_High_Megafauna)) )]
# dat[, Geary_Test_Low_Megafauna := Mean_Low_Megafauna / SD_Low_Megafauna * ( (4*N_Low_Megafauna^(3/2)) / (1+(4*N_Low_Megafauna)) )]
# 
# # dat.out[is.na(Geary_Test_High_Megafauna)]
# 
# # So retaining Geary >= 3
# nrow(dat[Geary_Test_High_Megafauna >= 3 & Geary_Test_Low_Megafauna >= 3, ])
# dat[, Geary_Filter := ifelse(Geary_Test_High_Megafauna >= 3 & Geary_Test_Low_Megafauna >= 3,
#                              "keep", "exclude")]
# # Retains 6538 rows
# nrow(dat)# out of 10,631
# 
# # That's a huge loss of data...
# range(dat$CV_High_Megafauna)
# 
# ggplot(data = dat, aes(x = CV_High_Megafauna, y = CV_Low_Megafauna, color = CV_Filter))+
#   geom_jitter(size = 3, alpha = .5)
# 
# 
# ggplot(data = dat, aes(x = CV_High_Megafauna, y = CV_Low_Megafauna, color = Geary_Filter))+
#   geom_jitter(size = 3, alpha = .5)
# 
# # 4. Calculate log-transformed SMD for skewed studies ---------------------
# 
# log_moments <- function(sd, x, n){
#   
#   cv2 <- (sd / x)^2
#   li <- log(1 + cv2)
#   log_variance <- li - ((li^2) / (2*(n-1))) + ((li * (li+2)) / (n * (x/sd)^2))
#   log_sd <- sqrt(log_variance)
#   log_mean <- log(x) - ((1/2) * log_variance) + (log_variance/(2*n))
#   
#   log_arithmetic_mean <- log(x) + (log_variance) / (2*n)
#   
#   return(data.table("log_variance" = log_variance,
#                     "log_sd" = log_sd,
#                     "log_mean" = log_mean,
#                     "log_arithmetic_mean" = log_arithmetic_mean))
# }
# 
# 
# log_high <- log_moments(sd = dat.out$SD_High_Megafauna,
#                         x = dat.out$Mean_High_Megafauna,
#                         n = dat.out$N_High_Megafauna)
# log_high
# names(log_high)
# dat.out[, Log_SD_High_Megafauna := log_high$log_sd]
# dat.out[, Log_Mean_High_Megafauna := log_high$log_mean]
# dat.out
# 
# 
# log_low <- log_moments(sd = dat.out$SD_Low_Megafauna,
#                        x = dat.out$Mean_Low_Megafauna,
#                        n = dat.out$N_Low_Megafauna)
# log_low
# 
# dat.out[, Log_SD_Low_Megafauna := log_low$log_sd]
# dat.out[, Log_Mean_Low_Megafauna := log_low$log_mean]
# dat.out
# 
# 
# # >>> SMD -----------------------------------------------------------------
# 
# dat.out2 <- escalc(measure = "SMD",
#                    m1i = Log_Mean_High_Megafauna,
#                    m2i = Log_Mean_Low_Megafauna,
#                    sd1i = Log_SD_High_Megafauna,
#                    sd2i = Log_SD_Low_Megafauna,
#                    n1i = N_High_Megafauna,
#                    n2i = N_Low_Megafauna,
#                    data = dat.out)
# setDT(dat.out2)
# dat.out2
# 
# setnames(dat.out2, 
#          c("yi", "vi"),
#          c("yi_smd_log", "vi_smd_log"))
# 
# dat.out2
# # If CV <= 10, just use regular SMD:
# # 
# # dat.out2[CV_High_Megafauna <= 10 & CV_Low_Megafauna <= 10, 
# #          `:=` (yi_smd_log = yi_smd,
# #                vi_smd_log = vi_smd)]
# 
# # Whoa. Some of these values became GIGANTIC.
# range(dat.out2$yi_smd_log, na.rm = T)
# range(dat.out2$vi_smd_log, na.rm = T)
# 
# View(dat.out2[yi_smd_log > 100, .(Mean_High_Megafauna, SD_High_Megafauna, N_High_Megafauna,
#                                   Log_Mean_High_Megafauna, Log_SD_High_Megafauna,
#                                   Mean_Low_Megafauna, SD_Low_Megafauna, N_Low_Megafauna,
#                                   Log_Mean_Low_Megafauna, Log_SD_Low_Megafauna, yi_smd, vi_smd, yi_smd_log)])
# 
# # Potentially filter SMD to abs(10)
# 
