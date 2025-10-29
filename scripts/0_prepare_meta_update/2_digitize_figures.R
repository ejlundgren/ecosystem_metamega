rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          "metaDigitise"),
                  date = "2025-04-15")


files <- data.table(file = list.files("data/literature_update/extraction", recursive = T,
                                      full.names = T))

# files <- files[!grepl(".pdf", files)]
# files <- files[!grepl("pdf", file)]
# files
files[, length := str_count(file, "/")]
files <- files[!grepl(".pdf", file)]

files
files[, folder := word(file, 1, 4, sep = "/")]
files[, folder := word(folder, -1, sep = "/")]
files

files[, finished := ifelse(any(grepl("caldat", .SD$file)), "yes", "no"),
      by = folder]
files


files[, has_figures := ifelse(grepl("jpg", file) | grepl("png", file) | grepl("jpeg", file),
                              TRUE, FALSE)]
files


files[finished == "yes", ]

to_do <- files[finished == "no" & has_figures == TRUE]$folder
to_do

to_do <- unique(word(to_do, -1, sep = "/"))
to_do

# Allen et al 2023 was done in WebPlotDigitiser:
to_do <- to_do[to_do != "Allen et al 2023 Ecological Applications"]
to_do <- sort(to_do)
to_do

# Aggerbeck et al 2022 Science of the Total Environment
# path <- file.path("data/literature_update/extraction/",
#                   "Aggerbeck et al 2022 Science of the Total Environment")
# list.files(path)
# data <- metaDigitise(dir = path)
# data
# 
# # Aggerbeck et al 2022 Science of the Total Environment
# path <- file.path("data/literature_update/extraction/",
#                   "Aggerbeck et al 2022 Science of the Total Environment")
# list.files(path)
# data <- metaDigitise(dir = path)
# data
# 
# # Allen et al 2023 Ecological Applications
# files[2]
# path <- file.path("data/literature_update/extraction/",
#                   "Allen et al 2023 Ecological Applications")
# list.files(path)
# data <- metaDigitise(dir = path)
# data



# XXXXXXXXX
# files[3]
# path <- file.path("data/literature_update/extraction/",
#                   "Andreoni et al 2024 Ecology")
# list.files(path)
# data <- metaDigitise(dir = path)
# data

folders <- sort(folders)

# Avila et al 2025 Applied Vegetation Science
folders[3]
path <- file.path("data/literature_update/extraction/",
                  "Avila et al 2025 Applied Vegetation Science")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Baidya et al 2025 Ecological Applications
folders[4]
path <- file.path("data/literature_update/extraction/",
                  "Baidya et al 2025 Ecological Applications")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))

# Baraza et al 2025 Plant Soil
folders[5]
path <- file.path("data/literature_update/extraction/",
                  "Baraza et al 2025 Plant Soil")
list.files(path)
data <- metaDigitise(dir = path)


# Barrios Garcia et al 2022 Biological Invasions
folders[6]
path <- file.path("data/literature_update/extraction/",
                  "Barrios Garcia et al 2022 Biological Invasions")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Beckett et al. 2022 Global Ecology and Conservation
folders[7]
path <- file.path("data/literature_update/extraction/",
                  "Beckett et al. 2022 Global Ecology and Conservation")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Bon et al Journal of Ecology
folders[8]
path <- file.path("data/literature_update/extraction/",
                  "Bon et al Journal of Ecology")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
folders[9]
path <- file.path("data/literature_update/extraction/",
                  "Bovio et al 2024 forests")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Brearley et al 2024 Forest Ecology and Management
folders[10]
path <- file.path("data/literature_update/extraction/",
                  "Brearley et al 2024 Forest Ecology and Management")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Antoniella et al 2025 Urban Forestry and Urban Greening
# folders[2]
path <- file.path("data/literature_update/extraction/",
                  "Antoniella et al 2025 Urban Forestry and Urban Greening")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Brown et al 2024 Biological Invasions
folders[11]
path <- file.path("data/literature_update/extraction/",
                  "Brown et al 2024 Biological Invasions")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
folders[12]
path <- file.path("data/literature_update/extraction/",
                  "Cera et al 2024 Restoration Ecology")
list.files(path)
data <- metaDigitise(dir = path)
data


# Chard et al 2022 Ecology and Evolution
folders[13]
path <- file.path("data/literature_update/extraction/",
                  "Chard et al 2022 Ecology and Evolution")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Coetsee et al 2022 Ecosystems
folders[14]
path <- file.path("data/literature_update/extraction/",
                  "Coetsee et al 2022 Ecosystems")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Coetsee et al 2023 Austral Ecology
folders[15]
path <- file.path("data/literature_update/extraction/",
                  "Coetsee et al 2023 Austral Ecology")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
folders[16]
path <- file.path("data/literature_update/extraction/",
                  "Coverdale et al 2024 Ecological Monographs")
list.files(path)
data <- metaDigitise(dir = path)
data <- data[data$group_id != "blah", ]
data$group_id <- paste("trt:", data$group_id)
fwrite(data, file.path(path, "out.csv"))


# Crameri et al 2025 JGR Biogeosciences
folders[17]
path <- file.path("data/literature_update/extraction/",
                  "Crameri et al 2025 JGR Biogeosciences")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Don et al 2024 sustainability
folders[18]
path <- file.path("data/literature_update/extraction/",
                  "Don et al 2024 sustainability")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Donoso et al 2024 Invasive Plant Science and Management
folders[19]
path <- file.path("data/literature_update/extraction/",
                  "Donoso et al 2024 Invasive Plant Science and Management")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Dornbusch et al 2022 Ecological Applications
folders[20]
path <- file.path("data/literature_update/extraction/",
                  "Dornbusch et al 2022 Ecological Applications")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
folders[21]
path <- file.path("data/literature_update/extraction/",
                  "Enochs et al 2022 American Midland Naturalist")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Forbes et al Ecosystems
folders[22]
path <- file.path("data/literature_update/extraction/",
                  "Forbes et al Ecosystems")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Guy et al 2021 Current Biology
folders[23]
path <- file.path("data/literature_update/extraction/",
                  folders[23])
list.files(path)haha
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Hancock et al 2025 European Journal of Forest Research
folders[24]
path <- file.path("data/literature_update/extraction/",
                  "Hancock et al 2025 European Journal of Forest Research")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Hupperts et al 2022 forests
folders[25]
path <- file.path("data/literature_update/extraction/",
                  "Hupperts et al 2022 forests")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Kadowaki et al 2023 Environmental DNA
folders[26]
path <- file.path("data/literature_update/extraction/",
                  "Kadowaki et al 2023 Environmental DNA")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Knauer et al 2023 Canadian Journal of Forest Research
folders[27]
path <- file.path("data/literature_update/extraction/",
                  "Knauer et al 2023 Canadian Journal of Forest Research")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Konopka and Seben 2024 Central European Forestrey Journal
folders[28]
path <- file.path("data/literature_update/extraction/",
                  "Konopka and Seben 2024 Central European Forestrey Journal")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Konopka et al 2021 Forests
folders[29]
path <- file.path("data/literature_update/extraction/",
                  "Konopka et al 2021 Forests")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))

# RESET TO-DO LIST ------------------------------------------------------------------
# XXXXXXXXX
to_do[1]
path <- file.path("data/literature_update/extraction/",
                  "Lecomte et al 2024 Ecological Applications")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Linden et al. 2021 Ecology and Evolution
to_do[2]
path <- file.path("data/literature_update/extraction/",
                  "Linden et al. 2021 Ecology and Evolution")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Lugar and Pennings 2025 Estuaries and Coasts
to_do[3]
path <- file.path("data/literature_update/extraction/",
                  "Lugar and Pennings 2025 Estuaries and Coasts")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Maillard et al 2021 Forest Ecology and Management
to_do[4]
path <- file.path("data/literature_update/extraction/",
                  "Maillard et al 2021 Forest Ecology and Management")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# McCleery et al 2025 Biological Conservation
to_do[5]
path <- file.path("data/literature_update/extraction/",
                  "McCleery et al 2025 Biological Conservation")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
to_do[6]
path <- file.path("data/literature_update/extraction/",
                  "Mndela et al Plant Ecology & Diversity")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Moon et al 2025 Ecosystems
to_do[7]
path <- file.path("data/literature_update/extraction/",
                  "Moon et al 2025 Ecosystems")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
to_do[8]
path <- file.path("data/literature_update/extraction/",
                  "Morimoto et al Frontiers in Forests and Global Change")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Morrison and Woldemariam 2022 Ecology and Evolution
to_do[9]
path <- file.path("data/literature_update/extraction/",
                  "Morrison and Woldemariam 2022 Ecology and Evolution")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
to_do[10]
path <- file.path("data/literature_update/extraction/",
                  "Perles et al 2021 Forest Ecology and Management")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# Rocca et al 2024 Journal of Vegetation Science
to_do[11]
path <- file.path("data/literature_update/extraction/",
                  "Rocca et al 2024 Journal of Vegetation Science")
list.files(path)
data <- metaDigitise(dir = path)
data
fwrite(data, file.path(path, "out.csv"))


# XXXXXXXXX
to_do[12]
path <- file.path("data/literature_update/extraction/",
                  "Salisbury et a 2023 JGR Biosciences")
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Soper et al 2024 Biogeochemistry
x <- 13
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Suzuki et al 2021 Applied Vegetation Science
x <- 14
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Swain et al 2023 Journal of Animal Ecology
x <- 15
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Takafumi et al 2023 Forest Ecology and Management"
x <- 16
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Tron et al 2024 Biological Conservation
x <- 17
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Villar and Medici 2021 Journal of Applied Ecology
x <- 18
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# RESET to-do -------------------------------------------------------------
# Wagner et al 2023 Natural Areas Journal
x <- 1
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Watson et al 2021 Fungal Ecology
x <- 2
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# RESET to-do -------------------------------------------------------------
# Faison et al 2025 Forest Ecology and Management
x <- 1
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Wells et al 2024 Ecosphere
x <- 2
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Yacucci et al 2024 New Forests
x <- 3
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Young et al 2021 Ecosphere
x <- 4
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Reset to-do -------------------------------------------------------------


# Braden et al 2021 Journal of Arid Environments
x <- 1
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Cepeda et al 2024 Journal of Experimental Botany
x <- 2
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Copeland et al 2025 Ecosphere
x <- 3
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))

# Kauffman et al 2025 Ecosphere
x <- 4
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Mizutani et al 2022 Journal of Forest Research
x <- 5
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# O'Connor et al 2024 African Journal of Ecology
x <- 6
to_do[x]
path <- file.path("data/literature_update/extraction/", 
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Otsu et al 2025 Plant Ecology
x <- 7
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Reed et al 2023 Ecology
x <- 8
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# Roy et al 2023 Global Change Biology
x <- 9
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# # Spicer et al 2023 Forest Ecology and Management
# x <- 10
# to_do[x]
# path <- file.path("data/literature_update/extraction/",
#                   to_do[x])
# list.files(path)
# data <- metaDigitise(dir = path, cex = .5)
# data
# fwrite(data, file.path(path, "out.csv"))
# WRONG STUDY DESIGN

# Tsuboike et al 2021 Frontiers in Ecology and Evolution
x <- 11
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))


# "Treby and Carnell 2023 Ecohydrology & Hydrobiology"
x <- 1
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))



# LaMalfa et al 2021 Ecological Applications
x <- 1
to_do[x]
path <- file.path("data/literature_update/extraction/",
                  to_do[x])
list.files(path)
data <- metaDigitise(dir = path, cex = .5)
data
fwrite(data, file.path(path, "out.csv"))
