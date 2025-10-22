rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          "metaDigitise"),
                  date = "2025-04-15")


files <- list.files("data/literature_update/extraction/")
files <- files[!grepl(".pdf", files)]
files
