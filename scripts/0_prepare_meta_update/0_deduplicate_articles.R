
rm(list = ls())
gc()

library("groundhog")
groundhog.day <- "2025-04-15"
libs <- c("tidyverse",
          "stringr", "synthesisr")
groundhog.library(libs, groundhog.day)

dat <- read.csv("data/literature_update/raw/rayyan_merged.csv") #using exported csv file

dim(dat) #4887 21 - note that many fields get collapsed into the "notes" field
names(dat)


dat$title2 <- str_replace_all(dat$title, "[:punct:]", "") %>% str_replace_all(.,"[ ]+", " ") %>% tolower() # Removing all punctuation and extra white spaces 

dat2 <- distinct(dat, title2, .keep_all = TRUE) # select records with unique titles (removes exact duplicates)
dim(dat2) #3864 records

duplicates_string <- find_duplicates(dat2$title2, method = "string_osa", to_lower = TRUE, rm_punctuation = TRUE, threshold = 7)

manual_checks <- review_duplicates(dat2$title, duplicates_string)

# if needed, manually mark some records as unique (not duplicates) by providing their "matches" number, e.g.
# duplicates_string_updated <- synthesisr::override_duplicates(duplicates_string, c(14))
#manual_checks <- review_duplicates(dat2$title, duplicates_string_updated)

dat3 <- extract_unique_references(dat2, duplicates_string)
#dat3 <- extract_unique_references(dat2, duplicates_string_updated) #if manually updated

dim(dat3) #3787 records 23 columns
#str(dat3)
names(dat3)

#keep only useful columns
names(dat3)
dat4 <- dat3 %>% select(title, year, volume, issue, authors, url, abstract, doi, keywords)

dim(dat4) #3787 records
#str(dat4)
names(dat4)

write_refs(dat4, format = "bib", file = "data/literature_update/deduplicated/deduplicated.bib") #save into a bib file
write_refs(dat4, format = "ris", file = "data/literature_update/deduplicated/deduplicated.ris") #save into a ris file
write.csv(dat4, "Rayyan_merged_deduplicated_SRP_main1_MP.csv", row.names = FALSE) #save as a csv file

