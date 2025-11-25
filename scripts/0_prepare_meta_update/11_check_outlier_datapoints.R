

rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          'gsheet', 'metafor'),
                  date = "2025-04-15")

dat <- readRDS("builds/analysis_ready/analysis_ready_dataset.Rds")

dat

# Make edits to the originals:
raw <- readRDS("data/literature_update/preliminary/updated_dataset.Rds")


#' [Screen the following responses:]
#' 1. with SMD: Dead Vegetation, Bare_ground, litter_cover, Vertebrate diversity 
#' 2. with ROM: Carnivore_Abundance, Bird_Abundance


unique(dat[analysis_group == "Vertebrate_Diversity" & Herbivore_nativeness == "Introduced"]$Citation)
dat[analysis_group == "Vertebrate_Diversity" & Herbivore_nativeness == "Introduced" &
      Citation == "Beever and Brussard 2004 Journal of Arid Environments", ]$yi


unique(dat[analysis_group == "Vertebrate_Abundance" & Herbivore_nativeness == "Introduced", .(Title, Citation, 
                                                                                              Herbivores_Manipulated, yi)])

# These are domestic horses, cattle, and fallow deer:
dat <- dat[Citation != "Putman et al. 1989 Biological Conservation"]

# Dead_Vegetation ---------------------------------------------------------

range(dat[analysis_group == "Dead_Vegetation" & eff_type == "smd", ]$yi)

unique(dat[analysis_group == "Dead_Vegetation" & eff_type == "smd" & abs(yi) > 2.5, ]$Title)

# "Vegetation Recovery 16 Years after Feral Pig Removal from a Wet Hawaiian Forest"
pts <- dat[analysis_group %in% "Dead_Vegetation" & eff_type == "smd" & abs(yi) > 2 & 
             Title == "Vegetation Recovery 16 Years after Feral Pig Removal from a Wet Hawaiian Forest", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# This is correct


# "Effects of Feral Horses on Vegetation of Sable Island, Nova Scotia"
pts <- dat[analysis_group == "Dead_Vegetation" & eff_type == "smd" & abs(yi) > 2 & 
             Title == "Effects of Feral Horses on Vegetation of Sable Island, Nova Scotia", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# This is correct

# "White-Tailed Deer (Odocoileus virginianus) Exclusion Shifts Soil Carbon Dynamics in Mature Oak-Dominated and Hemlock-Dominated Forest Stands"
pts <- dat[analysis_group == "Dead_Vegetation" & eff_type == "smd" & abs(yi) > 2 & 
             Title == "White-Tailed Deer (Odocoileus virginianus) Exclusion Shifts Soil Carbon Dynamics in Mature Oak-Dominated and Hemlock-Dominated Forest Stands", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]

# "Experimental test of the impacts of feral hogs on forest dynamics and processes in the southeastern US"
pts <- dat[analysis_group == "Dead_Vegetation" & eff_type == "smd" & abs(yi) > 2 & 
             Title == "Experimental test of the impacts of feral hogs on forest dynamics and processes in the southeastern US", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# looks correct

# "BIOMASS AND NITROGEN RESPONSES TO GRAZING OF UPLAND STEPPE ON YELLOWSTONE NORTHERN WINTER RANGE"
pts <- dat[analysis_group == "Dead_Vegetation" & eff_type == "smd" & abs(yi) > 2 & 
             Title == "BIOMASS AND NITROGEN RESPONSES TO GRAZING OF UPLAND STEPPE ON YELLOWSTONE NORTHERN WINTER RANGE", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Species_Class, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# All good


# "Residual effects of thinning and high white-tailed deer densities on northern redback salamanders in southern New England oak forests"
pts <- dat[analysis_group == "Dead_Vegetation" & eff_type == "smd" & abs(yi) > 2 & 
             Title == "Residual effects of thinning and high white-tailed deer densities on northern redback salamanders in southern New England oak forests", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Species_Class, Species, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# looks correct
# Positive effects on litter/dead veg? ------------------------------------------------------------

unique(dat[analysis_group %in% c("Litter_Cover", "Dead_Vegetation") & eff_type == "smd" & yi > 1, ]$Title)
# 

pts <- dat[analysis_group %in% c("Litter_Cover", "Dead_Vegetation") & eff_type == "smd" & yi > 1 & 
             Title == "BIOMASS AND NITROGEN RESPONSES TO GRAZING OF UPLAND STEPPE ON YELLOWSTONE NORTHERN WINTER RANGE", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Species_Class, Species, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]

pts <- dat[analysis_group %in% c("Litter_Cover", "Dead_Vegetation") & eff_type == "smd" & yi > 1 & 
             Title == "Grazing by over-abundant native herbivores jeopardizes conservation goals in semi-arid reserves", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Species_Class, Species, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# looks correct

pts <- dat[analysis_group %in% c("Litter_Cover", "Dead_Vegetation") & eff_type == "smd" & yi > 1 & 
             Title == "Indirect effects of excessive deer browsing through understory vegetation on stream insect assemblages", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Species_Class, Species, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# correct

pts <- dat[analysis_group %in% c("Litter_Cover", "Dead_Vegetation") & eff_type == "smd" & yi > 1 & 
             Title == "Relationships between soil macroinvertebrates and nonnative feral pigs (Sus scrofa) in Hawaiian tropical montane wet forests", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Species_Class, Species, Response, Mean_Type, Error_bar,
      Mean_High_Megafauna, Mean_Low_Megafauna, SD_High_Megafauna, SD_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# Correct

# Bare_Ground -------------------------------------------------------------
range(dat[analysis_group == "Bare_Ground" & eff_type == "smd", ]$yi)

dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3, ]

dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3, ]$Title

# "Vegetation response to removal of non-native feral pigs from Hawaiian tropical montane wet forest"
pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3 & 
             Title == "Vegetation response to removal of non-native feral pigs from Hawaiian tropical montane wet forest", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]

# "Vegetation Recovery 16 Years after Feral Pig Removal from a Wet Hawaiian Forest"
pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3 & 
             Title == "Vegetation Recovery 16 Years after Feral Pig Removal from a Wet Hawaiian Forest", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


# "Creating multi-functional landscapes: Using exclusion fences to frame feral ungulate management preferences in remote Aboriginal-owned northern Australia"
pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3 & 
             Title == "Creating multi-functional landscapes: Using exclusion fences to frame feral ungulate management preferences in remote Aboriginal-owned northern Australia", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Species, Species_Class, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# "smooth ground' not bare ground. Exclude this.


# "Vegetation change in response to grazing exclusion in montane grasslands, Argentina"
pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3 & 
             Title == "Vegetation change in response to grazing exclusion in montane grasslands, Argentina", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


# "Experimental test of the impacts of feral hogs on forest dynamics and processes in the southeastern US"
pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3 & 
             Title == "Experimental test of the impacts of feral hogs on forest dynamics and processes in the southeastern US", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]

# "Pinus halepensis invasion in mountain pampean grassland: effects of feral horses grazing on seedling establishment"
#' [I have checked this one 1,000 times. Pretty sure they reported SE but called it SD]
pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" & abs(yi) > 3 & 
             Title == "Pinus halepensis invasion in mountain pampean grassland: effects of feral horses grazing on seedling establishment", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# looks fine


# Negative bare_ground ----------------------------------------------------


unique(dat[analysis_group == "Bare_Ground" & eff_type == "smd" & yi < 0, ]$Title)

pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" &  yi < 0 & 
             Title == "Efficacy of exclusion fencing to protect ephemeral floodplain lagoon habitats from feral pigs_Sus scrofa", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate, years_since_treatment)]


pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" &  yi < 0 & 
             Title == "Assessment of impacts of feral horses (Equus caballus) in the Australian Alps", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" &  yi < 0 & 
             Title == "Piecewise disassembly of a large-herbivore community across a rainfall gradient: The UHURU Experiment", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# looks good


pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" &  yi < 0 & 
             Title == "How grazing and soil quality affect native and exotic plant diversity in rocky mountain grasslands", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# looks god


pts <- dat[analysis_group == "Bare_Ground" & eff_type == "smd" &  yi < 0 & 
             Title == "Moose, trees, and ground-living invertebrates: indirect interactions in Swedish pine forests", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]

# looks good.

# Vertebrate_Diversity ----------------------------------------------------

range(dat[analysis_group == "Vertebrate_Diversity" & eff_type == "smd", ]$yi)

unique(dat[analysis_group == "Vertebrate_Diversity" , ]$Title)


# "Effects of thinning and deer browsing on breeding birds in New England oak woodlands"
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Effects of Heavy Browsing on a Bird Community in Deciduous Forest", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Herbivore_nativeness, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# SUPPLEMENTAL FEEDING. EXCLUDE

# Effect of white-tailed deer on songbirds within managed forests in Pennsylvania
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Effect of white-tailed deer on songbirds within managed forests in Pennsylvania", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Herbivore_nativeness, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# Small enclosures



# Impacts of ungulates on the demography and diversity of small mammals in central Kenya
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Impacts of ungulates on the demography and diversity of small mammals in central Kenya", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, treatment_duration_days)]
# Correct


# Impacts of ungulates on the demography and diversity of small mammals in central Kenya
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Impacts of ungulates on the demography and diversity of small mammals in central Kenya", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, treatment_duration_days)]
# Correct




# "Effects of thinning and deer browsing on breeding birds in New England oak woodlands"
pts <- dat[analysis_group == "Vertebrate_Diversity" & eff_type == "smd" & abs(yi) > 2.5 & 
             Title == "Effects of thinning and deer browsing on breeding birds in New England oak woodlands", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# Good

# "The impact of tree modification by African elephant_Loxodonta africana on herpetofaunal species richness in northern Tanzania
pts <- dat[analysis_group == "Vertebrate_Diversity" & eff_type == "smd" & abs(yi) > 2.5 & 
             Title == "The impact of tree modification by African elephant_Loxodonta africana on herpetofaunal species richness in northern Tanzania", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


# "The impact of tree modification by African elephant_Loxodonta africana on herpetofaunal species richness in northern Tanzania Daskin and Pringle"
pts <- dat[analysis_group == "Vertebrate_Diversity" & eff_type == "smd" & abs(yi) > 2.5 & 
             Title == "The impact of tree modification by African elephant_Loxodonta africana on herpetofaunal species richness in northern Tanzania Daskin and Pringle", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]

# "Impacts of large herbivorous mammals on bird diversity and abundance in an African savanna"
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Impacts of large herbivorous mammals on bird diversity and abundance in an African savanna", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]

#
# "Termites, Large Herbivores, and Herbaceous Plant Dominance Structure Small Mammal Communities in Savannahs
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Termites, Large Herbivores, and Herbaceous Plant Dominance Structure Small Mammal Communities in Savannahs", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]




# Avian response to plant community composition and structure in regenerating timber harvests protected by ungulate exclusion fencing
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Avian response to plant community composition and structure in regenerating timber harvests protected by ungulate exclusion fencing", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# fine


# Elk herbivory alters small mammal assemblages in high-elevation drainages
pts <- dat[analysis_group == "Vertebrate_Diversity" & 
             Title == "Elk herbivory alters small mammal assemblages in high-elevation drainages", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, treatment_duration_days)]



# Elk herbivory alters small mammal assemblages in high-elevation drainages
pts <- dat[Title == "The impact of feral horses on grassland bird communities in Argentina", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,Plot_Covariate,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, treatment_duration_days)]


# Carnivore_Abundance & Bird_Abundance / ROM ----------------------------------------------------
unique(dat$analysis_group)
range(dat[analysis_group %in% c("Vert_Carn_Abundance", "Bird_Abundance") & eff_type == "rom", ]$yi)

unique(dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & eff_type == "rom" & abs(yi) > 2.5, ]$Title)

# A mammalian predator-prey imbalance: grizzly bear and wolf extinction affect avian neotropical migrants"
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & 
             eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "A mammalian predator-prey imbalance: grizzly bear and wolf extinction affect avian neotropical migrants", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]

# Beyond the browse line: complex cascade effects mediated by white-tailed deer
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & 
             eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "Beyond the browse line: complex cascade effects mediated by white-tailed deer", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]


# Effects of Heavy Browsing on a Bird Community in Deciduous Forest
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & 
             eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "Effects of Heavy Browsing on a Bird Community in Deciduous Forest", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


# Direct and indirect effects of ungulates on forest birds' nesting failure? An experimental test with artificial nests
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & 
             eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "Direct and indirect effects of ungulates on forest birds' nesting failure? An experimental test with artificial nests", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]


# Experimental evidence that deer browsing reduces habitat suitability for breeding Common Nightingales Luscinia megarhynchos
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & 
             eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "Experimental evidence that deer browsing reduces habitat suitability for breeding Common Nightingales Luscinia megarhynchos", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


# Indirect effects of feral horses on estuarine communities
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "Indirect effects of feral horses on estuarine communities", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
raw[Title == "Indirect effects of feral horses on estuarine communities" &
      Response_Cat == "Richness",
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# looks good, This study had a lot of posiitive abundance impacts but they didnt report them with error. Shame



# Vegetational and Faunal Changes in an Area of Heavily Grazed Woodland Following Relief of Grazing
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "Vegetational and Faunal Changes in an Area of Heavily Grazed Woodland Following Relief of Grazing", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]
# looks good

# Strong positive effects of termites on savanna bird abundance and diversity are amplified by large herbivore exclusion
pts <- dat[analysis_group  %in% c("Vert_Carn_Abundance", "Bird_Abundance") & eff_type == "rom" & abs(yi) > 2.5 & 
             Title == "Strong positive effects of termites on savanna bird abundance and diversity are amplified by large herbivore exclusion", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


# Vertebrate Abundance ----------------------------------------------------

sort(unique(dat[analysis_group == "Vertebrate_Abundance"]$Title))


# A generalist rodent benefits from logging regardless of deer density
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "A generalist rodent benefits from logging regardless of deer density", ]$data_point_ID
dat[analysis_group  %in% c("Vertebrate_Abundance") & 
      Title == "A generalist rodent benefits from logging regardless of deer density", .(yi)]
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]


# Deer-proof fence prevents regeneration of Picea jezoensis var. hondoensis through seed predation by increased woodmouse populations
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Deer-proof fence prevents regeneration of Picea jezoensis var. hondoensis through seed predation by increased woodmouse populations", ]$data_point_ID
dat[analysis_group  %in% c("Vertebrate_Abundance") & 
      Title == "Deer-proof fence prevents regeneration of Picea jezoensis var. hondoensis through seed predation by increased woodmouse populations", .(yi)]
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna)]



# Ecological engineering by a mega-grazer: White Rhino impacts on a South African savanna
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Ecological engineering by a mega-grazer: White Rhino impacts on a South African savanna", ]$data_point_ID
dat[analysis_group  %in% c("Vertebrate_Abundance") & 
      Title == "Ecological engineering by a mega-grazer: White Rhino impacts on a South African savanna", .(yi)]
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]



# Ecological engineering by a mega-grazer: White Rhino impacts on a South African savanna
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Effects of introduction and exclusion of large herbivores on small rodent communities", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]


#
# Herbivore-initiated interaction cascades and their modulation by productivity in an African savanna
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Herbivore-initiated interaction cascades and their modulation by productivity in an African savanna", ]$data_point_ID
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]

#
# Indirect effects of a keystone herbivore elevate local animal diversity
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Indirect effects of a keystone herbivore elevate local animal diversity", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]



# Indirect effects of a keystone herbivore elevate local animal diversity
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Indirect effects of large herbivores on snakes in an African savanna", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]


# Indirect effects of a keystone herbivore elevate local animal diversity
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Influence of grazing by bison and cattle on deer mice in burned tallgrass prairie", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]


# NET EFFECTS OF LARGE MAMMALS ON ACACIA SEEDLING SURVIVAL IN AN AFRICAN SAVANNA
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "NET EFFECTS OF LARGE MAMMALS ON ACACIA SEEDLING SURVIVAL IN AN AFRICAN SAVANNA", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]




# Rodents change acorn dispersal behaviour in response to ungulate presence
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Rodents change acorn dispersal behaviour in response to ungulate presence", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]




# Rodents change acorn dispersal behaviour in response to ungulate presence
pts <- dat[analysis_group  %in% c("Vertebrate_Abundance") & 
             Title == "Testing the effects of deer grazing on two woodland rodents, bankvoles and woodmice", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]



# The cascading effects of elephant presence/absence on arthropods and an Afrotropical thrush in Arabuko-Sokoke Forest, Keny
pts <- dat[Title == "Testing the effects of deer grazing on two woodland rodents, bankvoles and woodmice", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]





# The cascading effects of elephant presence/absence on arthropods and an Afrotropical thrush in Arabuko-Sokoke Forest, Kenya
pts <- dat[Title == "The cascading effects of elephant presence/absence on arthropods and an Afrotropical thrush in Arabuko-Sokoke Forest, Kenya", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]




# The effect of ungulate grazing on a small mammal community in southeastern Botswana
pts <- dat[Title == "The effect of ungulate grazing on a small mammal community in southeastern Botswana", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
#' THIS IS BODY MASS not BIOMASS. FUCK.
# They don't report any usable data...


# The effect of ungulate grazing on a small mammal community in southeastern Botswana
pts <- dat[Title == "Ungulates, rodents, shrubs: interactions in a diverse Mediterranean ecosystem", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]


# The effect of ungulate grazing on a small mammal community in southeastern Botswana
pts <- dat[Title == "Variable effects of wildlife and livestock on questing tick abundance across a topographical–climatic gradient", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]



# The effect of ungulate grazing on a small mammal community in southeastern Botswana
pts <- dat[Title == "Vegetational and Faunal Changes in an Area of Heavily Grazed Woodland Following Relief of Grazing", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# Tiny enclosures.




# The effect of ungulate grazing on a small mammal community in southeastern Botswana
pts <- dat[Title == "Woodland recovery after suppression of deer: Cascade effects for small mammals, wood mice (Apodemus sylvaticus) and bank voles (Myodes glareolus)", ]$data_point_ID
dat[data_point_ID %in% pts]$yi
raw[data_point_ID %in% pts, 
    .(Herbivore_nativeness, experimental_mechanism, Fig_Num, Response, Mean_Type, Error_bar,Species_Class, Species,
      Raw_Mean_High_Megafauna, Raw_Mean_Low_Megafauna, Error_High_Megafauna, Error_Low_Megafauna,
      N_High_Megafauna, N_Low_Megafauna, Plot_Covariate)]
# good



# Bird_Diversity ----------------------------------------------------

sort(unique(dat[analysis_group == "Bird_Diversity" & Herbivore_nativeness == "Introduced", ]$Title))

unique(dat[analysis_group_category == "Vertebrates"]$analysis_group)
sort(unique(dat[analysis_group == "Vert_Carn_Abundance" , ]$Title))


sort(unique(dat[analysis_group == "Vertebrate_Diversity" & Herbivore_nativeness == "Introduced", ]$Title))
dat[Title == "Community- and landscape-level responses of reptiles and small mammals to feral-horse grazing in the Great Basin" &
      analysis_group == "Vertebrate_Diversity"]
