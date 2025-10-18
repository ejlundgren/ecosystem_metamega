
rm(list = ls())

# Use metaDigitize to extract data
library("groundhog")
groundhog.library(pkg = c("data.table", "tidyverse",
                          "readxl", "stringr",
                          "metaDigitise"),
                  date = "2025-04-15")

# Ohira et al. APPENDIX -----------------------------------------------------------------------
dat <- read_excel("data/literature_update/extraction/Ohira et al 2022 Ecological Applications/Data_PlotVariable.xlsx")
dat

setDT(dat)
dat

names(dat)
# setnames(dat,)
dat <- dat[Sampling_year == 2018, ]

dat.mlt <- melt(dat, id.vars = c("Sampling_year", "Treatment", "Position_on_hillslope"))

dat.mlt
dat.mlt <- dat.mlt[variable != "Cover_class", ]
dat.mlt[is.na(as.numeric(value)), ]

dat.mlt[, value := as.numeric(value)]

x <- dat.mlt[, .(mean = mean(value), sd = sd(value),
                 n = .N), 
             by = .(Treatment, Position_on_hillslope, variable)]
x[, Position_on_hillslope := paste(Position_on_hillslope, "hillslope")]

x.cst <- dcast(x,
               Position_on_hillslope + variable ~ Treatment,
               value.var = c("mean", "sd", "n"))
x.cst


fwrite(x.cst, "data/literature_update/extraction/Ohira et al 2022 Ecological Applications/Ohira_summarized.csv")

# Woodward -----------------------------------------------------------------------
#' [No error]
# key <- read_excel("data/literature_update/extraction/Woodward et al 2021 Ecosphere/Elk_Exclosures_OlympicNP.xlsx",
#                   skip = 19)
# key
# setDT(key)
# setnames(key, "Life form (F = fern, G = graminoid, H = herb, L = lichen, M = moss, S = shrub, T = tree)", "lifeform")
# key[, lifeform := fcase(lifeform == "S", "shrub",
#                         lifeform == "F", "fern",
#                         lifeform == "G", "graminoid",
#                         lifeform == "H", "herb",
#                         lifeform == "L", "lichen",
#                         lifeform == "M", "moss",
#                         lifeform == "T", "tree")]
# key
# 
# 
# dat <- read_excel("data/literature_update/extraction/Woodward et al 2021 Ecosphere/Elk_Exclosures_OlympicNP.xlsx",
#                   skip = 3,
#                   sheet = "Herbs")
# dat
# setDT(dat)
# 
# dat.mlt <- melt(dat,
#                 id.vars = c("SUBPLOT", "YEAR"))
# dat.mlt
# 
# dat.mlt[, tab := "Herbs"]
# dat.mlt[, measure := "% Cover"]
# 
# dat.mlt
# 
# # Now shrubs:
# dat2 <- read_excel("data/literature_update/extraction/Woodward et al 2021 Ecosphere/Elk_Exclosures_OlympicNP.xlsx",
#                   skip = 3,
#                   sheet = "Shrubs")
# dat2
# setDT(dat2)
# dat.mlt2 <- melt(dat2,
#                 id.vars = c("SUBPLOT", "YEAR"))
# dat.mlt2
# 
# dat.mlt2[, tab := "Shrubs"]
# dat.mlt2[, measure := "% Cover"]
# dat.mlt2
# 
# # Now trees...
# dat3 <- read_excel("data/literature_update/extraction/Woodward et al 2021 Ecosphere/Elk_Exclosures_OlympicNP.xlsx",
#                    skip = 6,
#                    sheet = "Tree Seedlings")
# dat3
# setDT(dat3)
# 
# dat3 <- dat3[, .(SUBPLOT, YEAR, SPECIES, Total)]
# dat3
# setnames(dat3, "SPECIES", "variable")
# setnames(dat3, "Total", "value")
# dat3[, tab := "Trees >10 cm"]
# dat3[, measure := "Density"]
# 
# dat3
# dat.mlt2
# 
# # Combined:
# woodward <- rbind(dat.mlt, dat.mlt2, dat3)
# 
# woodward
# 
# woodward.mrg <- merge(woodward,
#                       key,
#                       by.x = "variable",
#                       by.y = "Abbreviation")
# woodward.mrg
# 
# setnames(woodward.mrg, "YEAR", "data_year")
# woodward.mrg[, years_since_treatment := data_year - 1979]
# 
# woodward.mrg[, treatment_duration_days := years_since_treatment * 365]
# 
# woodward.mrg
# 
# 
# woodward.mrg[, treatment := ifelse(grepl("IN", SUBPLOT), "Low_Megafauna", "High_Megafauna")]
# woodward.mrg
# 
# woodward.mrg.sum <- woodward.mrg[, .(mean = mean(value), sd = sd(value)),
#                                  by = .(data_year, years_since_treatment, treatment_duration_days,
#                                         Species, lifeform, measure, treatment)]
# 
# woodward.mrg.sum
# 
# 
# unique(woodward.mrg[, .(treat)])
# totals <- woodward.mrg[, .(mean = mean(value), sd = sd(value),
#                            n = uniqueN(SUBPLOT)),
#                        by = .(data_year, years_since_treatment, treatment_duration_days, lifeform, measure, treatment)]
# totals[, Species := lifeform]
# totals
# 
# woodward.abundance <- rbind(totals, woodward.mrg.sum)
# woodward.abundance
# 
# woodward.abundance.cst <- dcast(woodward.abundance,
#                                 ... ~ treatment,
#                                 value.var = c("mean", "sd"))
# woodward.abundance.cst
# 
# fwrite(woodward.abundance.cst, "data/literature_update/extraction/Woodward et al 2021 Ecosphere/abundance.csv")
# 
# 
# # Now diversity:
# woodward.mrg
# woodward.mrg[value == 0, ]
# woodward.mrg[, key := paste(SUBPLOT, data_year)]
# 
# woodward.mrg.filt <- woodward.mrg[value > 0, ]
# 
# setdiff(woodward.mrg$key, woodward.mrg.filt$key)
# # must be length 0
# 
# richness <- woodward.mrg.filt[, .(rich = uniqueN(Species)),
#                               by = .(SUBPLOT, data_year, lifeform,
#                                      years_since_treatment, treatment_duration_days,
#                                      treatment)]
# richness
# richness <- richness[, .(mean = )]


# Collings et al 2023 Ecosphere -------------------------------------------
key <- fread("data/literature_update/extraction/Collings et al 2023 Ecosphere/Column information.csv")
key

key[!Variable %in% c("year", "quad", "site", "area", "plot"), species := word(Description, 1, 6)]
key

key[grepl("categories", species), species := word(species, 3, 4)]
key[grepl("unidentified woody", species), species := word(species, 3, -1)]
key

spp_key <- key[!is.na(species)]
key <- key[is.na(species)]

dat <- fread("data/literature_update/extraction/Collings et al 2023 Ecosphere/Collings and Davalos - Data for repository.csv")
dat

dat.mlt <- melt(dat, id.vars = c("year", "quad", "site", "area", "plot"))
dat.mlt

dat.mlt.mrg <- merge(dat.mlt,
                     spp_key[, .(Variable, species)],
                     by.x = "variable", by.y = "Variable")
dat.mlt.mrg

# Drop the invasive plant removal data:
key
dat.mlt.mrg <- dat.mlt.mrg[area == "R", ]
dat.mlt.mrg <- dat.mlt.mrg[plot != "N", ]

unique(dat.mlt.mrg$plot)

dat.mlt.mrg[, plot := fcase(plot == "F", "No_Megafauna",
                            plot == "O", "High_Megafauna")]

dat.mlt.mrg

# Add species nativeness. Using New York Flora Atlas
unique(dat.mlt.mrg$species)
dat.mlt.mrg[species == "Acer negundo", nativeness := "Native"]
dat.mlt.mrg[species == "Acer rubrum", nativeness := "Native"]
dat.mlt.mrg[species == "Acer saccharum", nativeness := "Native"]
dat.mlt.mrg[species == "Actaea spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Ageratina altissima", nativeness := "Native"]
dat.mlt.mrg[species == "Alliaria petiolata", nativeness := "Introduced"]
dat.mlt.mrg[species == "Amelanchier spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Amphicarpaea bractiata", nativeness := "Native"]
dat.mlt.mrg[species == "Anemone acutiloba", nativeness := "Native"]
dat.mlt.mrg[species == "Aralia nudicaulis", nativeness := "Native"]
dat.mlt.mrg[species == "Arisaema triphyllum", nativeness := "Native"]
dat.mlt.mrg[species == "Asarum canadense", nativeness := "Native"]
dat.mlt.mrg[species == "Berberis thunbergii", nativeness := "Introduced"]
dat.mlt.mrg[species == "Carpinus caroliniana", nativeness := "Native"]
dat.mlt.mrg[species == "Cardamine concatenata", nativeness := "Native"]
dat.mlt.mrg[species == "Cardamine diphylla", nativeness := "Native"]
dat.mlt.mrg[species == "Carex spp", nativeness := "Native"]
dat.mlt.mrg[species == "Carex hitchcockiana", nativeness := "Native"]
dat.mlt.mrg[species == "Carex radiata", nativeness := "Native"]
dat.mlt.mrg[species == "Carya spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Caulophyllum spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Celastrus orbiculatus", nativeness := "Introduced"]
dat.mlt.mrg[species == "Circaea lutetiana", nativeness := "Native"]
dat.mlt.mrg[species == "Collinsonisa canadensis", nativeness := "Native"]
dat.mlt.mrg[species == "Cornus spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Dactylis glomerata", nativeness := "Introduced"]
dat.mlt.mrg[species == "Dryopteris spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Epipactis helleborine", nativeness := "Introduced"]
dat.mlt.mrg[species == "Eurybia divaricata", nativeness := "Native"]
dat.mlt.mrg[species == "Fagus grandifolia", nativeness := "Native"]
dat.mlt.mrg[species == "Fraxinus spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Galium circaezans", nativeness := "Native"]
dat.mlt.mrg[species == "Galium spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Geranium maculatum", nativeness := "Native"]
dat.mlt.mrg[species == "Geranium robertianum", nativeness := "Native"]
dat.mlt.mrg[species == "Geum spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Hydrophyllum virginianum", nativeness := "Native"]
dat.mlt.mrg[species == "Impatiens spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Lamiaceae spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Lapsana communis", nativeness := "Introduced"]
dat.mlt.mrg[species == "Ligustrum vulgare", nativeness := "Introduced"]
dat.mlt.mrg[species == "Lindera benzoin", nativeness := "Native"]
dat.mlt.mrg[species == "Liriodendron tulipifera", nativeness := "Native"]
dat.mlt.mrg[species == "Lonicera spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Maianthemum canadense", nativeness := "Native"]
dat.mlt.mrg[species == "Maianthemum racemosum", nativeness := "Native"]
dat.mlt.mrg[species == "Mitella diphylla", nativeness := "Native"]
dat.mlt.mrg[species == "Myosotis sylvatica", nativeness := "Introduced"]
dat.mlt.mrg[species == "Nabalus albus", nativeness := "Native"]
dat.mlt.mrg[species == "Onoclea sensibilis", nativeness := "Native"]
dat.mlt.mrg[species == "Ostrya virginiana", nativeness := "Native"]
dat.mlt.mrg[species == "Oxalis spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Pathenocissus quinquefolia", nativeness := "Native"]
dat.mlt.mrg[species == "Pathenocissus quinquefolia", species := "Parthenocissus quinquefolia"]

dat.mlt.mrg[species == "Pinus strobus", nativeness := "Native"]
dat.mlt.mrg[species == "Platanus occidentalis", nativeness := "Native"]
dat.mlt.mrg[species == "Poaceae spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Podophyllum peltatum", nativeness := "Native"]
dat.mlt.mrg[species == "Polystichum acrostichoides", nativeness := "Native"]
dat.mlt.mrg[species == "Polygonatum biflorum", nativeness := "Native"]
dat.mlt.mrg[species == "Prunus spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Quercus spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Ranunculus recurvatus", nativeness := "Native"]
dat.mlt.mrg[species == "Rhamnus cathartica", nativeness := "Introduced"]
dat.mlt.mrg[species == "Rosa multiflora", nativeness := "Introduced"]
dat.mlt.mrg[species == "Rubus sp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Sanguinarea canadensis", nativeness := "Native"]
dat.mlt.mrg[species == "Solidago caesia", nativeness := "Native"]
dat.mlt.mrg[species == "Solidago flexicaulis", nativeness := "Native"]
dat.mlt.mrg[species == "Solidago spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Staphylea trifolia", nativeness := "Native"]
dat.mlt.mrg[species == "Symphiotrichum spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Taraxicum spp.", nativeness := "Introduced"]
dat.mlt.mrg[species == "Thalictrum dioicum", nativeness := "Native"]
dat.mlt.mrg[species == "Tiarella cordifolium", nativeness := "Native"]
dat.mlt.mrg[species == "Tilia americana", nativeness := "Native"]
dat.mlt.mrg[species == "Toxicodendron radicans", nativeness := "Native"]
dat.mlt.mrg[species == "Tussilago farfara", nativeness := "Introduced"]
dat.mlt.mrg[species == "Ulmus spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Veronica spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Viburnum spp.", nativeness := "Native"]
dat.mlt.mrg[species == "Vinca minor", nativeness := "Introduced"]
dat.mlt.mrg[species == "Vincetoxicum rossicum", nativeness := "Introduced"]
dat.mlt.mrg[species == "Viola spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "Vitus spp.", nativeness := "Unknown"]
dat.mlt.mrg[species == "unidentified woody spp 1", nativeness := "Unknown"]
dat.mlt.mrg[species == "unidentified woody spp 2", nativeness := "Unknown"]
unique(dat.mlt.mrg$nativeness)

# growth form:
dat.mlt.mrg[species == "Acer negundo", growth_form := "tree"]
dat.mlt.mrg[species == "Acer rubrum", growth_form := "tree"]
dat.mlt.mrg[species == "Acer saccharum", growth_form := "tree"]
dat.mlt.mrg[species == "Actaea spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Ageratina altissima", growth_form := "forb"]
dat.mlt.mrg[species == "Alliaria petiolata", growth_form := "forb"]
dat.mlt.mrg[species == "Amelanchier spp.", growth_form := "shrub"]
dat.mlt.mrg[species == "Amphicarpaea bractiata", growth_form := "vine"] 
dat.mlt.mrg[species == "Anemone acutiloba", growth_form := "forb"]
dat.mlt.mrg[species == "Aralia nudicaulis", growth_form := "forb"]
dat.mlt.mrg[species == "Arisaema triphyllum", growth_form := "forb"]
dat.mlt.mrg[species == "Asarum canadense", growth_form := "forb"]
dat.mlt.mrg[species == "Berberis thunbergii", growth_form := "shrub"]
dat.mlt.mrg[species == "Carpinus caroliniana", growth_form := "tree"]
dat.mlt.mrg[species == "Cardamine concatenata", growth_form := "forb"]
dat.mlt.mrg[species == "Cardamine diphylla", growth_form := "forb"]
dat.mlt.mrg[species == "Carex spp", growth_form := "graminoid"]
dat.mlt.mrg[species == "Carex hitchcockiana", growth_form := "graminoid"]
dat.mlt.mrg[species == "Carex radiata", growth_form := "graminoid"]
dat.mlt.mrg[species == "Carya spp.", growth_form := "tree"]
dat.mlt.mrg[species == "Caulophyllum spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Celastrus orbiculatus", growth_form := "vine"] # perennial vine...
dat.mlt.mrg[species == "Circaea lutetiana", growth_form := "forb"]
dat.mlt.mrg[species == "Collinsonisa canadensis", growth_form := "forb"]
dat.mlt.mrg[species == "Cornus spp.", growth_form := "shrub"]
dat.mlt.mrg[species == "Dactylis glomerata", growth_form := "graminoid"]
dat.mlt.mrg[species == "Dryopteris spp.", growth_form := "fern"]
dat.mlt.mrg[species == "Epipactis helleborine", growth_form := "forb"]
dat.mlt.mrg[species == "Eurybia divaricata", growth_form := "forb"]
dat.mlt.mrg[species == "Fagus grandifolia", growth_form := "tree"]
dat.mlt.mrg[species == "Fraxinus spp.", growth_form := "tree"]
dat.mlt.mrg[species == "Galium circaezans", growth_form := "forb"]
dat.mlt.mrg[species == "Galium spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Geranium maculatum", growth_form := "forb"]
dat.mlt.mrg[species == "Geranium robertianum", growth_form := "forb"]
dat.mlt.mrg[species == "Geum spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Hydrophyllum virginianum", growth_form := "forb"]
dat.mlt.mrg[species == "Impatiens spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Lamiaceae spp.", growth_form := "unknown"]
dat.mlt.mrg[species == "Lapsana communis", growth_form := "forb"]
dat.mlt.mrg[species == "Ligustrum vulgare", growth_form := "shrub"]
dat.mlt.mrg[species == "Lindera benzoin", growth_form := "shrub"]
dat.mlt.mrg[species == "Liriodendron tulipifera", growth_form := "tree"]
dat.mlt.mrg[species == "Lonicera spp.", growth_form := "shrub"]
dat.mlt.mrg[species == "Maianthemum canadense", growth_form := "forb"]
dat.mlt.mrg[species == "Maianthemum racemosum", growth_form := "forb"]
dat.mlt.mrg[species == "Mitella diphylla", growth_form := "forb"]
dat.mlt.mrg[species == "Myosotis sylvatica", growth_form := "forb"]
dat.mlt.mrg[species == "Nabalus albus", growth_form := "forb"]
dat.mlt.mrg[species == "Onoclea sensibilis", growth_form := "fern"]
dat.mlt.mrg[species == "Ostrya virginiana", growth_form := "tree"]
dat.mlt.mrg[species == "Oxalis spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Parthenocissus quinquefolia", growth_form := "vine"]
# dat.mlt.mrg[species == "Pathenocissus quinquefolia", species := "Parthenocissus quinquefolia"]

dat.mlt.mrg[species == "Pinus strobus", growth_form := "tree"]
dat.mlt.mrg[species == "Platanus occidentalis", growth_form := "forb"]
dat.mlt.mrg[species == "Poaceae spp.", growth_form := "graminoid"]
dat.mlt.mrg[species == "Podophyllum peltatum", growth_form := "forb"]
dat.mlt.mrg[species == "Polystichum acrostichoides", growth_form := "fern"]
dat.mlt.mrg[species == "Polygonatum biflorum", growth_form := "forb"]
dat.mlt.mrg[species == "Prunus spp.", growth_form := "tree"]
dat.mlt.mrg[species == "Quercus spp.", growth_form := "tree"]
dat.mlt.mrg[species == "Ranunculus recurvatus", growth_form := "forb"]
dat.mlt.mrg[species == "Rhamnus cathartica", growth_form := "shrub"]
dat.mlt.mrg[species == "Rosa multiflora", growth_form := "shrub"]
dat.mlt.mrg[species == "Rubus sp.", growth_form := "shrub"]
dat.mlt.mrg[species == "Sanguinarea canadensis", growth_form := "forb"]
dat.mlt.mrg[species == "Solidago caesia", growth_form := "forb"]
dat.mlt.mrg[species == "Solidago flexicaulis", growth_form := "forb"]
dat.mlt.mrg[species == "Solidago spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Staphylea trifolia", growth_form := "shrub"]
dat.mlt.mrg[species == "Symphiotrichum spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Taraxicum spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Thalictrum dioicum", growth_form := "forb"]
dat.mlt.mrg[species == "Tiarella cordifolium", growth_form := "forb"]
dat.mlt.mrg[species == "Tilia americana", growth_form := "tree"]
dat.mlt.mrg[species == "Toxicodendron radicans", growth_form := "vine"]
dat.mlt.mrg[species == "Tussilago farfara", growth_form := "forb"]
dat.mlt.mrg[species == "Ulmus spp.", growth_form := "tree"]
dat.mlt.mrg[species == "Veronica spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Viburnum spp.", growth_form := "shrub"]
dat.mlt.mrg[species == "Vinca minor", growth_form := "forb"]
dat.mlt.mrg[species == "Vincetoxicum rossicum", growth_form := "vine"]
dat.mlt.mrg[species == "Viola spp.", growth_form := "forb"]
dat.mlt.mrg[species == "Vitus spp.", growth_form := "vine"]
dat.mlt.mrg[species == "unidentified woody spp 1", growth_form := "shrub"]
dat.mlt.mrg[species == "unidentified woody spp 2", growth_form := "shrub"]
unique(dat.mlt.mrg$growth_form)
unique(dat.mlt.mrg[is.na(growth_form)]$species)


# >>> Total richness ------------------------------------------------------

dat.mlt.mrg[, key := paste(year, site, plot, quad)]
richness <- dat.mlt.mrg[value > 0, .(richness = uniqueN(species)),
                        by = .(key, year, site, plot, quad)]
richness

# If plots have 0 species they will be dropped...
grid <- unique(dat.mlt.mrg[, .(key, year, site, plot, quad)])


richness <- merge(grid,
                  richness[, .(key, richness)], 
                   by = "key",
                   all.x = T)
richness[is.na(richness), ]
richness[is.na(richness), richness := 0]

richness <- richness[, .(mean = mean(richness), 
                         sd = sd(richness), 
                         n = uniqueN(quad)),
                     by = .(year, site, plot)]
richness
setorder(richness, site, year, plot)
richness
richness.cst <- dcast(richness,
                      ... ~ plot, value.var = c("mean", "sd", "n"))
richness.cst[, nativeness := "mixed"]
richness.cst

#
# >>> richness by nativeness ----------------------------------------------
# Might lose some species...so need a key...
richness2 <- dat.mlt.mrg[value > 0 & nativeness != "Unknown", 
                         .(richness = uniqueN(species)),
                         by = .(key, year, site, plot, quad, nativeness)]
richness2
richness2

grid <- CJ(key = unique(dat.mlt.mrg$key),
           nativeness = c("Native", "Introduced"))
grid <- merge(grid,
              unique(dat.mlt.mrg[, .(key, year, site, plot, quad)]),
              by = "key")
grid

grid[, key := paste(key, nativeness)]
richness2[, key := paste(key, nativeness)]

richness2 <- merge(grid,
                   richness2[, .(key, richness)], 
                   by = "key",
                   all.x = T)
richness2[is.na(richness), ]
richness2[is.na(richness), richness := 0]

richness2 <- richness2[, .(mean = mean(richness), sd = sd(richness), n = uniqueN(quad)),
                     by = .(year, site, plot, nativeness)]
richness2
setorder(richness2, site, year, plot)
richness2
richness.cst2 <- dcast(richness2,
                      ... ~ plot, value.var = c("mean", "sd", "n"))
richness.cst2



# >>> richness by growth form ----------------------------------------------
# Might lose some species...so need a key...
richness3 <- dat.mlt.mrg[value > 0 & growth_form != "Unknown", 
                         .(richness = uniqueN(species)),
                         by = .(key, year, site, plot, quad, growth_form)]
richness3
richness3

grid <- CJ(key = unique(dat.mlt.mrg$key),
           growth_form = unique(richness3$growth_form))
grid <- merge(grid,
              unique(dat.mlt.mrg[, .(key, year, site, plot, quad)]),
              by = "key")
grid

grid[, key := paste(key, growth_form)]
richness3[, key := paste(key, growth_form)]

richness3 <- merge(grid,
                   richness3[, .(key, richness)], 
                   by = "key",
                   all.x = T)
richness3[is.na(richness), ]
richness3[is.na(richness), richness := 0]
richness3

richness3 <- richness3[, .(mean = mean(richness), sd = sd(richness), n = uniqueN(quad)),
                       by = .(year, site, plot, growth_form)]
richness3
setorder(richness3, site, year, plot)
richness3
richness.cst3 <- dcast(richness3,
                       ... ~ plot, value.var = c("mean", "sd", "n"))
richness.cst3


# BIND:
richness.cst$growth_form <- "total"
richness.cst2$growth_form <- "total"
richness.cst3$nativeness <- "mixed"

richness.final <- rbind(richness.cst, richness.cst2, richness.cst3)
richness.final

setorder(richness.final, year, site)
fwrite(richness.final, "data//literature_update/extraction/Collings et al 2023 Ecosphere/richness_extracted.csv")

# >>> Abundance -----------------------------------------------------------

abund <- dat.mlt.mrg[, .(mean = mean(value), sd = sd(value), n = uniqueN(quad)),
                           by = .(year, site, area, species, plot, growth_form, nativeness)]
abund

# Drop those that are 0 for both treatments
abund.cst <- dcast(abund,
                   ... ~ plot, value.var = c("mean", "sd", "n"))
abund.cst

abund.cst <- abund.cst[mean_High_Megafauna > 0 & mean_No_Megafauna > 0, ]
abund.cst

abund.cst

fwrite(abund.cst, "data//literature_update/extraction/Collings et al 2023 Ecosphere/abundance_extracted.csv")


# Richins and Hulshof -----------------------------------------------------

dat <- read_excel("data/literature_update/extraction/Richins and Hulshof 2022 Frontiers in Conservation Science/table2.xlsx")
dat
setDT(dat)

abundance <- dat[Measurement == "Plant Area (cm^2)", ]
abundance

abundance <- melt(abundance,
                  id.vars = c("Measurement", "Treatment"))
# abundance[, value := as.numeric(value)]
abundance
abundance <- abundance %>%
  separate(col = "value", into = c("mean", "sd"), sep = " ± ", remove = F)
setDT(abundance)
abundance$value <- NULL

abundance[, Treatment := ifelse(Treatment == "C", "High_Megafauna", "Low_Megafauna")]
abundance

abundance.cst <- dcast(abundance, ... ~ Treatment, value.var = c("mean", "sd"))
abundance.cst

sort(unique(abundance.cst$variable))

# Maryland plant atlas
abundance.cst[variable == "Arabis lyrata", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Cerastium arvense", `:=` (nativeness = "Introduced", species_class = "Forb")]
abundance.cst[variable == "Liatris spicata", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Oenothera biennis", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Oenothera perennis", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Packera anonyma", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Polyganum tenue", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Polygala verticillata", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Sabatia angularis", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Sisyrinchium angustifolium", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Solidago nemoralis", `:=` (nativeness = "Native", species_class = "Forb")]
abundance.cst[variable == "Symphyotrichum depauperatum", `:=` (nativeness = "Native", species_class = "Forb")]

fwrite(abundance.cst, "data/literature_update/extraction/Richins and Hulshof 2022 Frontiers in Conservation Science/table2_extracted.csv")

# >>> Andreoni ------------------------------------------------------------

dat <- fread("data/literature_update/extraction/Andreoni et al 2024 Ecology/JRN086002_smes_quadrat_vegcover_data.csv")
dat

max(dat$year)

dat <- dat[year > 2013, ]
dat

unique(dat$treatment)
dat <- dat[treatment %in% c("C", "B"), ]
dat

# OUTPUTS:
# 1. dead cover
# 2. litter cover
# 3. abundance
# 4. diversity
# growth form x nativeness....

unique(dat$Species_binomial)

dat[Species_binomial == "Aphanostephus ramosissimus", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Bouteloua eriopoda", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Sporobolus flexuosus", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Sphaeralcea hastulata", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Yucca elata", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Sporobolus species", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Croton pottsii", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Uropappus lindleyi", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Prosopis glandulosa", `:=` (growth_form = "Tree", nativeness = "Native")]
dat[Species_binomial == "Aristida purpurea", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Gutierrezia sarothrae", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Senna bauhinioides", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Dalea nana", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Solanum elaeagnifolium", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Pomaria jamesii", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Hoffmannseggia glauca", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Aristida adscensionis", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Lesquerella fendleri", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Zinnia grandiflora", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Dimorphocarpa wislizeni", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Boerhavia spicata", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Bouteloua species", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Muhlenbergia porteri", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Ephedra trifurca", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Sphaeralcea incana", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Acourtia nana", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Machaeranthera tanacetifolia", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Tetraclea coulteri", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Opuntia phaeacantha", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Linum vernale", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Verbesina encelioides", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Eriogonum abertianum", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Sporobolus contractus", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Gutierrezia species", `:=` (growth_form = "Shrub", nativeness = "Native")]
dat[Species_binomial == "Lesquerella gordonii", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Aristida species", `:=` (growth_form = "Graminoid", nativeness = "Native")]
dat[Species_binomial == "Plantago patagonica", `:=` (growth_form = "Forb", nativeness = "Native")]
dat[Species_binomial == "Setaria leucopila", `:=` (growth_form = "Graminoid", nativeness = "Native")]

unique(dat[is.na(growth_form)]$Species_binomial)
unique(dat$Species_binomial)
dat

unique(dat$date)
unique(dat$season)

dat[, key := paste(site, block, plot, treatment, quad, year)]
grid <- CJ(key = unique(dat$key),
           Species_binomial = unique(dat[!Species_binomial %in% c("Litter", "no vegetation")]$Species_binomial))
grid

grid.m <- merge(grid,
              unique(dat[, .(key, site, block, plot, treatment, quad, year)]),
              by = "key",
              all.x = T)

grid.m

grid.m2 <- merge(grid.m,
                 unique(dat[, .(Species_binomial, growth_form, nativeness)]),
                 by = "Species_binomial",
                 all.x = T,
                 all.y = F)
grid.m2

# Now merge in cover...
plants <- dat[!is.na(growth_form) &
                live_dead == "live"]

plants

grid.m2[, key2 := paste(key, Species_binomial)]
plants[, key2 := paste(key, Species_binomial)]
plants.grid <- merge(grid.m2,
                     plants[, .(key2, cover)],
                     all.x = T,
                     by = "key2")
plants.grid

plants.grid[!is.na(cover)]
plants.grid[is.na(cover), cover := 0]

length(unique(plants.grid$key))
plants.grid
plants.abund <- plants.grid[, .(mean = mean(cover), sd = sd(cover), n = uniqueN(key)),
                           by = .(year, treatment, Species_binomial, nativeness, growth_form)]
plants.abund

# Cast wide:
plants.abund[, treatment := ifelse(treatment == "B", "No_Megafauna", "High_Megafauna")]
plants.abund.wide <- dcast(plants.abund,
                           ... ~ treatment,
                           value.var = c("mean", "sd", "n"))
plants.abund.wide

plants.abund.wide <- plants.abund.wide[!(mean_High_Megafauna == 0 & mean_No_Megafauna == 0), ]
plants.abund.wide

fwrite(plants.abund.wide, "data/literature_update/extraction/Andreoni et al 2024 Ecology/abundance_extracted.csv")
plants.abund.wide

# >>> Richness ------------------------------------------------------------
dat
# Number of species by group per subplot
# Then merge into a grid to get zeroes and take mean...
dat[is.na(cover)]
sort(unique(dat$Species_binomial))
richness.total <- dat[live_dead == "live" & !Species_binomial %in% c("Litter", "no vegetation"),
                      .(n_species = uniqueN(Species_binomial)),
                      by = .(key, treatment)]
richness.total$species_class <- "total"

richness.total


dat[growth_form %in% c("Tree", "Shrub"), growth_form := "Woody"]
richness.growthform <- dat[live_dead == "live" & !Species_binomial %in% c("Litter", "no vegetation"),
                           .(n_species = uniqueN(Species_binomial)),
                           by = .(key, treatment, growth_form)]
richness.growthform

setnames(richness.growthform, "growth_form", "species_class")
richness.growthform

unique(richness.growthform$species_class)
richness.combined <- rbind(richness.total, richness.growthform)
richness.combined

grid <- CJ(key = unique(grid$key),
           growth_form = c("total", unique(richness.combined$species_class)))
grid

grid.m1 <- merge(grid,
                 unique(dat[, .(key, treatment, year)]),
                 by = "key", all.x = T)
grid.m1

grid.m1[, key2 := paste(key, growth_form)]
richness.combined[, key2 := paste(key, species_class)]
richness.combined

grid.m2 <- merge(grid.m1,
                 richness.combined[, .(n_species, key2)],
                 by = "key2", all.x = T)
grid.m2

grid.m2[is.na(n_species), n_species := 0]
grid.m2

# Now summarize....
richness <- grid.m2[, .(mean = mean(n_species), sd = sd(n_species), n = uniqueN(key)),
                    by = .(treatment, growth_form, year)]
richness

richness[, treatment := ifelse(treatment == "B", "No_Megafauna", "High_Megafauna")]
richness.wide <- dcast(richness,
                           ... ~ treatment,
                           value.var = c("mean", "sd", "n"))
richness.wide

fwrite(richness.wide, "data/literature_update/extraction/Andreoni et al 2024 Ecology/richness_extracted.csv")



# Cain et al 2023 ---------------------------------------------------------

dat <- fread("data/literature_update/extraction/Cain et al 2023 Ecosphere/IndividualSpeciesCover.csv")
dat

dat.mlt <- melt(dat,
                id.vars = c("Sites", "Treatment"))
dat.mlt[value == "n/a", value := 0]
dat.mlt[, value := as.numeric(value)]
dat.mlt

class(dat.mlt$variable)
sort(unique(dat.mlt$variable))
dat.mlt[variable == "Acacia longifolia", growth_form := "woody"]
dat.mlt[variable == "Glycine clandestina", growth_form := "vine"]
dat.mlt[variable == "Gonocarpus teucrioides", growth_form := "woody"]
dat.mlt[variable == "Hardenbergia violaceae", growth_form := "vine"]
dat.mlt[variable == "Imperata cylindrica", growth_form := "graminoid"]
dat.mlt[variable == "Lepidosperma concavum", growth_form := "graminoid"]
dat.mlt[variable == "Lomandra longifolia", growth_form := "graminoid"]
dat.mlt[variable == "Pteridium esculentum", growth_form := "fern"]
dat.mlt[variable == "Themeda australis", growth_form := "graminoid"]
dat.mlt[variable == "Acacia terminalis", growth_form := "woody"]
dat.mlt[variable == "Elaeocarpus reticulatus", growth_form := "woody"]
dat.mlt[variable == "Hibbertia linearis", growth_form := "woody"]
dat.mlt[variable == "Ricinocarpus pinifolius", growth_form := "woody"]
dat.mlt[variable == "Banksia serrata", growth_form := "woody"]
dat.mlt[variable == "Bossiaea ensata", growth_form := "woody"]
dat.mlt[variable == "Bossiaea heterophylla", growth_form := "woody"]
dat.mlt[variable == "Caustis Flexuosa", growth_form := "woody"]

dat.mlt[variable == "Acacia ulicifolia", growth_form := "woody"]
dat.mlt[variable == "Smilax glyciphylla", growth_form := "vine"]
dat.mlt[variable == "Synoum glandulosum", growth_form := "woody"]

dat.mlt[variable == "Corymbia gummifera", growth_form := "woody"]
dat.mlt[variable == "Desmodium rhytidophyllus", growth_form := "forb"]
dat.mlt[variable == "Hibbertia scandens", growth_form := "vine"]

dat.mlt[variable == "Livistona australis", growth_form := "woody"]
dat.mlt[variable == "Marsdenia rostrata", growth_form := "vine"]
dat.mlt[variable == "Calochlaena dubia", growth_form := "fern"]

dat.mlt[variable == "Syncarpia glomulifera", growth_form := "woody"]
dat.mlt[variable == "Zieria smithii", growth_form := "woody"]
dat.mlt[variable == "Cissus hypoglauca", growth_form := "vine"]
dat.mlt[variable == "Dampiera purpurea", growth_form := "forb"]
dat.mlt[variable == "Lomandra glauca", growth_form := "forb"]
dat.mlt[variable == "Patersonia sericea", growth_form := "forb"]
dat.mlt[variable == "Persoonia linearis", growth_form := "woody"]
dat.mlt[variable == "Leucopogon lanceolatus", growth_form := "woody"]
dat.mlt[variable == "Eustrephus latifolius", growth_form := "vine"]
dat.mlt[variable == "Billardiera scandens", growth_form := "forb"]
dat.mlt[variable == "Dianella caerulea", growth_form := "forb"]
dat.mlt[variable == "Pultenaea palacea", growth_form := "woody"]
dat.mlt[variable == "Xanthosia pilosa", growth_form := "woody"]

sort(unique(dat.mlt[is.na(growth_form)]$variable))
dat.mlt
unique(dat.mlt$growth_form)


cover <- dat.mlt[, .(mean = mean(value), sd = sd(value), n = uniqueN(Sites)),
                 by = .(Treatment, growth_form, variable)]
cover

cover.wide <- dcast(cover,
                    ... ~ Treatment, value.var = c("mean", "sd", "n"))
cover.wide
fwrite(cover.wide, "data/literature_update/extraction/Cain et al 2023 Ecosphere/cover_extracted.csv")

nut <- fread("data/literature_update/extraction/Cain et al 2023 Ecosphere/NutrientTraitData.csv")
nut

setnames(nut, c("N(%)", "C(%)"), c("N", "C"))

nut.mlt <- melt(nut,
                measure.vars = c("N", "C"))
nut.mlt 


nut.sum <- nut.mlt[, .(mean = mean(value), sd = sd(value), n = .N), 
               by = .(Species, Treatment, variable)]
nut.sum


nut.sum.wide <- dcast(nut.sum,
                      ... ~ Treatment, value.var = c("mean", "sd", "n"))
fwrite(nut.sum.wide, "data/literature_update/extraction/Cain et al 2023 Ecosphere/nutrients_extracted.csv")



# Mohanbabu and Ritchie 2022 Journal of Ecology ---------------------------
# Plants:
# 4 .25x.25m plots within Each Plot_ID
dat <- read_excel("data/literature_update/extraction/Mohanbabu and Ritchie 2022 Journal of Ecology/LTGEdata_plant_biomass_Ritchielab.xlsx",
                  sheet = "plant_biomass_data")
setDT(dat)

setnames(dat, names(dat), gsub(" ", "_", names(dat)))
dat
dat[is.na(as.numeric(Plant_biomass)), ]
dat[, Plant_biomass := as.numeric(Plant_biomass)]

dat.sum <- dat[!is.na(Plant_biomass), .(mean_biomass = mean(Plant_biomass), sd_biomass = sd(Plant_biomass),
                   n = .N),
               by = .(Site, Graztrt, Year)]
dat.sum[, n := n*4]
dat.sum[, years_since := Year-1999]
dat.sum[, days_since := years_since*365]
dat.sum
dat.sum.cst <- dcast(dat.sum,
                     ... ~ Graztrt,
                     value.var = c("mean_biomass", "sd_biomass", "n"))
dat.sum.cst
fwrite(dat.sum.cst, "data/literature_update/extraction/Mohanbabu and Ritchie 2022 Journal of Ecology/biomass_extracted.csv")
dat.sum.cst

# N and P
dat <- read_excel("data/literature_update/extraction/Mohanbabu and Ritchie 2022 Journal of Ecology/LTGEdata_plant_biomass_Ritchielab.xlsx")
dat
setDT(dat)
msrs <- names(dat)[grepl("soil", names(dat)) | grepl("plant", names(dat))]

dat.mlt <- melt(dat,
                measure.vars = msrs)
dat.mlt

unique(dat.mlt$variable)
dat.mlt[, response := fcase(grepl("soiln", variable), "Soil N",
                            grepl("soilp", variable), "Soil P",
                            grepl("plantn", variable), "Plant N",
                            grepl("plantp", variable), "Plant P")]

dat.mlt[, year := str_sub(variable, -4, -1)]
dat.mlt[, years_since := as.numeric(year)-1999]
dat.mlt[, days_since := years_since*365]
dat.mlt

dat.mlt
dat.mlt.sum <- dat.mlt[, .(mean = mean(value), 
                           sd = sd(value),
                           n = .N),
                       by = .(Site, Graztrt, year, days_since, years_since,
                              response)]

dat.mlt.sum <- dat.mlt.sum[!is.na(mean)]
dat.mlt.sum[, n := n*4]

dat.sum.cst <- dcast(dat.mlt.sum,
                     ... ~ Graztrt,
                     value.var = c("mean", "sd", "n"))
dat.sum.cst
fwrite(dat.sum.cst, "data/literature_update/extraction/Mohanbabu and Ritchie 2022 Journal of Ecology/nutrients.csv")

# Sharp et al 2024 Journal of Ecology -------------------------------------

dat <- read_excel("data/literature_update/extraction/Sharp et al 2024 Journal of Ecology/Cumberland experiment.xlsx", sheet = "Plot data")
# filter out hogdamage == "yes'

# redox, ph, sal (porewater salinity), 
# tem (temperature), 
# co2 (CO2 flux), 
# ben.gr (green algae), ben.cy (blue-green algae), ben.di (diatom), 
# live.cov (live cover), dead.cov1-3 (dead cover in 3 quadrats, so increase N), 
# prop.sp.al * live.cov (for absoluate cover of Spartina alterniflora), 
# prop.sal * live.cov (for absolute cover of Salicornia spp
# prop.di.sp * live.cov for prop of veg that is Distichlis spicata
# 
# peri.ad1-3 for adult periwinkle snails in 3 quadrats
# peri.juv1-3 juvenile periwinkle snails
#fid.juv1-3 juvenile fiddler crabs
# crab1-3 adult fiddler crabs
# muss1-3 mussles
# ag.decom - aboveground decomposition
# bg.decom - beloground decomposition (% dry mass lost)
# som.lo - soil organic matter % in lower soil (5-15cm)
# som.up - ditto but for upper soil (0-5cm)
# soimois.lo - soil moisture % of mass in 5-15cm
# soimois.up - 0.5 cm
dat
setDT(dat)
names(dat)
dat <- dat[hogdamage == "n", 
           .(site, treatment, pair, plot, quarter, month,
              redox, ph, tem, sal, co2, ben.gr, ben.cy, ben.di,
              live.cov, dead.cov,
             prop.sp.al, prop.sal, prop.di.sp,
             peri.ad, peri.juv,
             fid.juv,
             crab,
             muss,
             ag.decom, bg.decom,
             som.lo, som.up,
             soimois.lo, soimois.up)]
dat
#
dat[, .(live.cov, prop.sp.al)]
dat[is.na(as.numeric(live.cov))]
dat[, live.cov := as.numeric(live.cov)/100]
dat
dat[, `:=` (Spartina_cover = live.cov * as.numeric(prop.sp.al),
            Salicornia_cover = live.cov * as.numeric(prop.sal),
            Distich_cover = live.cov * as.numeric(prop.di.sp))]
dat
dat <- dat[, !c("prop.sp.al", "prop.sal", "prop.di.sp", "live.cov")]

dat# Melt 
dat.mlt <- melt(dat,
                id.vars = c("site", "treatment", "pair", "plot", "quarter", "month"))
dat.mlt[, variable := as.character(variable)]
sort(unique(dat.mlt$variable))
dat.mlt[variable == "peri.juv"]

dat.mlt[, response := fcase(variable == "ag.decom", "Aboveground Decomposition Rate (% dry mass lost)",
                            variable == "bg.decom", "Belowground Decomposition Rate (% dry mass lost)",
                            variable == "ben.cy", "Blue-green Algae Abundance",
                            variable == "ben.gr", "Gree Algae Abundance",
                            variable == "ben.di", "Diatom Abundance",
                            variable == "co2", "CO2 flux rate",
                            variable == "crab", "Adult fiddler crab abundance",
                            variable == "dead.cov", "Dead Vegetation Cover (%)",
                            variable == "fid.juv", "Juvenile fiddler crab abundance",
                            variable == "muss", "Mussle abundance",
                            variable == "peri.ad", "Adult periwinkle abundance",
                            variable == "peri.juv", "Juvenile periwinkle abundance",
                            variable == "ph", "pH",
                            variable == "redox", "Redox",
                            variable == "sal", "Porewater salinity",
                            variable == "Salicornia_cover", "Salicornia spp cover",
                            variable == "soimois.lo", "Soil moisture (%) 5-15 cm depth",
                            variable == "soimois.up", "Soil moisture (%) 0-5 cm depth",
                            variable == "som.lo", "Soil Organic Matter (%) 5-15 cm depth",
                            variable == "som.up", "Soil Organic Matter (%) 0-5 cm depth",
                            variable == "Spartina_cover", "Spartina alterniflora cover",
                            variable == "tem", "Soil temperature",
                            variable == "Distich_cover", "Distichlis spicata cover",
                            variable == "XXXX", "XXXXX",
                            variable == "XXXX", "XXXXX",
                            variable == "XXXX", "XXXXX")]

sort(unique(dat.mlt$response))
dat.mlt[is.na(response), ]


# Damn
range(dat.mlt$month)
dat.mlt[, value := as.numeric(value)]
dat.mlt <- dat.mlt[!is.na(value)]
dat.mlt

dat.mlt.sum <- dat.mlt[, .(mean = mean(value), sd = sd(value), n = .N),
                       by = .(site, treatment, month, response)]
dat.mlt.sum


dat.mlt.sum.cst <- dcast(dat.mlt.sum,
                         ... ~ treatment,
                         value.var = c("mean", "sd", "n"))

setorder(dat.mlt.sum.cst, response, site, month)
dat.mlt.sum.cst
dat.mlt.sum.cst[, time_series_clean := seq(1:.N), by = .(site, response)]
dat.mlt.sum.cst

dat.mlt.sum.cst[, days_since := month * 30]
dat.mlt.sum.cst[, month := paste0(month, " months")]
dat.mlt.sum.cst

# setorder(dat.mlt.sum.cst, response, month, site)
dat.mlt.sum.cst
fwrite(dat.mlt.sum.cst, "data/literature_update/extraction/Sharp et al 2024 Journal of Ecology/extracted.csv")


# Wijas et al 2023 Ecology ------------------------------------------------

dat <- fread("data/literature_update/extraction/Wijas et al 2023 Ecology/FINAL_Data_terveg.csv")
dat

# Melt and make names legible.
dat.mlt <- melt(dat,
                id.vars = c("site", "set", "plot", "treatment", "date", "vegetation", "rain12", "averain"))
dat.mlt

unique(dat.mlt$variable)

dat.mlt <- dat.mlt[!is.na(value), ] 
dat.mlt[, response := fcase(variable == "deadgperc", "Dead grass cover",
                            variable == "litterperc", "Litter percent cover",
                            variable == "deadforbperc", "Dead forb percent cover",
                            variable == "tpmonth", "Termite activity")]
dat.mlt <- dat.mlt[!is.na(response), ]

dat.mlt.sum <- dat.mlt[, .(mean = mean(value),
                           sd = sd(value),
                           n = .N),
                       by = .(site, treatment, response, date, rain12, averain)]
dat.mlt.sum


dat.mlt.sum.cst <- dcast(dat.mlt.sum,
                         ... ~ treatment,
                         value.var = c("mean", "sd", "n"))

dat.mlt.sum.cst[, year := year(ym(date))]
dat.mlt.sum.cst

fwrite(dat.mlt.sum.cst, "data//literature_update/extraction/Wijas et al 2023 Ecology/extracted.csv")
# - vegetation: vegetation type of block
# - deadgperc: dead grass percentage cover
# - litterperc: litter percentage cover
# - deadforbperc: dead forb percentage cover
# - akang: average percentage of kangaroo dung found on transect
# - arabb: average percentage of rabbit dung found on transect
# - agoat: average percentage of goat dung found on transect
# - tpmonth: average toilet paper consumed per month (%)
# - rooafperkm: number of kangaroos seen per km on afternoon transect
# - goataf: number of goats seen per km on afternoon transect
# - rabbitaf: number of rabbit seen per km on afternoon transect
# - roospotperkm: number of kangaroos seen per km on spotlight transect
# - rabbitspot: number of rabbits seen per km on spotlight transect
# - averain: average annual rainfall in mm
# - rain12: average rainfall in the past 12 months in mm

