# Herbivores disrupt the flow of food resources to termites in dryland ecosystems
---
Baptiste J. Wijas, Will K. Cornwell, Mike Letnic

Data collected in 3 nature reserves across western New South Wales in Australia. These measurements were undertaken in large herbivore exclosures and adjacent control plots. 

There are 2 data files in this folder:
  1) FINAL_Data_terveg.csv = summary of measurements of vegetation and termite activity taken at each plot, each variable is a column:
        - site: nature reserve
        - set: blocks of exclosures
        - plot: ID name of each plot
        - treatment: herbivore exclusion treatment
        - date: sampling period
        - vegetation: vegetation type of block
        - deadgperc: dead grass percentage cover
        - litterperc: litter percentage cover
        - deadforbperc: dead forb percentage cover
        - akang: average percentage of kangaroo dung found on transect
        - arabb: average percentage of rabbit dung found on transect
        - agoat: average percentage of goat dung found on transect
        - tpmonth: average toilet paper consumed per month (%)
        - rooafperkm: number of kangaroos seen per km on afternoon transect
        - goataf: number of goats seen per km on afternoon transect
        - rabbitaf: number of rabbit seen per km on afternoon transect
        - roospotperkm: number of kangaroos seen per km on spotlight transect
        - rabbitspot: number of rabbits seen per km on spotlight transect
        - averain: average annual rainfall in mm
        - rain12: average rainfall in the past 12 months in mm
  2) ter_com.csv = termite community composition at each site and during each sampling period, columns =
        - site: nature reserve
        - date: sampling period
        - set: blocks of exclosures
        - vegetation: vegetation type of block
        - treatment: herbivore exclusion treatment
        - family: termite family
        - species: termite species
        - num: presence
        
There is one script file to run all the code in R which makes the figures and runs the analyses