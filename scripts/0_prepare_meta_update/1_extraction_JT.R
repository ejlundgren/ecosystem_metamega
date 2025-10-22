
library("groundhog")
# Use metaDigitize to extract data

groundhog.library(pkg = c("data.table", "tidyverse",
                          "metaDigitise"),
                  date = "2025-04-15")

##### Orr et al 2022 Journal of Ecology ----------------------------------------
dt_div <- fread("data/literature_update/extraction/Orr et al 2022 Journal of Ecology/Tejon_PlotLevel_Diversity.csv")
dt_plants <- fread("data/literature_update/extraction/Orr et al 2022 Journal of Ecology/Tejon_2019_PlotLevel_PlantStructure.csv")

dt_orr <- dt_div %>% 
  left_join(dt_plants) %>% 
  filter(Treatment != "Open") %>% 
  mutate(Treatment = case_when(
    Treatment == "Partial" ~ "high_megafauna", 
    Treatment == "Total" ~ "low_megafauna", 
  )) %>% 
  dplyr::select(Treatment, Plot_ID, richness, plant_cover = total.cover, 
                bare_cover = total.bare, litter_volume = litter.volume) %>% 
  pivot_longer(cols = c("richness", "plant_cover", "bare_cover", "litter_volume")) %>% 
  group_by(Treatment, name) %>% 
  summarize(mean = mean(value), 
            sd = sd(value)) %>% 
  pivot_wider(
    id_cols = name,
    names_from = Treatment,
    values_from = c(mean, sd)#,
    #names_glue = "{Treatment}_{.value}"
  )
fwrite(dt_orr, "data/literature_update/extraction/Orr et al 2022 Journal of Ecology/summary_for_dataset.csv")

##### Charles et al 2021 Ecology and Evolution ----------------------------------------
dt_t1 <- fread("data/literature_update/extraction/Charles et al 2021 Ecology and Evolution/table_s1.csv")

dt_charles_meso <- dt_t1 %>% 
  filter(!grepl("C", Treatment) & !Treatment == "MW") %>% 
  mutate(Treatment = case_when(
    Treatment == "W" ~ "high_megafauna", 
    Treatment == "O" ~ "low_megafauna", 
  )) %>% 
  dplyr::select(Treatment, Block, n_trees = `Trees >1m`, 
                mounds_per_ha = `Mounds per ha`) %>% 
  pivot_longer(cols = c("mounds_per_ha", "n_trees")) %>% 
  group_by(Treatment, name) %>% 
  summarize(mean = mean(value), 
            sd = sd(value)) %>% 
  pivot_wider(
    id_cols = name,
    names_from = Treatment,
    values_from = c(mean, sd)#,
    #names_glue = "{Treatment}_{.value}"
  ) %>% mutate(what = "mesoherbivores vs exclosures")

dt_charles_mega <- dt_t1 %>% 
  filter(!grepl("C", Treatment) & !Treatment == "W") %>% 
  mutate(Treatment = case_when(
    Treatment == "MW" ~ "high_megafauna", 
    Treatment == "O" ~ "low_megafauna", 
  )) %>% 
  dplyr::select(Treatment, Block, n_trees = `Trees >1m`, 
                mounds_per_ha = `Mounds per ha`) %>% 
  pivot_longer(cols = c("mounds_per_ha", "n_trees")) %>% 
  group_by(Treatment, name) %>% 
  summarize(mean = mean(value), 
            sd = sd(value)) %>% 
  pivot_wider(
    id_cols = name,
    names_from = Treatment,
    values_from = c(mean, sd)#,
    #names_glue = "{Treatment}_{.value}"
  ) %>% mutate(what = "all herbivores vs exclosures")

dt_charles <- rbind(dt_charles_mega, dt_charles_meso)

fwrite(dt_charles, "data/literature_update/extraction/Charles et al 2021 Ecology and Evolution/summary_for_dataset.csv")

## Trepel et al 2024 Conservation Science and Practice ---------

dt <- fread("data/literature_update/extraction/Trepel et al 2024 Conservation Science and Practice/exclosure_plot_data.csv")


dt_trepel_soil <- dt %>% 
  filter(Species_Co == "Soil") %>% 
  mutate(Treatment = case_when(
    Type == "Grazed" ~ "high_megafauna", 
    Type == "Exclosure" ~ "low_megafauna", 
  )) %>% 
  dplyr::select(Treatment, N, C, P, K, Na, Ca, Mg, N_P, N_Ca, P_Ca) %>% 
  pivot_longer(cols = c(N, C, P, K, Na, Ca, Mg, N_P, N_Ca, P_Ca)) %>% 
  group_by(Treatment, name) %>% 
  summarize(mean = mean(value), 
            sd = sd(value)) %>% 
  pivot_wider(
    id_cols = name,
    names_from = Treatment,
    values_from = c(mean, sd)#,
    #names_glue = "{Treatment}_{.value}"
  ) %>% mutate(what = "soil")

dt_trepel_veg <- dt %>% 
  filter(!Species_Co == "Soil") %>% 
  mutate(Treatment = case_when(
    Type == "Grazed" ~ "high_megafauna", 
    Type == "Exclosure" ~ "low_megafauna", 
  )) %>% 
  dplyr::select(Treatment, N, C, P, K, Na, Ca, Mg, N_P, N_Ca, P_Ca) %>% 
  pivot_longer(cols = c(N, C, P, K, Na, Ca, Mg, N_P, N_Ca, P_Ca)) %>% 
  group_by(Treatment, name) %>% 
  summarize(mean = mean(value), 
            sd = sd(value)) %>% 
  pivot_wider(
    id_cols = name,
    names_from = Treatment,
    values_from = c(mean, sd)#,
    #names_glue = "{Treatment}_{.value}"
  ) %>% mutate(what = "veg")

dt_trepel_soil

dt_charles <- rbind(dt_charles_mega, dt_charles_meso)

fwrite(dt_charles, "data/literature_update/extraction/Charles et al 2021 Ecology and Evolution/summary_for_dataset.csv")
