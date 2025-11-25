# ecosystem_metamega

This repository contains the scripts and dataset to analyze the effects of native, introduced, 
and 'invasive' megafauna on other animals and dimensions of ecosystem functionality.

scripts/ contains all scripts, which are meant to be run in order. 
scripts/0_prepare_meta_update/ processes newly extracted literature and compiles it with original dataset (Trepel et al. 2024 NEE). Some of these scripts will not run without access to the Google Sheets where our raw data was collected
scripts/1_analyze/ contains all scripts for analysis.

outputs/ contains modelling outputs and summary tables

data/ contains the source meta-analysis data

builds/ includes the analysis ready dataset and sidecar files produced in the 'prepare_meta_update' scripts

figures/ contains main text and supplementary figures

Our scripts utilize the 'groundhog' package to manage package installation, ensuring that future users will use the same package versions as we did when we conducted this analysis, to ensure reproducibility.