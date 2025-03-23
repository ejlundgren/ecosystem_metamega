#!/bin/sh
# This script will call a bunch of bash scripts in parallel that will send job to array
# Doing this in 2 bash scripts is annoying but necessary so that things aren't sequential

#SBATCH --account meta_megafauna
#SBATCH --job-name=randomization_boss
#SBATCH --mail-type=FAIL,END
#SBATCH --mail-user=ejlundgren@bio.au.dk
#SBATCH --partition normal
#SBATCH --mem-per-cpu=1gb
#SBATCH --cpus-per-task 1
#SBATCH --time 00:30:00
#SBATCH --output=outfiles/randomization_boss_%j.out

max_formulas=38
# number of models, not chunks in this case

for (( i=1; i<=${max_formulas}; i++ )); 
  do
  export i
  sbatch randomization_job.sh
  done
  
  
