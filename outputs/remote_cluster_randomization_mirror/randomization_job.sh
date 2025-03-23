#!/bin/sh

# SLURM commands:

#SBATCH --account meta_megafauna
#SBATCH --job-name=randomization_job
#SBATCH --mail-type=FAIL,END
#SBATCH --partition normal
#SBATCH --mem-per-cpu=3gb
#SBATCH --cpus-per-task 1
#SBATCH --time 4:00:00
#SBATCH --output=outfiles/randomization_%j.out

source ~/miniconda3/bin/activate meta_proj2
# conda activate meta_proj

Rscript randomization_2025_feb.R $i >logs/log_"$i".txt
