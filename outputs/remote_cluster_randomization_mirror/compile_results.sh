#!/bin/sh

# SLURM commands:

#SBATCH --account meta_megafauna
#SBATCH --job-name=randomization_job
#SBATCH --mail-type=FAIL,END
#SBATCH --partition normal
#SBATCH --mem-per-cpu=5gb
#SBATCH --cpus-per-task 1
#SBATCH --time 3:00:00
#SBATCH --output=outfiles/randomization_%j.out

source ~/miniconda3/bin/activate meta_proj2
# conda activate meta_proj

Rscript compile_results.R >logs/compile_results_log_"$i".txt
