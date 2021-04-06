#!/bin/bash
#SBATCH --job-name=prepClim
#SBATCH --output=/scratch/lb968/prep_worldclim_%a.log
#SBATCH --workdir=/scratch/lb968/
#SBATCH --time=12:00:00
#SBATCH --mem=150000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1

echo prep clim
date

# run application
srun Rscript /home/lb968/code/nslope_biomass/5.0_prep_worldclim.R
