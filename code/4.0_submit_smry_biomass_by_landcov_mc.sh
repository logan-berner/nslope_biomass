#!/bin/bash
#SBATCH --job-name=smryAGBlandcov
#SBATCH --output=/scratch/lb968/logs/smryAGBlandcov_%a.out
#SBATCH --workdir=/scratch/lb968/
#SBATCH --time=01:00:00
#SBATCH --mem=75000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000

echo summary_agb_by_landcov
date

module load R

srun Rscript /home/lb968/code/nslope_biomass/4.0_smry_biomass_by_landcov_mc.R ${SLURM_ARRAY_TASK_ID} 
