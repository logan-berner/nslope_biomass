#!/bin/bash
#SBATCH --job-name=calcNDVI
#SBATCH --output=/scratch/lb968/logs/calcNDVI_%a.out
#SBATCH --workdir=/scratch/lb968/
#SBATCH --time=0:40:00
#SBATCH --mem=250000
#SBATCH --partition=all
#SBATCH --cpus-per-task=1
#SBATCH --array=1-1000

echo calcNDVI
date

module load R

srun Rscript /home/lb968/code/nslope_biomass/2.0_calc_regional_ndvi_mc.R ${SLURM_ARRAY_TASK_ID} 
