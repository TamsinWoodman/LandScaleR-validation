#!/bin/bash
#SBATCH --mem 24G
#SBATCH --partition=uoa-compute-priority
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 7:00:00
#SBATCH --job-name=downscaleC

module load terra/1.5-21

Rscript ~/03_Downscaling/src/Maxwell_DS_HILDA_COL_1960_2019_one_sim.R "fuzzy" 1.0 10
