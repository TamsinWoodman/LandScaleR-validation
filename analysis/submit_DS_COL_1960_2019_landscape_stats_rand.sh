#!/bin/bash
#SBATCH --mem 20G
#SBATCH --partition=uoa-compute-priority
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 35:00:00
#SBATCH --job-name=downscaleStats

module load terra/1.5-21
module load rnaturalearth/0.1.0
module load rgdal/1.5-32

Rscript ~/03_Downscaling/src/DS_COL_1960_2019_landscape_stats.R "null_model" 1
