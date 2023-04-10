#!/bin/bash
#SBATCH --mem 32G
#SBATCH --partition=uoa-compute-priority
#SBATCH -N 1
#SBATCH -n 1
#SBATCH -t 65:00:00
#SBATCH --job-name=downscaleC

module load terra/1.5-21

sleep 10

Rscript ~/03_Downscaling/src/Maxwell_DS_HILDA_COL_1960_2019.R "fuzzy" 1.5
