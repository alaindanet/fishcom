#! /bin/bash -l

#SBATCH -J random-noise
#SBATCH -o random-noise.out
#SBATCH -e random-noise.error
#SBATCH --partition=type_2
#SBATCH -n 5

#SBATCH -t 04:00:00


# Chargement des modules
module load userspace/tr17.10
module load biology

module load lapack/3.7.1 
module load jags/4.3.0 
module load proj.4/4.9.3 
module load geos/3.6.2 
module load R/3.4.2

Rscript pcia_r_test_parallel.R
