#!/bin/bash
#PBS -q alpha 
#PBS -l select=1:ncpus=3
#PBS -l walltime=00:10:00
#PBS -N test_future

# Go to my place
cd $PBS_O_WORKDIR


# Load R version of the cluster
module load R/3.4.3

## Which version is used?
which R | echo 
which Rscript | echo 

R_LIBS_USER="$R_LIBS_USER:$HOME/.local/share/R-3.4.3/library/"
export R_LIBS_USER

# Launch the good guys: 
./A_test_future.R "PBS"
