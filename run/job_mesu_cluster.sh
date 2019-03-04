#!/bin/bash
#Request 20 CPU cores on one node (typical for MeSU-alpha)
#PBS -l select=1:ncpus=20
#PBS -l walltime=10:00:00
#PBS -N temporal_networks
 
#The command below allows"module" to properly initialize a bash / sh shell in
#non-interactive mode. It sources the file for the chosen shell,
 
# Launch the good guys: 

cd $PBS_O_WORKDIR
echo $pwd

#./00_dependancies.sh
wait
./01_install_packages.sh
wait
./02_run_analysis.r
wait

echo "Is it working ?"
