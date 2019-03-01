#!/bin/bash
#Request 20 CPU cores on one node (typical for MeSU-alpha)
#PBS -l select=1:ncpus=20
#PBS -l walltime=10:00:00
#PBS -N temporal networks in fire 
 
#The command below allows"module" to properly initialize a bash / sh shell in
#non-interactive mode. It sources the file for the chosen shell,
 
# Launch the good guys: 

./00_dependancies.sh
wait
./01_dependancies.sh
wait
./02_run_analysis.r
wait

echo "Is it working ?"
