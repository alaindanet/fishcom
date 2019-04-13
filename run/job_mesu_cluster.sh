#!/bin/bash
#Request 20 CPU cores on one node (typical for MeSU-alpha)
#PBS -q alpha 
#PBS -l select=1:ncpus=20
#PBS -l walltime=40:00:00
#PBS -N temporal_networks
 
#The command below allows"module" to properly initialize a bash / sh shell in
#non-interactive mode. It sources the file for the chosen shell,

# Go to my place
cd $PBS_O_WORKDIR
echo $pwd > loc.txt

if [ -d "$HOME/bin" ] ; then
  PATH="$PATH:$HOME/bin"
  LD_LIBRARY_PATH="$LD_LIBRARY_PATH:/opt/dev/Modules/Libraries"
fi

# Load R version of the cluster
module load R/3.4.3
## Which version is used?
which R | echo 
which Rscript | echo 

R_LIBS_USER="$R_LIBS_USER:$HOME/.local/share/R-3.4.3/library/"
export R_LIBS_USER

# Launch the good guys: 

#./00_dependancies.sh
wait
./01_install_packages.sh
wait
# Build network species
./02_run_analysis.R "PBS" "species"
wait
# Build network classes
./02_run_analysis.R "PBS" "classes"

echo "Is it working ?"
