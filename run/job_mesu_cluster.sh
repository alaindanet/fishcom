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


# Load R version of the cluster
module load R/3.4.3

# Load R library: 
R_LIBS_USER="$R_LIBS_USER:$HOME/.local/share/R-3.4.3/library/"
export R_LIBS_USER

# Define scratch space scratch alpha for UV scratchbeta for ICE
SCRATCH=/scratchalpha/$USER/my_scratch_space
PROJECT=fishcom
DATADIR=data
RAWDIR=data-raw
ANALYSIS=analysis
PARALLEL=analysis/misc/parallel_setup.R

mkdir $SCRATCH
mkdir $SCRATCH/$PROJECT
mkdir $SCRATCH/$PROJECT/data
mkdir $SCRATCH/$PROJECT/data/species
mkdir $SCRATCH/$PROJECT/data/classes
mkdir $SCRATCH/$PROJECT/data-raw
mkdir $SCRATCH/$PROJECT/data-raw/fishing_op_build
mkdir $SCRATCH/$PROJECT/R
mkdir $SCRATCH/$PROJECT/run
mkdir $SCRATCH/$PROJECT/analysis
mkdir $SCRATCH/$PROJECT/analysis/misc

# copy some input files to  $SCRATCH directory
cp -rp ../$DATADIR/* $SCRATCH/$PROJECT/$DATADIR
cp -rp ../$RAWDIR/fishing_op_build/* $SCRATCH/$PROJECT/$RAWDIR/fishing_op_build
cp -rp ../$RAWDIR/weight_length_coef.csv $SCRATCH/$PROJECT/$RAWDIR
cp -rp ../R/* $SCRATCH/$PROJECT/R
cp -rp ../$ANALYSIS/* $SCRATCH/$PROJECT/$ANALYSIS
cp -rp ../run/* $SCRATCH/$PROJECT/run
cp -rp ../DESCRIPTION $SCRATCH/$PROJECT

cd $SCRATCH/$PROJECT/run
# Launch the good guys: 

./00_dependancies.sh
wait
#./01_install_packages.sh
wait
# Build network species
./02_run_analysis.R "PBS" "species"
wait
# Build network classes
./02_run_analysis.R "PBS" "classes"
wait

echo "Is it working ?"

# Copyfile
cd $PBS_O_WORKDIR
cp -rp  $SCRATCH/$PROJECT/$DATADIR/* ../$DATADIR || exit 1

#clean the temporary directory
rm -rf $SCRATCH/$PROJECT/*

