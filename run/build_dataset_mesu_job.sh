#!/bin/bash
#Request 90 CPU cores on one node (typical for MeSU-alpha)
#PBS -q alpha 
#PBS -l select=1:ncpus=1
#PBS -l walltime=70:00:00
#PBS -N build_fish_lot 
 
#The command below allows"module" to properly initialize a bash / sh shell in
#non-interactive mode. It sources the file for the chosen shell,
#load appropriate modules
module purge
module load R/3.4.3

#move to PBS_O_WORKDIR
cd $PBS_O_WORKDIR

# Load R library: 
R_LIBS_USER="$R_LIBS_USER:$HOME/.local/share/R-3.4.3/library/"
export R_LIBS_USER

# Define scratch space scratch alpha for UV scratchbeta for ICE
SCRATCH=/scratchalpha/$USER/my_scratch_space
PROJECT=fishcom
DATADIR=data-raw/fishing_op_build
PARALLEL=analysis/misc/parallel_setup.R
mkdir $SCRATCH
mkdir $SCRATCH/$PROJECT
mkdir $SCRATCH/$PROJECT/data-raw
mkdir $SCRATCH/$PROJECT/data-raw/fishing_op_build
mkdir $SCRATCH/$PROJECT/R
mkdir $SCRATCH/$PROJECT/run
mkdir $SCRATCH/$PROJECT/analysis
mkdir $SCRATCH/$PROJECT/analysis/misc

# copy some input files to  $SCRATCH directory
cp ../$DATADIR/* $SCRATCH/$PROJECT/$DATADIR
cp ../R/building_dataset.R $SCRATCH/$PROJECT/R/building_dataset.R
cp ../$PARALLEL $SCRATCH/$PROJECT/$PARALLEL
cp ../run/* $SCRATCH/$PROJECT/run
cp ../DESCRIPTION $SCRATCH/$PROJECT

cd $SCRATCH/$PROJECT/run
# Launch the good guys: 
wait
./01_install_packages.sh
wait
./02_build_fish_lot.R "PBS"
wait
./02bis_check_fish_lot.R "PBS"

# Copyfile
cd $PBS_O_WORKDIR
cp -p  $SCRATCH/$PROJECT/$DATADIR/fish_length.rda ../$DATADIR || exit 1
cp -p  $SCRATCH/$PROJECT/$DATADIR/fish_lot_check.rda ../$DATADIR || exit 1

#clean the temporary directory
rm -rf "$SCRATCH/$PROJECT"/*
