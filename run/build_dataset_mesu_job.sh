#!/bin/bash
#Request 90 CPU cores on one node (typical for MeSU-alpha)
#PBS -q alpha 
#PBS -l select=1:ncpus=5
#PBS -l walltime=48:00:00
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
mkdir $SCRATCH/$PROJECT/$DATADIR
mkdir $SCRATCH/$PROJECT/R
mkdir $SCRATCH/$PROJECT/analysis/misc

# copy some input files to  $SCRATCH directory
$PKG_DIR="../"
cp $PKG_DIR/$DATADIR/* $SCRATCH/$PROJECT/$DATADIR
cp $PKG_DIR/R/building_dataset.R $SCRATCH/$PROJECT/R/building_dataset.R
cp $PKG_DIR/$PARALLEL $SCRATCH/$PROJECT/$PARALLEL

cd $SCRATCH/$PROJECT/run
# Launch the good guys: 
wait
./01_install_packages.sh
wait
./02_build_fish_lot.R "PBS"
wait

# Copyfile
cd $PBS_O_WORKDIR
cp -p  $SCRATCH/$PROJECT/$DATADIR/fish_length.rda ../$DATADIR || exit 1

#clean the temporary directory
rm -rf "$SCRATCH/$PROJECT”/*
