#! /bin/bash -l

#SBATCH -J network_analysis 
#SBATCH -o network_analysis.out
#SBATCH -e network_analysis.error
#SBATCH --partition=type_2
#SBATCH -n 10 

#SBATCH -t 06:00:00



# Chargement des modules
module load userspace/tr17.10
module load biology

module load lapack/3.7.1 
module load jags/4.3.0 
module load proj.4/4.9.3 
module load geos/3.6.2 
module load gcc/7.2.0
module load R/4.0.2


# Load R library: 
R_LIBS_USER="$R_LIBS_USER:$HOME/.local/share/R-4.0.2/library/"
export R_LIBS_USER
echo R_LIBS_USER

# Load R library: 
R_LIBS_USER="$R_LIBS_USER:$HOME/.local/share/R-3.4.2/library/"
export R_LIBS_USER

# Launch the good guys: 

#./00_dependancies.sh
#wait
#./01_install_packages.sh
#wait
# Build network species
./02_run_analysis.R "Slurm" "species"
wait
# Build network classes
./02_run_analysis.R "Slurm" "classes"
wait

echo "Is it working ?"
