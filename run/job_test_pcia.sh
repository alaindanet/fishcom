#!/bin/bash                                                                                                               

# Les lignes commançant par #SBATCH sont interprétées par SLURM.
# Elles permettent demander les ressources nécessaires au job.

# Nombre de Noeud
#SBATCH --nodes=1

# Nombre de processeur par noeud
#SBATCH --ntasks-per-node=1

# Nom du job
#SBATCH --job-name=essai_R

# Quantité de RAM par noeud
#SBATCH --mem=8G

# Quel type de machine demander (type_1 ou type_2)
#SBATCH --partition=type_2
 

# Chargement des modules
module load userspace/tr17.10
module load biology

module load lapack/3.7.1 
module load jags/4.3.0 
module load proj.4/4.9.3 
module load geos/3.6.2 
module load R/4.0.2

echo $HOME
source $HOME/.bashrc

echo "Which version is used?"
which R | echo
which Rscript | echo 

R_LIBS_USER="$R_LIBS_USER:$HOME/.local/share/R-4.0.2/library/"
export R_LIBS_USER
