#!/usr/bin/env bash

# biocomp
#SBATCH --ignore-pbs
#SBATCH --job-name=cmerge
#SBATCH --output=%x-%j.out
#SBATCH --error=%x-%j.out
# #SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=12:30:00
#SBATCH --mem=16G
#SBATCH --mail-type=FAIL,STAGE_OUT,TIME_LIMIT,INVALID_DEPEND,END
#SBATCH --mail-user=matthias.cuntz@inrae.fr

eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
conda activate pystd

# This line is replaced with the appropriate target during run
python3 
