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
#SBATCH --time=2:30:00
#SBATCH --mem=4G
#SBATCH --mail-type=FAIL,STAGE_OUT,TIME_LIMIT,INVALID_DEPEND,END
#SBATCH --mail-user=matthias.cuntz@inrae.fr

eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
conda activate pystd

# This line is replaced with the appropriate target during run
python3 /home/mcuntz/prog/github/cable/trendy_v12/merge_to_output2d.py -v -z -o /home/mcuntz/prog/github/cable/trendy_v12/trendy_v12/output/cru_out_LUC_1901_2022.nc /home/mcuntz/prog/github/cable/trendy_v12/trendy_v12/run*/outputs/cru_out_LUC_1901_2022.nc
