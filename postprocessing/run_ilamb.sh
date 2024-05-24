#!/usr/bin/env bash

# biocomp
#SBATCH --ignore-pbs
#SBATCH --job-name=ilamb
#SBATCH --output=%x-%j.out
#SBATCH --error=%x-%j.out
# #SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=01:00:00
#SBATCH --mem=64G
#SBATCH --mail-type=FAIL,STAGE_OUT,TIME_LIMIT,INVALID_DEPEND,END
#SBATCH --mail-user=matthias.cuntz@inrae.fr

# Script to run ILAMB  on output of postprocess_TRENDY.sh

set -ex

# --------------------------------------------------------------------
# Load Modules
# --------------------------------------------------------------------

eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
conda activate pystd


# --------------------------------------------------------------------
# Settings
# --------------------------------------------------------------------

#
# Paths
startdir=${PWD}
# Location of TRENDY output
simdir="/home/mcuntz/projects/cable/trendy_v12"
# ILAMB root directory
ilamb="/home/mcuntz/data/ilamb"
# ILAMB output directory
idir="${simdir}/ilamb"
# ILAMB config file
iconfig="${startdir}/ILAMB/ilamb_TRENDY.cfg"

# experiments to ILAMB
# exps="S0 S1 S2 S3"
exps="S3"

# export PATH=${PATH}:${HOME}/.local/bin
export MPLBACKEND=Agg
export ILAMB_ROOT=${ilamb}

mkdir -p ${idir}/RESULTS
for exp in ${exps} ; do
    mkdir -p ${idir}/MODELS/${exp}
    if [[ -d ${idir}/RESULTS/${exp} ]] ; then
        rm -r ${idir}/RESULTS/${exp}
    fi
done

if [[ ! -d ${ILAMB_ROOT}/DATA ]] ; then
    echo "ILAMB evaluation data need to be downloaded first (ilamb-fetch)"
fi

# loop over experiments
for exp in ${exps} ; do
    echo "ILAMB experiment ${exp}"
    ilamb-run --config ${iconfig} \
              --model_root ${idir}/MODELS/ \
              --build_dir ${idir}/RESULTS/${exp} \
              --models ${exp} \
              --regions global
done

exit
