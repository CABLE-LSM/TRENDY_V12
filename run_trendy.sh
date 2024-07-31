#!/usr/bin/env bash

# This script starts multiple sessions of the CABLE run script, each
# time using different land masks and output folders Before running
# this script, the following needs to be checked in the CABLE run
# script: PBS/SBATCH settings, Run sequence, CABLE settings

# Script requires the following files
# landmask_script --> creates landmasks
#
# run_script      --> runs CABLE instances
#     - requires run_cable-pop_lib.sh
# merge_script    --> merges CABLE outputs to latlon grid
#     - requires merge_to_output2d.py
# cleanup script  --> cleans up folder structure

set -e

#-------------------------------------------------------
# Modules
#-------------------------------------------------------

if [[ -e ${HOME}/miniconda3/bin/conda ]] ; then
    eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
    conda activate pystd
else
    module purge
    module load intel-compiler/2021.8.0
    module load conda_concept/analysis3
fi

#-------------------------------------------------------
# Settings
#-------------------------------------------------------
# TRENDY experiment (S0, S1, S2, S3, S4, S5, S6):
experiment="S3"
experiment_name="S3"
run_model=1       # 1/0: Do/Do not run the model
merge_results=1   # 1/0: Do/Do not merge results into one folder and backup restart,
                  # logs, landmasks, etc.
# mergesteps="zero_biomass spinup_nutrient_limited_1 spinup_nutrient_limited_2 1700_1900 1901_2022"   # sub-steps to be merged
mergesteps="1700_1900 1901_2022"  # sub-steps to be merged

### Spatial subruns ###
create_landmasks=1                 # 1: create new landmasks, 0: use existing masks
nruns=31                           # number of runs in parallel
# extent="64.0,66.0,60.0,62.0"     # "global" or "lon_min,lon_max,lat_min,lat_max"
extent="global"
climate_restart="cru_climate_rst"  # name of climate restart file (without file extension)
keep_dump=0                        # 1/0: keep or discard dump files.
                                   # They are always kept for LUC runs
inodes=""  # empty or comma-delimited list of nodes to use
xnodes=""  # empty or comma-delimited list of nodes to exclude

### Directories and files ###
# Code directory - set this to where your version of the code is located
cablecode="${HOME}/prog/github/cable/cable.cable-pop_trendy"
# Run directory (assumes that trend_v12 directory was copied to rundir)
rundir="${PWD}"
# Output directory- where the results are written to
outpath="${HOME}/projects/cable/trendy_v12_met/${experiment_name}"
# Parameter directory
paramdir="${cablecode}/params/v12"
# LUT directory
lutdir="${cablecode}/params"

# The various scripts used are contained in the configuration repository
landmask_script="${rundir}/split_landmask.py"
run_script="${rundir}/run_cable.sh"
merge_script="${rundir}/merge_outputs.sh"
cleanup_script="${rundir}/cleanup.sh"

# Cable executable- we should move this to bin
exe="${cablecode}/offline/cable"

# Append the location of the cablepop python module to the PYTHONPATH
export PYTHONPATH=${cablecode}/scripts:${PYTHONPATH}

# The location of the data- note that the "aux" variable has been removed,
# and all the data now lives in rp23/no_provenance
datadir="/home/mcuntz/data"
# Global Meteorology
GlobalMetPath="${datadir}/met_forcing/CRUJRA2023/daily_1deg_met"
# Global LUC
GlobalTransitionFilePath="${datadir}/cable/LUH2/GCB_2023/1deg/EXTRACT"
# Global Surface file
SurfaceFile="${datadir}/cable/CABLE-AUX/offline/gridinfo_CSIRO_1x1.nc"
# Global Land Mask
GlobalLandMaskFile="${datadir}/cable/ipbes/masks/glob_ipsl_1x1.nc"
# vegetation parameters
filename_veg="${paramdir}/def_veg_params.txt"
# soil parameters
filename_soil="${paramdir}/def_soil_params.txt"
# casa-cnp parameters
casafile_cnpbiome="${paramdir}/pftlookup.csv"
# mesophyll conductance lookup tables
gm_lut_bernacchi_2002="${lutdir}/gm_LUT_351x3601x7_1pt8245_Bernacchi2002.nc"
gm_lut_walker_2013="${lutdir}/gm_LUT_351x3601x7_1pt8245_Walker2013.nc"
# 13C
filename_d13c_atm="${lutdir}/graven_et_al_gmd_2017-table_s1-delta_13c-1700-2025.txt"

# OS
# Linux: "sed --in-place=.old" ; macOS/Unix: "sed -i .old"
if [[ $(uname -s) == Darwin ]] ; then
    ised="sed -i .old"
else
    ised="sed --in-place=.old"
fi

# Workload manager
# 1. PBS: "qsub" ; Slurm: "sbatch --ignore-pbs --parsable"
# 2. PBS: qsub -W "depend=afterok:id1:id2"
#    Slurm: sbatch --ignore-pbs --parsable --dependency=afterok:id1:id2
# 3. PBS: "PBS -N " ; Slurm: "SBATCH --job-name="
if [[ -e ${HOME}/miniconda3/bin/conda ]] ; then
    pqsub="sbatch --ignore-pbs --parsable"
    function dqsub()
    {
        echo "sbatch --ignore-pbs --parsable --dependency=afterok${1}"
    }
    function isub()
    {
	if [[ $# -gt 0 ]] ; then
            echo "--nodelist=${1}"
	else
	    echo ""
	fi
    }
    function xsub()
    {
	if [[ $# -gt 0 ]] ; then
            echo "--exclude=${1}"
	else
	    echo ""
	fi
    }
    ntag="SBATCH --job-name="
    ttag="SBATCH --time="
else
    pqsub="qsub"
    function dqsub()
    {
        echo "qsub -W \"depend=afterok${1}\""
    }
    # isub and xsub not tested on gadi
    function isub()
    {
	if [[ $# -gt 0 ]] ; then
            echo "-l \"nodes=${1/,/:}\""
	else
	    echo ""
	fi
    }
    function xsub()
    {
	if [[ $# -gt 0 ]] ; then
            echo "-l \"h=!${1/,/:!}\""
	else
	    echo ""
	fi
    }
    ntag="PBS -N "
    ttag="PBS -l walltime="
fi

## ---------------------------- End Settings ---------------------------------- ##

# -----------------------------------------------------------------------
# 1) Create landmasks (created in folders ${outpath}/runX/landmask)
# -----------------------------------------------------------------------
if [[ ${create_landmasks} -eq 1 ]] ; then
    echo "Create landmasks"
    ${landmask_script} ${GlobalLandMaskFile} ${nruns} ${outpath} -e ${extent}
    echo "Finished creating landmasks"
fi

# -----------------------------------------------------------------------
# 2) Run CABLE
# -----------------------------------------------------------------------
RUN_IDS=
if [[ -n ${inodes} ]] ; then
    inode=$(echo ${inodes} | cut -d ',' -f 1)
else
    iopt=''
fi
if [[ -n ${xnodes} ]] ; then
    xopt=$(xsub ${xnodes})
else
    xopt=''
fi
if [[ ${run_model} -eq 1 ]] ; then
    echo "Submit model runs"
    # 2.1) Write general settings into run script
    ${ised} -e "s|^\([ ]*\)#${ntag}.*|\1#${ntag}${experiment_name}|" ${run_script}
    ${ised} -e "s|^\([ ]*\)experiment=.*|\1experiment='${experiment}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)experiment_name=.*|\1experiment_name='${experiment_name}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)cablecode=.*|\1cablecode='${cablecode}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)rundir=.*|\1rundir='${rundir}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)datadir=.*|\1datadir='${datadir}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)exe=.*|\1exe='${exe}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)MetPath=.*|\1MetPath='${GlobalMetPath}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)MetVersion=.*|\1MetVersion='${MetVersion}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)TransitionFilePath=.*|\1TransitionFilePath='${GlobalTransitionFilePath}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)SurfaceFile=.*|\1SurfaceFile='${SurfaceFile}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)filename_veg=.*|\1filename_veg='${filename_veg}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)filename_soil=.*|\1filename_soil='${filename_soil}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)casafile_cnpbiome=.*|\1casafile_cnpbiome='${casafile_cnpbiome}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)gm_lut_bernacchi_2002=.*|\1gm_lut_bernacchi_2002='${gm_lut_bernacchi_2002}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)gm_lut_walker_2013=.*|\1gm_lut_walker_2013='${gm_lut_walker_2013}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)filename_d13c_atm=.*|\1filename_d13c_atm='${gm_lut_bernacchi_2002}'|" ${run_script}
    ${ised} -e "s|^\([ ]*\)ised=.*|\1ised='${ised}'|" ${run_script}

    # 2.2) Loop over landmasks and start runs
    for ((irun=1; irun<=${nruns}; irun++)) ; do
        runpath="${outpath}/run${irun}"
        ${ised} -e "s|^\([ ]*\)runpath=.*|\1runpath='${runpath}'|" ${run_script}
        ${ised} -e "s|^\([ ]*\)LandMaskFile=.*|\1LandMaskFile='${runpath}/landmask/landmask${irun}.nc'|" ${run_script}
	if [[ -n ${inodes} ]] ; then
	    inode=$(echo ${inodes} | tr ',' '\n' | grep -v ${inode})
	    iopt=$(isub ${inode})
	fi
        RUN_IDS="${RUN_IDS}:$(${pqsub} ${iopt} ${xopt} ${run_script})"
    done

    if [[ -f ${run_script}.old ]] ; then rm ${run_script}.old ; fi

    echo "Submitted model jobs ${RUN_IDS}"
fi

# -----------------------------------------------------------------------
# 3) Merge outputs (only if all previous runs were OK)
# -----------------------------------------------------------------------
if [[ ${merge_results} -eq 1 ]] ; then
    echo "Submit merge jobs"
    ftypes="cable casa LUC"
    outfinal="${outpath}/output"
    if [[ -d ${outfinal} ]] ; then
        rm -r ${outfinal}
    fi
    mkdir -p ${outfinal}

    for mergestep in ${mergesteps} ; do
        for ftype in ${ftypes} ; do
            if [[ ("${ftype}" != "LUC") || ("${experiment}" == "S3" && ("${mergestep}" == "1700_1900" || "${mergestep}" == "1901_"* )) ]] ; then
                ${ised} -e "s|^\([ ]*\)python3.*|\1python3 ${rundir}/merge_to_output2d.py -v -z -o ${outfinal}/cru_out_${ftype}_${mergestep}.nc ${outpath}/run*/outputs/cru_out_${ftype}_${mergestep}.nc|" ${merge_script}
                if [[ ${run_model} -eq 1 ]] ; then
                    MERGE_IDS="${MERGE_IDS}:$($(dqsub ${RUN_IDS}) ${merge_script})"
                else
                    MERGE_IDS="${MERGE_IDS}:$(${pqsub} ${merge_script})"
                fi
            fi
        done
    done

    if [[ -f ${merge_script}.old ]] ; then rm ${merge_script}.old ; fi

    echo "Submitted merge jobs ${MERGE_IDS}"

    # -----------------------------------------------------
    # 4) Backup and cleanup
    # -----------------------------------------------------

    echo "Submit cleaning job"
    ${ised} -e "s|^\([ ]*\)exp_name=.*|\1exp_name='${experiment_name}'|" ${cleanup_script}
    ${ised} -e "s|^\([ ]*\)outpath=.*|\1outpath='${outpath}'|" ${cleanup_script}
    ${ised} -e "s|^\([ ]*\)nruns=.*|\1nruns=${nruns}|" ${cleanup_script}
    ${ised} -e "s|^\([ ]*\)climate_restart=.*|\1climate_restart='${climate_restart}'|" ${cleanup_script}
    ${ised} -e "s|^\([ ]*\)keep_dump=.*|\1keep_dump=${keep_dump}|" ${cleanup_script}
    ${ised} -e "s|^\([ ]*\)mergesteps=.*|\1mergesteps='${mergesteps}'|" ${cleanup_script}

    CLEAN_ID=$(eval $(dqsub ${MERGE_IDS}) ${cleanup_script})

    if [[ -f ${cleanup_script}.old ]] ; then rm ${cleanup_script}.old ; fi

    echo "Submitted clean job ${CLEAN_ID}"
fi

exit
