#!/usr/bin/env bash

# This script starts multiple sessions of the CABLE run script, each time using different land masks and output folders
# Before running this script, the following needs to be checked in the CABLE run script:
# PBS settings, Run sequence, CABLE settings 

# Script requires the following files
# landmask_script --> creates landmasks
#
# run_script      --> runs CABLE instances
#     - requires run_cable-pop_lib.sh
# merge_script    --> merges CABLE outputs to latlon grid
#     - requires merge_to_output2d.py 
# cleanup script  --> cleans up folder structure

#-------------------------------------------------------
# Modules
#-------------------------------------------------------
module purge
module load intel-compiler/2021.8.0
module load conda_concept/analysis3

#-------------------------------------------------------
# Settings
#-------------------------------------------------------
experiment=""
experiment_name="${experiment}"
run_model=1       # run the model or just do other steps (e.g. merging)?
merge_results=0   # after runs are finished, merge results into one folder and backup 
                  # restart, logs, landmasks etc. (1) or keep folder structure as it is (0).
                  # The latter is useful if runs are to be resumed from restart files. 
#mergesteps="zero_biomass spinup_nutrient_limited_1 spinup_nutrient_limited_2 1700_1900 1901_2022"   # sub-steps to be merged
mergesteps="1700_1900 1901_2022"

### Spatial subruns ###
create_landmasks=1               # create new landmask files (1) or use existing ones (0)?
nruns=100                        # number of runs in parallel
#extent="64.0,66.0,60.0,62.0"    # "global" or "lon_min,lon_max,lat_min,lat_max"
extent="global"
climate_restart="cru_climate_rst"       # name of climate restart file (without file extension)
keep_dump=1                             # keep dump files (1) or discard (0)? They are always kept for LUC runs


### Directories and files###
# Code directory- set this to where your version of the code is located
cablecode=""
# Run directory
rundir="${PWD}"
# Output directory- where the results are written to
outpath="${rundir}/${experiment_name}"
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
datadir="/g/data/rp23/data/no_provenance/"
# Global Meteorology
GlobalMetPath="/g/data/rp23/experiments/2024-03-12_CABLE4-dev/lw5085/data_links/"
# Global LUC
GlobalTransitionFilePath="${datadir}/luc/LUH2_GCB_1x1/v2023"
# Global Surface file 
SurfaceFile="${datadir}/gridinfo/gridinfo_CSIRO_1x1.nc"
# Global Land Mask
GlobalLandMaskFile="${datadir}/landmask/glob_ipsl_1x1.nc"
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

# OS and Workload manager
ised="sed --in-place=.old"  # Linux: "sed --in-place=.old" ; macOS/Unix: "sed -i .old"
pqsub="qsub"  # PBS: "qsub" ; Slurm: "sbatch --parsable"
# PBS: qsub -W "depend=afterok:id1:id2"
# Slurm: sbatch --dependency=afterok:id1:id2
function dqsub()
{
    # PBS
    echo "qsub -W \"depend=afterok${1}\""
    # # Slurm
    # echo "sbatch --parsable --dependency=afterok${1}"
}
ntag="PBS -N "  # PBS: "PBS -N " ; Slurm: "SBATCH --job-name="

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
if [[ ${run_model} -eq 1 ]] ; then
    echo "Submit model runs"
    # 2.1) Write general settings into run script
    ${ised} -e "s|^#${ntag}.*|#${ntag}${experiment_name}|" ${run_script}
    ${ised} -e "s|^experiment=.*|experiment='${experiment}'|" ${run_script}
    ${ised} -e "s|^experiment_name=.*|experiment_name='${experiment_name}'|" ${run_script}
    ${ised} -e "s|^cablecode=.*|cablecode='${cablecode}'|" ${run_script}
    ${ised} -e "s|^rundir=.*|rundir='${rundir}'|" ${run_script}
    ${ised} -e "s|^datadir=.*|datadir='${datadir}'|" ${run_script}
    ${ised} -e "s|^exe=.*|exe='${exe}'|" ${run_script}
    ${ised} -e "s|^MetPath=.*|MetPath='${GlobalMetPath}'|" ${run_script}
    ${ised} -e "s|^MetVersion=.*|MetVersion='${MetVersion}'|" ${run_script}
    ${ised} -e "s|^TransitionFilePath=.*|TransitionFilePath='${GlobalTransitionFilePath}'|" ${run_script}
    ${ised} -e "s|^SurfaceFile=.*|SurfaceFile='${SurfaceFile}'|" ${run_script}
    ${ised} -e "s|^filename_veg=.*|filename_veg='${filename_veg}'|" ${run_script}
    ${ised} -e "s|^filename_soil=.*|filename_soil='${filename_soil}'|" ${run_script}
    ${ised} -e "s|^casafile_cnpbiome=.*|casafile_cnpbiome='${casafile_cnpbiome}'|" ${run_script}
    ${ised} -e "s|^gm_lut_bernacchi_2002=.*|gm_lut_bernacchi_2002='${gm_lut_bernacchi_2002}'|" ${run_script}
    ${ised} -e "s|^gm_lut_walker_2013=.*|gm_lut_walker_2013='${gm_lut_walker_2013}'|" ${run_script}
    ${ised} -e "s|^filename_d13c_atm=.*|filename_d13c_atm='${gm_lut_bernacchi_2002}'|" ${run_script}
    ${ised} -e "s|^ised=.*|ised='${ised}'|" ${run_script}

    # 2.2) Loop over landmasks and start runs
    for ((irun=1; irun<=${nruns}; irun++)) ; do
        runpath="${outpath}/run${irun}"
        ${ised} -e "s|^runpath=.*|runpath='${runpath}'|" ${run_script}
        ${ised} -e "s|^LandMaskFile=.*|LandMaskFile='${runpath}/landmask/landmask${irun}.nc'|" ${run_script}
        RUN_IDS="${RUN_IDS}:$(${pqsub} ${run_script})"
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
                ${ised} -e "s|^python3.*|python3 ${rundir}/merge_to_output2d.py -v -z -o ${outfinal}/cru_out_${ftype}_${mergestep}.nc ${outpath}/run*/outputs/cru_out_${ftype}_${mergestep}.nc|" ${merge_script}
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
    ${ised} -e "s|^exp_name=.*|exp_name='${experiment_name}'|" ${cleanup_script}
    ${ised} -e "s|^outpath=.*|outpath='${outpath}'|" ${cleanup_script}
    ${ised} -e "s|^nruns=.*|nruns=${nruns}|" ${cleanup_script}
    ${ised} -e "s|^climate_restart=.*|climate_restart='${climate_restart}'|" ${cleanup_script}
    ${ised} -e "s|^keep_dump=.*|keep_dump=${keep_dump}|" ${cleanup_script}
    ${ised} -e "s|^mergesteps=.*|mergesteps='${mergesteps}'|" ${cleanup_script}

    CLEAN_ID=$(eval $(dqsub ${MERGE_IDS}) ${cleanup_script})

    if [[ -f ${cleanup_script}.old ]] ; then rm ${cleanup_script}.old ; fi

    echo "Submitted clean job ${CLEAN_ID}"
fi

exit
