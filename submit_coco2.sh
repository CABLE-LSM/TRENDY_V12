#!/usr/bin/env bash

# This script submits CABLE-POP runs on a list of stations

# Script requires the following files:
#   run_cable.sh
#   run_cable-pop_lib.sh
#
# Example
#   cp -R submit_coco2.sh, run_cable.sh, run_cable-pop_lib.sh, namelists/ ${rundir}/
#   cd rundir
#   ./run_coco2.sh
#
# Matthias Cuntz, Jul 2024

set -e

# -----------------------------------------------------
# Modules
#
if [[ -e ${HOME}/miniconda3/bin/conda ]] ; then
    eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
    conda activate pystd
else
    module purge
    module load intel-compiler/2021.8.0
    module load conda_concept/analysis3
fi

# -----------------------------------------------------
# Settings
#
# COCO2 base directory
coco2dir="${HOME}/projects/coco2/cable-pop"
sitelist="${coco2dir}/misc/coco2_sites.txt"
# sitelist="${coco2dir}/misc/coco2_test_sites1.txt"
run_model=1       # 1/0: Do/Do not run Cable-POP
clean_runpath=0   # 1/0: Do/Do not remove all files from runpath
plot_results=0    # 1/0: Do/Do not plot results
inodes=""  # empty or comma-delimited list of nodes to use
xnodes=""  # empty or comma-delimited list of nodes to exclude

# run script to submit
run_script="run_cable.sh"
# Code directory - set this to where your version of the code is located
cablecode="${HOME}/prog/github/cable/cable.cable-pop_trendy"
# Cable executable
exe="${cablecode}/bin/cable"
# TRENDY experiment (S0, S1, S2, S3, S4, S5, S6)
experiment="S3"
# Directory of namelist folder
rundir="${PWD}"
# Parameter directory
paramdir="${cablecode}/params/v12"
# LUT directory
lutdir="${cablecode}/params"
# Data directory
datadir="${coco2dir}"
# meteo input directory
MetPath="${datadir}/input"
# NDep file
CO2NdepFile="${MetPath}/AmaFACE_co2npdepforcing_1850_2100_AMB_JK.csv"
# parameter files
filename_veg="${paramdir}/def_veg_params.txt"
filename_soil="${paramdir}/def_soil_params.txt"
casafile_cnpbiome="${paramdir}/pftlookup.csv"

# plot script to submit
plot_script="plot_cable_site.sh"
# obs directory
obsdir="${coco2dir}/../obs4"
# id of run in plotting files
runid=coco2_no_iveg
# plot script python
pplotscript=${coco2dir}/plot/plot_cable-pop_vs_obs.py
# plot script R
rplotdir=${coco2dir}/plot/R
rplotscript=${rplotdir}/CABLE_plots.R

# ------------------ End Settings ---------------------

# Append the location of the cablepop python module to the PYTHONPATH
export PYTHONPATH=${cablecode}/scripts:${PYTHONPATH}

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
# 4. PBS -l excludenodes=host3:host4 or -l h=!node3:!node4 -l nodes=node1
#    Slurm: --nodelist=node1,node2 --exclude=node3,node4
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

sites=$(sed -e '1d' ${sitelist} | cut -f 1)
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

# -----------------------------------------------------
# Run model
#
if [[ ${run_model} -eq 1 ]] ; then
    echo "Submit model runs"
    # Write general settings into run script
    ${ised} -e "s|^\([ ]*\)isite=.*|\1isite=1|" \
	    -e "s|^\([ ]*\)outaverage7=.*|\1outaverage7='all'|" \
	    -e "s|^#${ttag}.*|#${ttag}02:00:00|" \
	    -e "s|^\([ ]*\)experiment=.*|\1experiment='${experiment}'|" \
	    -e "s|^\([ ]*\)cablecode=.*|\1cablecode='${cablecode}'|" \
	    -e "s|^\([ ]*\)rundir=.*|\1rundir='${rundir}'|" \
	    -e "s|^\([ ]*\)datadir=.*|\1datadir='${datadir}'|" \
	    -e "s|^\([ ]*\)MetPath=.*|\1MetPath='${MetPath}'|" \
	    -e "s|^\([ ]*\)exe=.*|\1exe='${exe}'|" \
	    -e "s|^\([ ]*\)filename_veg=.*|\1filename_veg='${filename_veg}'|" \
	    -e "s|^\([ ]*\)filename_soil=.*|\1filename_soil='${filename_soil}'|" \
	    -e "s|^\([ ]*\)casafile_cnpbiome=.*|\1casafile_cnpbiome='${casafile_cnpbiome}'|" \
	    ${run_script}

    RUN_IDS=
    for site in ${sites} ; do
	echo "    ${site}"
        experiment_name=${site}
        runpath="${coco2dir}/output_no_iveg/${experiment_name}"
        MetFile="${MetPath}/${experiment_name}_meteo.no_iveg.nc"

        # Write site specific settings into run script
        ${ised} -e "s|^#${ntag}.*|#${ntag}${experiment_name}|" \
		-e "s|^\([ ]*\)experiment_name=.*|\1experiment_name='${experiment_name}'|" \
		-e "s|^\([ ]*\)runpath=.*|\1runpath='${runpath}'|" \
		-e "s|^\([ ]*\)MetFile=.*|\1MetFile='${MetFile}'|" \
		${run_script}

	# Clean runpath
	if [[ ${clean_runpath} -eq 1 && -d ${runpath} ]] ; then
	    rm -r ${runpath}
	fi
	mkdir -p ${runpath}
	
    	# submit script
	if [[ -n ${inodes} ]] ; then
	    inode=$(echo ${inodes} | tr ',' '\n' | grep -v ${inode})
	    iopt=$(isub ${inode})
	fi
        RUN_IDS="${RUN_IDS}:$(${pqsub} ${iopt} ${xopt} ${run_script})"
    done
    echo "Submitted model jobs ${RUN_IDS}"
    echo "for sites $(echo ${sites})"

    # remove sed backup file
    if [[ -f ${run_script}.old ]] ; then rm ${run_script}.old ; fi
fi

# -----------------------------------------------------
# Plot results
#
if [[ ${plot_results} -eq 1 ]] ; then
    echo "Submit plotting jobs"
    # Write general settings into plot script
    ${ised} -e "s|^#${ttag}.*|#${ttag}00:09:59|" \
	    -e "s|^\([ ]*\)runid=.*|\1runid='${runid}'|" \
	    -e "s|^\([ ]*\)coco2dir=.*|\1coco2dir='${coco2dir}'|" \
	    -e "s|^\([ ]*\)sitelist=.*|\1sitelist='${sitelist}'|" \
	    -e "s|^\([ ]*\)obsdir=.*|\1obsdir='${obsdir}'|" \
	    -e "s|^\([ ]*\)datadir=.*|\1datadir='${datadir}'|" \
	    -e "s|^\([ ]*\)pplotscript=.*|\1pplotscript='${pplotscript}'|" \
	    -e "s|^\([ ]*\)rplotdir=.*|\1rplotdir='${rplotdir}'|" \
	    -e "s|^\([ ]*\)rplotscript=.*|\1rplotscript='${rplotscript}'|" \
	    ${plot_script}

    for site in ${sites} ; do
	echo "    ${site}"
        obsfile="${obsdir}/${site}_flux.nc"
	runpath="${coco2dir}/output_no_iveg/${site}"
	start_year=$(grep "${site}" ${sitelist} | cut -f 2)
	end_year=$(grep "${site}" ${sitelist} | cut -f 3)
	outfile="${runpath}/outputs/site_out_cable_${start_year}_${end_year}.nc"
	siteid="${site}_${runid}"
	ppdffile="${runpath}/outputs/${siteid}.pdf"

        ${ised} -e "s|^#${ntag}.*|#${ntag}${site}|" \
		-e "s|^\([ ]*\)site=.*|\1site='${site}'|" \
		-e "s|^\([ ]*\)obsfile=.*|\1obsfile='${obsfile}'|" \
		-e "s|^\([ ]*\)runpath=.*|\1runpath='${runpath}'|" \
		-e "s|^\([ ]*\)start_year=.*|\1start_year='${start_year}'|" \
		-e "s|^\([ ]*\)end_year=.*|\1end_year='${end_year}'|" \
		-e "s|^\([ ]*\)outfile=.*|\1outfile='${outfile}'|" \
		-e "s|^\([ ]*\)siteid=.*|\1siteid='${siteid}'|" \
		-e "s|^\([ ]*\)ppdffile=.*|\1ppdffile='${ppdffile}'|" \
		${plot_script}

	if [[ -n ${inodes} ]] ; then
	    inode=$(echo ${inodes} | tr ',' '\n' | grep -v ${inode})
	    iopt=$(isub ${inode})
	fi
	if [[ ${run_model} -eq 1 ]] ; then
            PLOT_IDS="${PLOT_IDS}:$($(dqsub ${RUN_IDS}) ${iopt} ${xopt} ${plot_script})"
        else
            PLOT_IDS="${PLOT_IDS}:$(${pqsub} ${iopt} ${xopt} ${plot_script})"
        fi
    done
    echo "Submitted plot jobs ${PLOT_IDS}"
    echo "for sites $(echo ${sites})"

    # remove sed backup file
    if [[ -f ${plot_script}.old ]] ; then rm ${plot_script}.old ; fi
fi

exit
