#!/usr/bin/env bash

# biocomp
#SBATCH --ignore-pbs
#SBATCH --job-name=ZM-Mon
#SBATCH --output=%x-%j.out
#SBATCH --error=%x-%j.out
# #SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=00:09:59
#SBATCH --mem=4G
#SBATCH --mail-type=FAIL,STAGE_OUT,TIME_LIMIT,INVALID_DEPEND,END
#SBATCH --mail-user=matthias.cuntz@inrae.fr

# Gadi
# https://opus.nci.org.au/display/Help/How+to+submit+a+job
#PBS -N ZM-Mon
#PBS -P rp23
#PBS -q normal
#PBS -p 600
#PBS -l walltime=00:09:59
#PBS -l mem=4GB
#PBS -l ncpus=1
#PBS -l storage=gdata/rp23+scratch/rp23+scratch/hh5
#PBS -l software=netCDF:MPI:Intel:GNU
#PBS -r y
#PBS -l wd
#PBS -j oe
#PBS -S /bin/bash

# Plot site-level Cable-POP output

if [[ -e ${HOME}/miniconda3/bin/conda ]] ; then  # biocomp
    eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
    conda activate pystd
else  # gadi
    pdir=${isdir}
    . /etc/bashrc
    module purge
    # ToDO
fi

site="FR-Hes"
# id of run in plotting files
runid="coco2_iveg"

# COCO2 base directory
coco2dir="${HOME}/projects/coco2/cable-pop"
sitelist="${coco2dir}/misc/coco2_sites.txt"
# obs directory
obsdir="${coco2dir}/../obs4"
# plot script python
pplotscript=${coco2dir}/plot/plot_cable-pop_vs_obs.py
# plot script R
rplotdir=${coco2dir}/plot/R
rplotscript=${rplotdir}/CABLE_plots.R

# Plot
obsfile="${obsdir}/${site}_flux.nc"
runpath="${coco2dir}/output/${site}"
start_year=$(grep "${site}" ${sitelist} | cut -f 2)
end_year=$(grep "${site}" ${sitelist} | cut -f 3)
outfile="${runpath}/outputs/site_out_cable_${start_year}_${end_year}.nc"
siteid="${site}_${runid}"
ppdffile="${runpath}/outputs/${siteid}.pdf"

# Python
python3 ${pplotscript} -t pdf -p ${ppdffile} ${obsfile} ${outfile}

# R
Rscript ${rplotscript} ${siteid} ${start_year} ${end_year} ${outfile} ${obsdir} ${rplotdir}

exit
