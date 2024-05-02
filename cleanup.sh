#!/usr/bin/env bash

# biocomp
#SBATCH --ignore-pbs
#SBATCH --job-name=cclean
#SBATCH --output=%x-%j.out
#SBATCH --error=%x-%j.out
# #SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=01:00:00
#SBATCH --mem=1G
#SBATCH --mail-type=FAIL,STAGE_OUT,TIME_LIMIT,INVALID_DEPEND,END
#SBATCH --mail-user=matthias.cuntz@inrae.fr

exp_name="TRENDY_S0"
outpath="${HOME}/projects/cable/${exp_name}"
nruns=100
climate_restart='cru_climate_rst'
keep_dump=1
mergesteps='1700_1900 1901_2022'

# executable
mkdir -p ${outpath}/exe
mv ${outpath}/run1/cable ${outpath}/exe/

# job files
mkdir -p ${outpath}/job
mv ${PWD}/${exp_name}.o* ${outpath}/job/

for ((irun=1; irun<=${nruns}; irun++)) ; do
    # Restart files (includes namelists)
    mkdir -p ${outpath}/restart/run${irun}
    mv ${outpath}/run${irun}/restart/* ${outpath}/restart/run${irun}/

    # Climate restart
    mkdir -p ${outpath}/climate_restart
    mv ${outpath}/run${irun}/${climate_restart}.nc ${outpath}/climate_restart/${climate_restart}${irun}.nc

    # log files
    mkdir -p ${outpath}/logs/run${irun}
    mv ${outpath}/run${irun}/logs/* ${outpath}/logs/run${irun}/

    # landmasks
    mkdir -p ${outpath}/landmasks
    mv ${outpath}/run${irun}/landmask/landmask${irun}.nc ${outpath}/landmasks/

    # dump files
    if [[ ${keep_dump} -eq 1 || ("${outpath}" == "S3*" && ("${mergesteps}" == "*1700_1900" || "${mergesteps}" == "1901_*")) ]] ; then
        mkdir -p ${outpath}/dump_files/run${irun}
        mv ${outpath}/run${irun}/*_dump.nc ${outpath}/dump_files/run${irun}/
    fi

    # delete all the rest
    rm -r ${outpath}/run${irun}/
done
