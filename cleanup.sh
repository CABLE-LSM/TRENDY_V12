#!/usr/bin/env bash

# Gadi
# https://opus.nci.org.au/display/Help/How+to+submit+a+job
#PBS -N CABLE_cleanup
#PBS -P pr09
#PBS -q normal
#PBS -l walltime=00:30:00
#PBS -l mem=1GB
#PBS -l ncpus=1
#PBS -l storage=gdata/x45+scratch/pt17+gdata/pr09+scratch/pr09
#PBS -l software=netCDF:MPI:Intel:GNU:scorep
#PBS -r y
#PBS -l wd
#PBS -j oe
#PBS -S /bin/bash

exp_name='TRENDY_S0'
outpath='/g/data/pr09/TRENDY_v12/S0'
nruns=100
climate_restart='cru_climate_rst'
keep_dump=1
mergesteps='1700_1900 1901_2022'

# Info
echo ""
echo "Cleaning ${exp_name}"
echo "in path ${outpath}"
echo "Keeping dump: ${keep_dump}"
echo "for steps: ${mergesteps}"
echo ""

# executable
echo "Move executable"
mkdir -p ${outpath}/exe
mv ${outpath}/run1/cable ${outpath}/exe/

# job files
echo "Move job output files"
mkdir -p ${outpath}/job
mv ${PWD}/${exp_name}.o* ${outpath}/job/

echo "Move restart files, logs, landmasks, and dump files"
echo "Delete run directories"
for ((irun=1; irun<=${nruns}; irun++)) ; do
    # Restart files (incl. namelists)
    mkdir -p ${outpath}/restart/run${irun}
    mv ${outpath}/run${irun}/restart/* ${outpath}/restart/run${irun}/

    # Climate restart file
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
echo "Finished cleaning ${exp_name}"

exit
