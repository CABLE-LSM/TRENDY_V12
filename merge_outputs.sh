#!/usr/bin/env bash

# Gadi
# https://opus.nci.org.au/display/Help/How+to+submit+a+job
#PBS -N CABLE_merge
#PBS -P rp23
#PBS -q express
#PBS -l walltime=09:30:00
#PBS -l mem=16GB
#PBS -l ncpus=1
#PBS -l storage=gdata/rp23+scratch/rp23
#PBS -l software=netCDF:MPI:Intel:GNU:scorep
#PBS -r y
#PBS -l wd
#PBS -j oe
#PBS -S /bin/bash
#PBS -v PYTHONPATH

# This line is replaced with the appropriate target during run
python3 
