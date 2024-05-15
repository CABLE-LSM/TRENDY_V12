#!/bin/bash

# First set the default options
NRUNS=100
START_STAGE=0
END_STAGE=6
RUN_FLAGS=(0 0 0 0 0 0 0)
RUN_CLIMATE_SPINUP=1
RUN_ZERO_BIOMASS=1
CLEAN=0

# Get the command line options passed to overwrite any defaults
while getopts "n:s:e:c" option
do
    case ${option} in
        n) NRUNS=${OPTARG};;
        s) START_STAGE=${OPTARG};;
        e) END_STAGE=${OPTARG};;
        c) CLEAN=1;;
    esac
done

for ((STAGE=$START_STAGE; STAGE<=$END_STAGE; STAGE++))
do
    RUN_FLAGS[$STAGE]=1
done

# Now bind the flags in the list to their respective stage flags
SPLIT_LANDMASK=${RUN_FLAGS[0]}
RUN_CLIMATE_SPINUP=${RUN_FLAGS[1]}
RUN_ZERO_BIOMASS=${RUN_FLAGS[2]}

LandmaskFile=${HOME}/Work/ANU/rp23/data/no_provenance/landmask/glob_ipsl_1x1.nc

# Prepare the split landmask
if [[ ${SPLIT_LANDMASK} -eq 1 ]] ; then
    if [[ ${CLEAN} -eq 1 ]] ; then
        rm landmasks -r
    fi
    python3 split_landmask.py $LandmaskFile $NRUNS landmasks -e "global"
fi

# Run the climate spinup stage
if [[ ${RUN_CLIMATE_SPINUP} -eq 1 ]] ; then
    if [[ ${CLEAN} -eq 1 ]] ; then
        rm stage_1_climate_spinup -r
    fi
    mkdir stage_1_climate_spinup -p
    python3 prepare_stage.py namelists stage_1_climate_spinup/ stage_configurations/climate_spinup_config.yml $NRUNS
fi

if [[ ${RUN_ZERO_BIOMASS} -eq 1 ]] ; then
    if [[ ${CLEAN} -eq 1 ]] ; then
        rm stage_2_zero_biomass -r
    fi
    mkdir stage_2_zero_biomass -p
    python3 prepare_stage.py namelists stage_2_zero_biomass stage_configurations/zero_biomass_config.yml $NRUNS
fi
