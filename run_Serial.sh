#!/bin/bash

# This script is called by the run_PseudoParallel.sh script, and executes one of the
# serial tasks which form the larger pseudo-parallel experiment.

# Shouldn't have to define any variables here, any shell variables required should be
# exported from the run_PseudoParallel.sh script.

# Run the climate spinup stage

echo $RUN
if [[ ${RUN_CLIMATE_SPINUP} -eq 1 ]] ; then
    if [[ ${CLEAN} -eq 1 ]] ; then
        rm stage_1_climate_spinup -r
    fi
    mkdir stage_1_climate_spinup/ -p
    python3 prepare_stage.py namelists stage_1_climate_spinup/ stage_configurations/climate_spinup_config.yml $RUN
    cd stage_1_climate_spinup/run${RUN}/
    mkdir -p logs
    ./${CABLEExecutable} > logs/cable_out_log.txt
    cd ../..
fi

if [[ ${RUN_ZERO_BIOMASS} -eq 1 ]] ; then
    if [[ ${CLEAN} -eq 1 ]] ; then
        rm stage_2_zero_biomass -r
    fi
    mkdir stage_2_zero_biomass -p
    python3 prepare_stage.py namelists stage_2_zero_biomass stage_configurations/zero_biomass_config.yml $RUN
    cd stage_2_zero_biomass/run${RUN}/
    mkdir -p logs
    ./${CABLEExecutable} > logs/cable_out_log.txt
    cd ../..
fi
