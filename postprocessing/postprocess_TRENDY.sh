#!/usr/bin/env bash

# biocomp
#SBATCH --ignore-pbs
#SBATCH --job-name=cpost
#SBATCH --output=%x-%j.out
#SBATCH --error=%x-%j.out
# #SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
#SBATCH --time=23:00:00
#SBATCH --mem=64G
#SBATCH --mail-type=FAIL,STAGE_OUT,TIME_LIMIT,INVALID_DEPEND,END
#SBATCH --mail-user=matthias.cuntz@inrae.fr

# Script to postprocess TRENDY simulations to give outputs ready for
# upload as specified in the TRENDY protocol
# Juergen Knauer August 2021
#     - July 2022: TRENDY test suite and ILAMB functionalities added
#     - August 2022: casa variables added (using Matthias' Python script)
#     - August 2023: - script adjusted to handle outputs from serial setup
#     - additional output variables added
#     - May 2024, Matthias Cuntz: port to cluster biocomp (Slurm)

# The script loops through all TRENDY experiments and all variables
# and does all steps of postprocessing, including change of names and
# units, unit conversion, spatial and temporal aggregation, etc.

# The first step is to copy the TRENDY results from the simdir to the
# updir. If this step should be repeated (e.g. new runs) these files
# must be deleted first (or the entire folder).

# Assumptions to be met for this script to work:
#     - cable output is at monthly resolution
#     - LUC output is at annual resolution
#     - spatial resolution for all outputs is 1 deg
#     - every grid cell has 3 PFTs (1-2 of them may be missing values)
#     - soil has 6 layers
#     - soil C has 3 pools
#     - 0.7 of wood is belowground and added to roots

# Note: this script currently is set up for gadi and TRENDY runs S0-S3 only

# TODO:
# - do not process patchfrac for every PFT-type variable
#   but skip if it already exists

set -ex

cdo=cdo
tmpdir='.'

# --------------------------------------------------------------------
# Load Modules
# --------------------------------------------------------------------

eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
conda activate pystd
cdo="cdo -f nc4 -z zip -L"
tmpdir='/local'


# --------------------------------------------------------------------
# Settings
# --------------------------------------------------------------------

#
# Paths
modelname="CABLE-POP"
startdir=${PWD}
# Location of TRENDY output
simdir="/home/mcuntz/projects/cable/trendy_v12"
# Upload directory
updir="${simdir}/upload"
# TRENDY grid definition file
gridfile="/home/mcuntz/data/cable/juergen_aux/trendy.grid.1deg"
# file that contains all information regarding output variables
varfile="${startdir}/variables_TRENDY_v12.csv"
# ILAMB root directory
ilamb="/home/mcuntz/data/ilamb"
# ILAMB output directory
idir="${simdir}/ilamb"
# file with information on ILAMB variables
ivarfile="${startdir}/ILAMB/variables_ilamb.csv"
# ILAMB config file
iconfig="${startdir}/ILAMB/ilamb_TRENDY.cfg"

#
# Run type, experiment and variables
# run TRENDY test suite on 1000 grid points (1) or the whole globe (0)?
test_suite=0
# run ILAMB and prepare outputs for it?
run_ilamb=1
# experiments to postprocess
# must end with "_1000pts" if test_suite -eq 1
# exps="S0 S1 S2 S3"
exps="S3"

# All variables to postprocess
vars=$(cut -d, -f 2 ${varfile} | tail -n +2)
# # Part 1 (ILAMB)
# vars="tas mrro evapotrans cVeg cSoil npp npppft gpp gpppft ra rh lai tran nbp"
# # Part 2
# vars="pr rsds mrso evapotranspft transpft evapo albedopft snow_depthpft shflxpft"
# vars+=" rnpft cLitter cProduct cVegpft cSoilpft fLuc rhpft nbppft landCoverFrac"
# vars+=" oceanCoverFrac laipft cLeaf cWood cRoot cCwd tsl msl evspsblveg evspsblsoi"
# vars+=" tskinpft mslpft theightpft"
# # Part 3
# vars="fVegLitter fLeafLitter fWoodLitter fRootLitter fVegSoil fNdep fBNF fNup"
# vars+=" fNnetmin nSoil nOrgSoil nInorgSoil fNloss nVeg nLeaf nWood nRoot nLitter"
# vars+=" cSoilpools fAllocLeaf fAllocWood fAllocRoot"
# # Test
# vars="fAllocRoot"
new_run=0    # 1/0: do/do not delete all previous post-processed variables.
             # Set to 1 when postprocessing new runs and
             # to 0 when postprocessing new variables of an existing run.
             # Warning: new_run=1 deletes all existing outputs in the upload folder!

#
# Model properties
startyear=1700
endyear=2022
layer_depths="0.022 0.058 0.154 0.409 1.085 6.872"  # depths of soil layers (m)
ivegs="1 2 3 4 5 6 7 8 14 17"  # existing ivegs in CABLE
PFT_names="0=Evergreen Needleleaf Forest, 1=Evergreen Broadleaf Forest"
PFT_names+=", 2=Deciduous Needleleaf Forest, 3=Deciduous Broadleaf Forest"
PFT_names+=", 4=Shrub, 5=C3 Grass, 6=C4 Grass, 7=Tundra, 8=Barren, 9=Ice"
SoilCPool_names="0=microbial, 1=slow, 2=passive"

# Output
compress_files=0      # 1/0: do/do not compress output file
strip_history=0       # 1/0: do/do not delete history attributes in netcdf files

# global attributes
creation_date=$(date)
source_code="https://github.com/CABLE-LSM/CABLE/tree/CABLE-POP_TRENDY"
revision="96840b8"
institution="INRAE Grand Est - Nancy; Western Sydney University; CSIRO Environment"
contact="Matthias Cuntz (matthias.cuntz@inrae.fr)"
contact+="; Juergen Knauer (J.Knauer@westernsydney.edu.au)"

# constants
missval=-99999
gtokg=0.001
kgtoPg=1.0e-12
km2tom2=1.0e6
# Note: no leap years in cru forcing (also 'leaps=FALSE' in cable.nml)
secperyear=$(echo "86400 * 365" | bc)

# pool indices
leaf=0
wood=1
froot=2

# Test suite
if [[ ${test_suite} -eq 1 ]] ; then
    startyear=1901
    # specify regions to average over (lon1,lon2,lat1,lat2)
    regions="-180.0,180.0,-60.0,90.0
             -180.0,180.0,-30.0,30.0
             -180.0,180.0,30.0,90.0"
fi

# ILAMB
if [[ $run_ilamb -eq 1 ]] ; then
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

    # all ILAMB variables (ILAMB name)
    ivars=$(cut -d, -f 1 ${ivarfile} | tail -n +2)
    # equivalent name of CABLE variables
    ivars_cable=$(cut -d, -f 2 ${ivarfile} | tail -n +2)

    if [[ ! -d ${ILAMB_ROOT}/DATA ]] ; then
        echo "ILAMB evaluation data need to be downloaded first (ilamb-fetch)"
    fi
fi


# --------------------------------------------------------------------
# Start Script
# --------------------------------------------------------------------

if [[ ${test_suite} -eq 1 ]] ; then
    mkdir -p ${updir}/results
else
    mkdir -p ${updir}/zonal_nbp
fi

# loop over experiments
for exp in ${exps} ; do
    echo "Processing experiment ${exp}"

    # delete existing postprocessed outputs for new runs
    if [[ ${new_run} -eq 1 ]] ; then
        if [[ -d ${updir}/${exp} ]] ; then
            rm -r ${updir}/${exp}
        fi
    fi

    # Experiment directory
    if [[ ${test_suite} -eq 1 ]] ; then
        mkdir -p ${updir}/results
        mkdir -p ${updir}/${exp}/results
        cd ${updir}/${exp}/results
    else
        mkdir -p ${updir}/${exp}
        cd ${updir}/${exp}
    fi

    # process output files and copy to experiment directory within the
    # upload folder: mergetime, set time axis, regrid
    if [[ ! -f cable_output.nc ]] ; then  # only if this hasn't been done before
        ftypes="cable casa"
        # LUC for S3 upwards
        if [[ "${exp}" == *"S3"* ]] ; then
            ftypes="${ftypes} LUC"
        fi

        # merge output files
        for ftype in ${ftypes} ; do
            echo "Merge output files ${ftype}"
            if [[ ${test_suite} -eq 1 ]] ; then
                cp ${simdir}/${exp}/output/cru_out_${ftype}_${startyear}_${endyear}.nc \
                   ${tmpdir}/${ftype}_tmp1.nc
            else
                ${cdo} -O -s mergetime \
                    ${simdir}/${exp}/output/cru_out_${ftype}_${startyear}_1900.nc \
                    ${simdir}/${exp}/output/cru_out_${ftype}_1901_${endyear}.nc \
                    ${tmpdir}/${ftype}_tmp1.nc
            fi

            com=""
            if [[ "${ftype}" == "LUC" ]] ; then
                com="-settunits,years ${com}"
            fi
            # set time axis and calendar
            com="-setcalendar,365_day ${com}"
            # ncview will likely display months incorrectly.
            if [[ "${ftype}" == "LUC" ]] ; then
                com="-settaxis,${startyear}-07-01,12:00:00,1year ${com}"
            else
                com="-settaxis,${startyear}-01-15,12:00:00,1mon ${com}"
            fi
            com="-setrtomiss,9e33,inf ${com}"
            ${cdo} ${com} ${tmpdir}/${ftype}_tmp1.nc ${tmpdir}/${ftype}_tmp.nc

            # regrid output (fill missing values < 60 degS)
            ${cdo} remapnn,${gridfile} ${tmpdir}/${ftype}_tmp.nc ${ftype}_output.nc
            rm ${tmpdir}/${ftype}_tmp1.nc ${tmpdir}/${ftype}_tmp.nc
        done
    fi

    # loop over variables
    for var in ${vars} ; do
        echo "Processing variable ${var}"

        # name of output file
        outfile=${modelname}_${exp}_${var}.nc

        # get variable characteristics from variable file
        # variable name in CABLE
        simvar=$(awk -F',' -v var=${var} '{ if ($2 == var) print $3}' ${varfile})
        # full name as per TRENDY protocol
        longname=$(awk -F',' -v var=${var} '{ if ($2 == var) print $1}' ${varfile})
        # output file where variable is located: cable, casa, or LUC
        ftype=$(awk -F',' -v var=${var} '{ if ($2 == var) print $4}' ${varfile})
        # final unit of variable
        unit=$(awk -F',' -v var=${var} '{ if ($2 == var) print $5}' ${varfile})
        # conversion constant (format is operator:number, where
        # operator is one of add,sub,mul,div)
        scalefac=$(awk -F',' -v var=${var} '{ if ($2 == var) print $7}' ${varfile})
        # if empty: longitude, latitude, time
        extradim=$(awk -F',' -v var=${var} '{ if ($2 == var) print $9}' ${varfile})
        # time step: monthly or annual
        freq=$(awk -F',' -v var=${var} '{ if ($2 == var) print $11}' ${varfile})
        # comments to be added as variable attribute
        varcomment=$(awk -F',' -v var=${var} '{ if ($2 == var) print $13}' ${varfile})

        echo "simvar = ${simvar}"
        echo "longname = ${longname}"

        # Skip variables we don't simulate (a similar check happens
        # further below with ${simvar})
        if [[ "${ftype}" == "NA" ]] ; then
            echo "No model output for variable ${var}. Continuing with next variable."
            continue
        fi

        # extract variable from model output or calculate from more than one variable
        if [[ "${simvar}" == "calculate" ]] ; then
            case ${var} in
                mrro)
                    ${cdo} expr,'mrro=Qs+Qsb;patchfrac;' ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ;;
                mrso|msl)
                    ${cdo} -s selname,SoilMoist ${ftype}_output.nc ${tmpdir}/tmp.nc
                    n=0
                    for layer_depth in ${layer_depths} ; do
                        ncks -O -d soil,${n} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc   # m3 m-3
                        n=$(( ${n} + 1 ))
                        ${cdo} -s -mulc,1000 -mulc,${layer_depth} ${tmpdir}/tmp1.nc \
                               ${tmpdir}/SoilMoist_layer${n}.nc  # l m-2 = kg m-2
                        if [[ ${n} -eq 1 ]] ; then
                            cp ${tmpdir}/SoilMoist_layer${n}.nc ${tmpdir}/previous.nc
                        else
                            ${cdo} -s add ${tmpdir}/SoilMoist_layer${n}.nc \
                                   ${tmpdir}/previous.nc \
                                   ${tmpdir}/total_layer${n}.nc
                            mv ${tmpdir}/total_layer${n}.nc ${tmpdir}/previous.nc
                        fi
                    done
                    mv ${tmpdir}/previous.nc ${tmpdir}/SoilMoist_cumulative.nc
                    rm ${tmpdir}/tmp?.nc

                    if [[ "${var}" == "mrso" ]] ; then
                        ncrename -O -v SoilMoist,${var} ${tmpdir}/SoilMoist_cumulative.nc ${tmpdir}/tmp.nc
                    elif [[ "${var}" == "msl" ]] ; then
                        for ((layer=1;layer<=6;layer++)) ; do
                            # make soil record dimension
                            ncpdq -O -a soil,time ${tmpdir}/SoilMoist_layer${layer}.nc \
                                  ${tmpdir}/SoilMoist_layer${layer}_tmp.nc
                        done
                        ncrcat -O ${tmpdir}/SoilMoist_layer?_tmp.nc ${tmpdir}/tmp1.nc
                        # reset time as record dimension
                        ncpdq -O -a time,soil ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                        # renaming and attributes
                        ncrename -O -v SoilMoist,${var} ${tmpdir}/tmp.nc
                        ncrename -O -d soil,smlayer ${tmpdir}/tmp.nc
                        ncatted -O -a cell_methods,${var},d,c, ${tmpdir}/tmp.nc
                        rm ${tmpdir}/SoilMoist_layer?_tmp.nc
                    fi
                    rm ${tmpdir}/SoilMoist_layer?.nc ${tmpdir}/SoilMoist_cumulative.nc
                    ;;
                cProduct)
                    if [[ "${exp}" == *"S3"* ]] ; then
                        ${cdo} -vertsum -selname,ClearProd,HarvProd ${ftype}_output.nc ${tmpdir}/tmp2.nc
                        ${cdo} expr,'cProduct=ClearProd+HarvProd;' ${tmpdir}/tmp2.nc ${tmpdir}/tmp1.nc
                        ${cdo} mulc,${gtokg} ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc   # gC m-2 -> kgC m-2
                    else
                        echo "${var} not calculated for this experiment!"
                        continue
                    fi
                    ;;
                cRoot)
                    ${cdo} expr,'cRoot=(0.3*PlantCarbWood)+PlantCarbFineRoot;patchfrac;' \
                        ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ;;
                oceanCoverFrac)
                    # land mask # missing data (ocean) to 2 # subtract 1 (land = 0, ocean = 1)
                    ${cdo} -subc,1 -setmisstoc,2 -vertsum -seltimestep,1 -selname,patchfrac \
                           ${ftype}_output.nc ${tmpdir}/tmp1.nc
                    ncrename -O -v patchfrac,${var} ${tmpdir}/tmp1.nc ${tmpdir}/tmp2.nc
                    # remove time information (S3 and higher only)
                    if [[ "${exp}" == *"S3"* ]] ; then
                        ncwa -O -a time ${tmpdir}/tmp2.nc ${tmpdir}/tmp1.nc
                        ncks -O -C -x -v time ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                    else
                        mv ${tmpdir}/tmp2.nc ${tmpdir}/tmp.nc
                    fi
                    ;;
                landCoverFrac)
                    ${cdo} -s selname,patchfrac,iveg ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ;;
                cSoilpools)
                    ${cdo} -s selname,SoilCarbFast,SoilCarbSlow,SoilCarbPassive,patchfrac \
                        ${ftype}_output.nc ${tmpdir}/SoilCPools_tmp.nc
                    ;;
                fAllocRoot)
                    ${cdo} expr,'fAllocRoot=(0.3*CallocWood)+CallocFineRoot;patchfrac;' \
                        ${ftype}_output.nc ${tmpdir}/tmp.nc
                    # TODO: add iveg to casa output
                    ${cdo} selname,iveg cable_output.nc ${tmpdir}/iveg_tmp.nc
                    ${cdo} -O merge ${tmpdir}/tmp.nc ${tmpdir}/iveg_tmp.nc ${tmpdir}/tmp1.nc
                    ${cdo} -O setrtomiss,-inf,-1 ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                    ;;
                fVegLitter)
                    ${cdo} -s -vertsum -selname,FluxCtolitter ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ncrename -O -v FluxCtolitter,${var} ${tmpdir}/tmp.nc
                    ;;
                fLeafLitter)
                    ${cdo} -s -vertsum -selname,FluxCLeaftolitter ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ncrename -O -v FluxCLeaftolitter,${var} ${tmpdir}/tmp.nc
                    ;;
                fWoodLitter)
                    ${cdo} -s expr,'fWoodLitter=0.7*FluxCWoodtolitter;' ${ftype}_output.nc \
                        ${tmpdir}/tmp1.nc
                    ${cdo} -s vertsum ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                    ;;
                fRootLitter)
                    # note FluxCFineRoottolitte seems to exceed
                    # maximum length of variable name and is cropped
                    ${cdo} -s expr,'fRootLitter=0.3*FluxCWoodtolitter+FluxCFineRoottolitter;' \
                        ${ftype}_output.nc ${tmpdir}/tmp1.nc
                    ${cdo} -s vertsum ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                    ;;
                fLitterSoil)
                    echo "not yet implemented. Requires new output from code"
                    continue
                    # casaflux%ctosoil includes both fluxes from
                    # litter to soil as well as between soil
                    # pools. The latter are gross fluxes only!
                    # Solution is to report the fromLtoS (not the fromStoS) fluxes only.
                    # ${cdo} -s selname,FluxCtosoil ${ftype}_output.nc ${tmpdir}/Ctosoil_tmp.nc
                    # ${cdo} -s vertsum ${tmpdir}/Ctosoil_tmp.nc ${tmpdir}/tmp.nc
                    # ncrename -O -v FluxCtosoil,${var} ${tmpdir}/tmp.nc
                    ;;
                fVegSoil)
                    # 0.0 everywhere on land
                    ${cdo} -s -setrtoc2,0,inf,0.0,0.0 -selname,FluxCtolitter ${ftype}_output.nc \
                           ${tmpdir}/tmp.nc
                    ncrename -O -v FluxCtolitter,${var} ${tmpdir}/tmp.nc
                    ;;
                nSoil)
                    # organic soil N
                    ${cdo} -s -vertsum -selname,nsoil ${ftype}_output.nc ${tmpdir}/nsoilorg_tmp.nc
                    # inorganic soil N
                    ${cdo} -s selname,Nsoilmin,patchfrac ${ftype}_output.nc ${tmpdir}/nsoilmin_tmp1.nc
                    ncwa -O -a patch -w patchfrac -v Nsoilmin ${tmpdir}/nsoilmin_tmp1.nc \
                         ${tmpdir}/nsoilmin_tmp.nc
                    ${cdo} -s selname,Nsoilmin ${tmpdir}/nsoilmin_tmp.nc ${tmpdir}/nsoilmin_tmp1.nc
                    # total soil N = organic + inorganic soil N
                    ${cdo} -s add ${tmpdir}/nsoilorg_tmp.nc ${tmpdir}/nsoilmin_tmp1.nc ${tmpdir}/tmp.nc
                    ncrename -O -v nsoil,${var} ${tmpdir}/tmp.nc
                    ;;
                nOrgSoil)
                    ${cdo} -s -vertsum -selname,nsoil ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ncrename -O -v nsoil,${var} ${tmpdir}/tmp.nc
                    ;;
                nVeg)
                    ${cdo} -s -vertsum -selname,nplant ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ncrename -O -v nplant,nVeg ${tmpdir}/tmp.nc
                    ;;
                nLeaf)
                    ${cdo} -s selname,nplant ${ftype}_output.nc ${tmpdir}/nveg_tmp.nc
                    ncks -O -d mplant,${leaf} ${tmpdir}/nveg_tmp.nc ${tmpdir}/tmp.nc
                    ncrename -O -v nplant,nLeaf ${tmpdir}/tmp.nc
                    ;;
                nWood)
                    ${cdo} -s selname,nplant ${ftype}_output.nc ${tmpdir}/nveg_tmp.nc
                    ncks -O -d mplant,${wood} ${tmpdir}/nveg_tmp.nc ${tmpdir}/tmp.nc
                    ncrename -O -v nplant,nWood ${tmpdir}/tmp.nc
                    ;;
                nRoot)
                    ${cdo} -s selname,nplant ${ftype}_output.nc ${tmpdir}/nveg_tmp.nc
                    ncks -O -d mplant,${froot} ${tmpdir}/nveg_tmp.nc ${tmpdir}/nfroot_tmp.nc
                    ncks -O -d mplant,${wood} ${tmpdir}/nveg_tmp.nc ${tmpdir}/nwood_tmp1.nc
                    ${cdo} -s -add ${tmpdir}/nfroot_tmp.nc -mulc,0.3 ${tmpdir}/nwood_tmp1.nc \
                           ${tmpdir}/tmp.nc
                    ncrename -O -v nplant,nRoot ${tmpdir}/tmp.nc
                    ;;
                nLitter)
                    ${cdo} -s -vertsum -selname,nlitter ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ncrename -O -v nlitter,nLitter ${tmpdir}/tmp.nc
                    ;;
                fNloss)
                    ${cdo} -s expr,'fNloss=Nminloss+Nminleach;patchfrac;' \
                        ${ftype}_output.nc ${tmpdir}/tmp.nc
                    ncwa -O -a patch -w patchfrac -v ${var} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                    ncatted -O -a cell_methods,${var},d,c, ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                    ;;
                *)
                    echo "Variable ${var} not yet calculated!"
                    continue
                    ;;
            esac

        elif [[ "${simvar}" == "" ]] ; then
            if [[ "${var}" == "fLUC" ]] ; then
                echo "${var} is calculated after experiment loop."
            else
                echo "${var} not available from model output! Skip Variable..."
            fi
            continue
        else
            # just extract simvar from file, together with patchfrac
            # and iveg (PFT-output only)
            if [[ "${ftype}" == "casa" ]] ; then
                ${cdo} selname,${simvar},patchfrac ${ftype}_output.nc ${tmpdir}/tmp.nc
                if [[ "${extradim}" == "PFT" ]] ; then
                    # TODO: add iveg to casa output
                    ${cdo} selname,iveg cable_output.nc ${tmpdir}/iveg_tmp.nc
                    ${cdo} -O merge ${tmpdir}/tmp.nc ${tmpdir}/iveg_tmp.nc ${tmpdir}/tmp1.nc
                    ${cdo} -O setrtomiss,-inf,-1 ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                fi
            else
                if [[ "${extradim}" == "PFT" && "${var}" != "landCoverFrac" ]] ; then
                    ${cdo} selname,${simvar},patchfrac,iveg ${ftype}_output.nc ${tmpdir}/tmp.nc
                elif [[ "${extradim}" == "stlayer" ]] ; then
                    ${cdo} selname,${simvar} ${ftype}_output.nc ${tmpdir}/tmp.nc
                else
                    ${cdo} selname,${simvar},patchfrac ${ftype}_output.nc ${tmpdir}/tmp.nc
                fi
            fi
        fi

        # rename variable if necessary
        if [[ "${simvar}" != "${var}" && "${simvar}" != "calculate" ]] ; then
            # "calculate" variables are named above
            ncrename -O -v ${simvar},${var} ${tmpdir}/tmp.nc
        fi

        # patch averaging (if necessary)
        if [[ "${extradim}" != "PFT" ]] ; then
            if [[ "${extradim}" == "SoilCPool" ]] ; then
                ncwa -O -a patch -w patchfrac ${tmpdir}/SoilCPools_tmp.nc ${tmpdir}/SoilCPools_tmp1.nc
                # needed so that next ncap2 command works
                ncks -O -5 ${tmpdir}/SoilCPools_tmp1.nc ${tmpdir}/SoilCPools_tmp2.nc
                # add Soil C Pool dimension
                ncap2 -O -s 'defdim("soilCpool",3);cSoilpools[time,soilCpool,latitude,longitude]=-1e+33' \
                      ${tmpdir}/SoilCPools_tmp2.nc ${tmpdir}/SoilCPools_tmp1.nc
                ncap2 -O -s 'cSoilpools(:,0,:,:)=SoilCarbFast;cSoilpools(:,1,:,:)=SoilCarbSlow;cSoilpools(:,2,:,:)=SoilCarbPassive' \
                      ${tmpdir}/SoilCPools_tmp1.nc ${tmpdir}/SoilCPools_tmp2.nc
                ${cdo} -s -setctomiss,-1e+33 -selname,cSoilpools ${tmpdir}/SoilCPools_tmp2.nc \
                       ${tmpdir}/tmp.nc
            elif [[ -z ${extradim} ]] ; then
                if [[ "${var}" != "oceanCoverFrac" && \
                      "${var}" != "cProduct" && \
                      "${var}" != "mrso" && \
                      ("${ftype}" != "casa" || "${simvar}" != "calculate") && \
                      "${extradim}" != "stlayer" && \
                      "${extradim}" != "smlayer" ]] ; then # already averaged
                    ncwa -O -a patch -w patchfrac -v ${var} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                    ncatted -O -a cell_methods,${var},d,c, ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
                fi
            fi
        else  # PFT-level output
            for iveg in ${ivegs} ; do
                for ((patch=0;patch<=2;patch++)) ; do
                    # select individual patch and its iveg, patchfrac and variable
                    ncks -O -d patch,${patch} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                    ${cdo} -s selname,iveg ${tmpdir}/tmp1.nc ${tmpdir}/iveg_tmp.nc
                    ${cdo} -s selname,patchfrac ${tmpdir}/tmp1.nc ${tmpdir}/patchfrac${patch}_tmp.nc
                    if [[ "${var}" != "landCoverFrac" ]] ; then
                        ${cdo} -s selname,${var} ${tmpdir}/tmp1.nc ${tmpdir}/var_patch${patch}_tmp1.nc
                    fi

                    # create iveg mask
                    ${cdo} -s setvrange,${iveg},${iveg} ${tmpdir}/iveg_tmp.nc ${tmpdir}/iveg_tmp2.nc

                    # use current iveg to mask variable and patchfrac
                    ${cdo} -s ifthen ${tmpdir}/iveg_tmp2.nc ${tmpdir}/patchfrac${patch}_tmp.nc \
                        ${tmpdir}/patchfrac${patch}_tmp1.nc
                    if [[ "${var}" != "landCoverFrac" ]] ; then
                        ${cdo} -s ifthen ${tmpdir}/iveg_tmp2.nc ${tmpdir}/var_patch${patch}_tmp1.nc \
                            ${tmpdir}/var_patch${patch}_tmp.nc
                    fi

                    # set missval to 0 for sum to work
                    ${cdo} -s setmisstoc,0 ${tmpdir}/patchfrac${patch}_tmp1.nc \
                        ${tmpdir}/patchfrac${patch}_tmp.nc

                    # sum up patch fraction for that vegtype
                    if [[ ${patch} -eq 0 ]] ; then
                        cp ${tmpdir}/patchfrac${patch}_tmp.nc ${tmpdir}/previous_patchfrac_tmp.nc
                    else
                        ${cdo} -s add ${tmpdir}/patchfrac${patch}_tmp.nc ${tmpdir}/previous_patchfrac_tmp.nc \
                            ${tmpdir}/total_patchfrac_tmp.nc
                        mv ${tmpdir}/total_patchfrac_tmp.nc ${tmpdir}/previous_patchfrac_tmp.nc
                    fi
                done  # end patch loop
                ${cdo} -O -s setctomiss,0 ${tmpdir}/previous_patchfrac_tmp.nc \
                    ${tmpdir}/total_patchfrac${iveg}_tmp.nc

                if [[ "${var}" == "landCoverFrac" ]] ; then
                    if [[ "${exp}" == *"S3"* ]] ; then
                        # make PFT record dimension
                        ncpdq -O -a patch,time ${tmpdir}/total_patchfrac${iveg}_tmp.nc \
                              ${tmpdir}/var_iveg${iveg}_tmp.nc
                        ncrename -O -v patchfrac,landCoverFrac ${tmpdir}/var_iveg${iveg}_tmp.nc
                    else
                        # make PFT record dimension (would be good to
                        # use same command for S0-S2 and S3!)
                        ncks -O --mk_rec_dmn patch ${tmpdir}/total_patchfrac${iveg}_tmp.nc \
                             ${tmpdir}/var_iveg${iveg}_tmp.nc
                        ncrename -O -v patchfrac,landCoverFrac ${tmpdir}/var_iveg${iveg}_tmp.nc
                    fi
                else  # all other variables
                    for ((patch=0;patch<=2;patch++)) ; do
                        ${cdo} -s div ${tmpdir}/patchfrac${patch}_tmp.nc \
                            ${tmpdir}/total_patchfrac${iveg}_tmp.nc ${tmpdir}/patchfrac_weight${patch}_tmp.nc
                        ${cdo} -s mul ${tmpdir}/patchfrac_weight${patch}_tmp.nc \
                            ${tmpdir}/var_patch${patch}_tmp.nc ${tmpdir}/var_patch${patch}_weighted_tmp.nc

                        if [[ ${patch} -eq 0 ]] ; then
                            cp ${tmpdir}/var_patch${patch}_weighted_tmp.nc ${tmpdir}/varsum_previous_tmp.nc
                        else
                            ${cdo} -s add ${tmpdir}/var_patch${patch}_weighted_tmp.nc \
                                ${tmpdir}/varsum_previous_tmp.nc ${tmpdir}/varsum_total_tmp.nc
                            mv ${tmpdir}/varsum_total_tmp.nc ${tmpdir}/varsum_previous_tmp.nc
                        fi
                    done  # end patch loop
                    if [[ "${exp}" == *"S3"* ]] ; then
                        ncrename -O -v patchfrac,${var} ${tmpdir}/varsum_previous_tmp.nc
                    fi
                    # make PFT record dimension
                    ncpdq -O -a patch,time ${tmpdir}/varsum_previous_tmp.nc ${tmpdir}/var_iveg${iveg}_tmp.nc
                fi
            done  # end iveg loop

            # concatenate files along patch dimension
            ncrcat -O ${tmpdir}/var_iveg?_tmp.nc ${tmpdir}/var_iveg??_tmp.nc ${tmpdir}/tmp1.nc

            # reset time as record dimension (if existing)
            if [[ "${var}" == "landCoverFrac" && "${exp}" != *"S3"* ]] ; then
                mv ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
            else
                ncpdq -O -a time,patch ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
            fi

            # rename patch dimension to PFT
            ncrename -O -d patch,PFT ${tmpdir}/tmp.nc

            # make sure variable has correct name
            if [[ "${simvar}" != "${var}" && \
                  "${var}" != "landCoverFrac" && \
                  "${exp}" != *"S3"* ]] ; then
                ncrename -O -v patchfrac,${var} ${tmpdir}/tmp.nc
            fi
            ncatted -O -a cell_methods,${var},d,c, ${tmpdir}/tmp.nc
        fi

        # Unit conversion
        operator=$(echo ${scalefac} | cut -d ":" -f 1)
        convconst=$(echo ${scalefac} | cut -d ":" -f 2)

        case ${operator} in
            add)
                ${cdo} addc,${convconst} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                ;;
            sub)
                ${cdo} subc,${convconst} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                ;;
            mul)
                ${cdo} mulc,${convconst} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                ;;
            div)
                ${cdo} divc,${convconst} ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                ;;
            *)
                echo "no unit conversion"
                if [[ -f ${tmpdir}/tmp.nc ]] ; then
                    mv ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
                fi
                ;;
        esac

        if [[ -f ${tmpdir}/tmp1.nc ]] ; then
            mv ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
        fi

        # subtract harvest fluxes for NBP in S3 and above
        if [[ "${var}" == "nbp" && "${exp}" == *"S3"* ]] ; then
            # select fluxes
            ${cdo} selname,AgProdLoss,ClearProdLoss,HarvProdLoss LUC_output.nc ${tmpdir}/tmp1.nc
            ${cdo} vertsum ${tmpdir}/tmp1.nc ${tmpdir}/tmp2.nc  # sum over product pools

            # g C m-2 yr-1 -> kg C m-2 s-1
            convfac=$(echo "scale=28; ${gtokg} / ${secperyear}" | bc)
            ${cdo} -mulc,${convfac} ${tmpdir}/tmp2.nc ${tmpdir}/tmp1.nc
            ${cdo} expr,'AllProdLoss=AgProdLoss+ClearProdLoss+HarvProdLoss;' ${tmpdir}/tmp1.nc \
                ProdLossTotal.nc

            for ((year=${startyear};year<=${endyear};year++)) ; do
                # same value every month
                ${cdo} -s selyear,${year} ProdLossTotal.nc ${tmpdir}/ProdLossTotalYear.nc
                ${cdo} -s selyear,${year} ${tmpdir}/tmp.nc ${tmpdir}/NBPyear.nc
                ${cdo} -s sub ${tmpdir}/NBPyear.nc ${tmpdir}/ProdLossTotalYear.nc ${tmpdir}/NBP_${year}.nc
            done
            ${cdo} -O mergetime ${tmpdir}/NBP_????.nc ${tmpdir}/tmp.nc
            rm ${tmpdir}/NBPyear.nc ${tmpdir}/NBP_????.nc ${tmpdir}/ProdLossTotalYear.nc

        elif [[ "${var}" == "nbppft" && "${exp}" == *"S3"* ]] ; then
            echo "NBP at PFT-level not defined in current CABLE version. Skipping ${var}"
            continue
        fi

        # temporal averaging from monthly to annual (always do the
        # averaging to keep it simple)
        if [[ "${ftype}" == "cable" || "${ftype}" == "casa" ]] ; then
            if [[ ("${var}" == "landCoverFrac" && "${exp}" != *"S3"*) || \
                  ("${var}" == "oceanCoverFrac") ]] ; then
                echo "no time aggregation for variable ${var} and experiment ${exp}"
                mv ${tmpdir}/tmp.nc ${tmpdir}/tmp_annual.nc
            else
                mv ${tmpdir}/tmp.nc ${tmpdir}/tmp_monthly.nc  # copy monthly output for later use
                ${cdo} yearmonmean ${tmpdir}/tmp_monthly.nc ${tmpdir}/tmp1.nc
                # delete redundant 'time_bnds' variable
                ncatted -O -a bounds,time,d,c, ${tmpdir}/tmp1.nc ${tmpdir}/tmp2.nc
                ncks -O -C -x -v time_bnds ${tmpdir}/tmp2.nc ${tmpdir}/tmp1.nc
                # correct time information
                ${cdo} settunits,years ${tmpdir}/tmp1.nc ${tmpdir}/tmp2.nc
                ${cdo} settaxis,${startyear}-06-15,12:00:00,1year ${tmpdir}/tmp2.nc ${tmpdir}/tmp_annual.nc
            fi
        elif [[ "${ftype}" == "LUC" ]] ; then
            mv ${tmpdir}/tmp.nc ${tmpdir}/tmp_annual.nc
        fi

        # set missing value and set/modify attributes
        if [[ -f ${tmpdir}/tmp_monthly.nc ]] ; then
            ncatted -O -a _FillValue,${var},o,f,${missval} ${tmpdir}/tmp_monthly.nc
            ncatted -O -a missing_value,${var},o,f,${missval} ${tmpdir}/tmp_monthly.nc
            ncatted -O -a long_name,${var},o,c,"${longname}" ${tmpdir}/tmp_monthly.nc
            ncatted -O -a units,${var},o,c,"${unit}" ${tmpdir}/tmp_monthly.nc
        fi
        ncatted -O -a _FillValue,${var},o,f,${missval} ${tmpdir}/tmp_annual.nc
        ncatted -O -a missing_value,${var},o,f,${missval} ${tmpdir}/tmp_annual.nc
        ncatted -O -a long_name,${var},o,c,"${longname}" ${tmpdir}/tmp_annual.nc
        ncatted -O -a units,${var},o,c,"${unit}" ${tmpdir}/tmp_annual.nc

        # Prepare standard TRENDY output files
        if [[ ${test_suite} -eq 0 ]] ; then
            # make sure output time step is correct
            if [[ "${freq}" == "monthly" ]] ; then
                cp ${tmpdir}/tmp_monthly.nc ${tmpdir}/tmp.nc
            elif [[ "${freq}" == "annual" || "${freq}" == "once" ]] ; then
                cp ${tmpdir}/tmp_annual.nc ${tmpdir}/tmp.nc
            else
                echo "Frequency must be 'monthly', 'yearly', or 'once'!"
            fi

            # set/modify attributes
            if [[ "${var}" == "mrso" || \
                  "${var}" == "msl" || \
                  "${var}" == "mslpft" || \
                  "${var}" == "tsl" ]] ; then
                ncatted -O -a layer_depths_m,${var},c,c,"${layer_depths}" ${tmpdir}/tmp.nc
                if [[ "${var}" == "tsl" ]] ; then
                    ncrename -O -d soil,stlayer ${tmpdir}/tmp.nc
                fi
            fi
            if [[ "${varcomment}" != "" ]] ; then
                ncatted -O -a comments,${var},c,c,"${varcomment}" ${tmpdir}/tmp.nc
            fi
            if [[ "${extradim}" == "PFT" ]] ; then
                ncatted -O -a PFT_key,${var},c,c,"${PFT_names}" ${tmpdir}/tmp.nc
            fi
            if [[ "${extradim}" == "SoilCPool" ]] ; then
                ncatted -O -a SoilCPool_key,${var},c,c,"${SoilCPool_names}" ${tmpdir}/tmp.nc
            fi

            # global attributes
            ncatted -O -a source_code,global,c,c,"${source_code}" ${tmpdir}/tmp.nc
            ncatted -O -a code_revision,global,c,c,"${revision}" ${tmpdir}/tmp.nc
            ncatted -O -a institution,global,c,c,"${institution}" ${tmpdir}/tmp.nc
            ncatted -O -a contact,global,c,c,"${contact}" ${tmpdir}/tmp.nc

            # cleanup: delete redundant attributes
            ncatted -O -a Output_averaging,global,d,c, ${tmpdir}/tmp.nc  # confusing --> delete
            if [[ "${ftype}" == "LUC" ]] ; then
                ncatted -O -a StartYear,global,d,c, ${tmpdir}/tmp.nc
                ncatted -O -a EndYear,global,d,c, ${tmpdir}/tmp.nc
            fi
            if [[ "${ftype}" == "casa" ]] ; then
                ncatted -O -a icycle,global,d,c, ${tmpdir}/tmp.nc
                ncatted -O -a startyear,global,d,c, ${tmpdir}/tmp.nc
                ncatted -O -a endyear,global,d,c, ${tmpdir}/tmp.nc
                ncatted -O -a runiden,global,d,c, ${tmpdir}/tmp.nc
                ncatted -O -a run-type,global,d,c, ${tmpdir}/tmp.nc
            fi
            if [[ ${strip_history} -eq 1 ]] ; then
                ncatted -h -a history,global,d,c, ${tmpdir}/tmp.nc
                ncatted -h -a history_of_appended_files,global,d,c, ${tmpdir}/tmp.nc
            fi

            # name file
            mv ${tmpdir}/tmp.nc ${outfile}

            # compress file and give permissions
            if [[ ${compress_files} -eq 1 ]] ; then
                if [[ ${test_suite} -eq 0 && "${var}" == "nbp" ]] ; then
                    echo "NBP: file is compressed later"
                else
                    gzip -f ${outfile}
                    chmod 775 ${outfile}.gz
                fi
            else
                chmod 775 $outfile
            fi

        else  # ${test_suite} -eq 1
            # Specific for test suite: calculate mean values across latitudinal bands
            for region in ${regions} ; do
                lon1=$(echo $region | cut -d "," -f 1)
                lon2=$(echo $region | cut -d "," -f 2)
                lat1=$(echo $region | cut -d "," -f 3)
                lat2=$(echo $region | cut -d "," -f 4)

                # annual output
                ${cdo} sellonlatbox,${lon1},${lon2},${lat1},${lat2} ${tmpdir}/tmp_annual.nc ${tmpdir}/tmp_band.nc
                ${cdo} fldmean ${tmpdir}/tmp_band.nc ${var}_${lon1}_${lon2}_${lat1}_${lat2}_annual.nc

                # monthly output
                ${cdo} sellonlatbox,${lon1},${lon2},${lat1},${lat2} ${tmpdir}/tmp_monthly.nc \
                    ${tmpdir}/tmp_band.nc
                ${cdo} fldmean ${tmpdir}/tmp_band.nc ${var}_${lon1}_${lon2}_${lat1}_${lat2}_monthly.nc
            done # end region loop
        fi

        # copy monthly files for use in ilamb
        if [[ ${run_ilamb} -eq 1 ]] ; then
            if [[ "${ivars_cable}" == *"${var}"* || \
                  "${var}" == "rh" || \
                  "${var}" == "ra" ]] ; then
                cp ${tmpdir}/tmp_monthly.nc ${tmpdir}/${var}_tmp_global.nc
            fi
        fi

        if [[ -f ${tmpdir}/tmp_monthly.nc ]] ; then rm ${tmpdir}/tmp_monthly.nc ; fi
        if [[ -f ${tmpdir}/tmp_annual.nc ]] ; then rm ${tmpdir}/tmp_annual.nc ; fi

    done # end variable loop

    # Run ILAMB if run_ilamb = True
    if [[ ${run_ilamb} -eq 1 ]] ; then
        # 1) process data in preparation for ILAMB
        for ivar in ${ivars} ; do
            # variable name in CABLE
            var=$(awk -F',' -v ivar=${ivar} '{ if ($1 == ivar) print $2}' ${ivarfile})
            # startyear of ILAMB data (not used)
            # istartyear=$(awk -F',' -v ivar=${ivar} '{ if ($1 == ivar) print $3}' ${ivarfile})
            # endyear of ILAMB data (not used)
            # iendyear=$(awk -F',' -v ivar=${ivar} '{ if ($1 == ivar) print $4}' ${ivarfile})
            # unit conversion factor
            unit_conversion=$(awk -F',' -v ivar=${ivar} '{ if ($1 == ivar) print $5}' ${ivarfile})

            if [[ "${var}" == "derive" ]] ; then
                case ${ivar} in
                    nee)
                        ${cdo} add ${tmpdir}/ra_tmp_global.nc ${tmpdir}/rh_tmp_global.nc ${tmpdir}/reco_tmp.nc
                        ${cdo} sub ${tmpdir}/reco_tmp.nc ${tmpdir}/gpp_tmp_global.nc ${tmpdir}/tmp.nc
                        ncrename -O -v ra,${ivar} ${tmpdir}/tmp.nc
                        ;;
                    reco)
                        ${cdo} add ${tmpdir}/ra_tmp_global.nc ${tmpdir}/rh_tmp_global.nc ${tmpdir}/tmp.nc
                        ncrename -O -v ra,${ivar} ${tmpdir}/tmp.nc
                        ;;
                    *)
                        echo "Nothing implemented here!"
                        ;;
                esac
            else
                cp ${tmpdir}/${var}_tmp_global.nc ${tmpdir}/tmp.nc
            fi

            # select years and change time units
            # ${cdo} selyear,${istartyear}/${iendyear} tmp.nc tmp1.nc
            ${cdo} settunits,days ${tmpdir}/tmp.nc ${tmpdir}/tmp1.nc
            if [[ ${var} == "lai" ]] ; then
                ncatted -O -a units,lai,o,c,"1" ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
            else
                mv ${tmpdir}/tmp1.nc ${tmpdir}/tmp.nc
            fi

            # convert unit if necessary
            if [[ "${unit_conversion}" != "" ]] ; then
                echo "Unit needs conversion but not yet implemented...check!"
            fi

            # rename variable
            if [[ "${var}" != "${ivar}" && "${var}" != "derive" ]] ; then
                ncrename -O -v ${var},${ivar} ${tmpdir}/tmp.nc \
                         ${idir}/MODELS/${exp}/CABLE-POP_${exp}_${ivar}.nc
            else
                mv ${tmpdir}/tmp.nc ${idir}/MODELS/${exp}/CABLE-POP_${exp}_${ivar}.nc
            fi

        done # end ivar loop

        # 2) Run ILAMB (configuration at the beginning of the script)
        ilamb-run --config ${iconfig} \
                  --model_root ${idir}/MODELS/ \
                  --build_dir ${idir}/RESULTS/${exp} \
                  --models ${exp} \
                  --regions global
    fi

    # cleanup
    rm ${tmpdir}/*tmp*.nc


    # --------------------------------------------------------------------
    # create Ascii files with annual NBP output for different latitude bands
    # --------------------------------------------------------------------
    if [[ ${test_suite} -eq 0 ]] ; then
        if [[ "${vars}" == *"nbp"* ]] ; then  # if nbp is in the variable list...

            # 2.1) get area in km^2 (identical across experiments)
            ${cdo} selname,Area cable_output.nc ${tmpdir}/area_tmp1.nc
            ${cdo} vertsum ${tmpdir}/area_tmp1.nc ${tmpdir}/area_tmp2.nc
            ${cdo} mulc,${km2tom2} ${tmpdir}/area_tmp2.nc area.nc   # area in m^2

            # 2.2) calculate annual NBP (kgC m-2 s-1)
            ${cdo} yearmonmean ${modelname}_${exp}_nbp.nc ${tmpdir}/global_nbp_annual_tmp.nc

            if [[ ${compress_files} -eq 1 ]] ; then  # compress file
                gzip -f ${modelname}_${exp}_nbp.nc
                chmod 775 ${modelname}_${exp}_nbp.nc.gz
            fi

            # 2.3) calculate absolute NBP per grid cell (kgC s-1 gridcell-1)
            ${cdo} mul ${tmpdir}/global_nbp_annual_tmp.nc area.nc ${tmpdir}/global_nbp_tmp.nc

            # 2.4) convert NBP to PgC yr-1 gridcell-1
            ${cdo} mulc,${secperyear} ${tmpdir}/global_nbp_tmp.nc ${tmpdir}/global_nbp_tmp1.nc
            ${cdo} mulc,${kgtoPg} ${tmpdir}/global_nbp_tmp1.nc ${tmpdir}/nbp_global.nc

            # 2.5) extract latitudinal bands
            # Use: sellonlatbox,lon1,lon2,lat1,lat2
            ${cdo} sellonlatbox,-180.0,180.0,30.0,90.0    ${tmpdir}/nbp_global.nc  ${tmpdir}/nbp_north.nc
            ${cdo} sellonlatbox,-180.0,180.0,-30.0,30.0   ${tmpdir}/nbp_global.nc  ${tmpdir}/nbp_tropics.nc
            ${cdo} sellonlatbox,-180.0,180.0,-90.0,-30.0  ${tmpdir}/nbp_global.nc  ${tmpdir}/nbp_south.nc

            # 2.6) calculate annual values and export to Ascii file (in R)
            # command line arguments: 1) model name, 2) experiment, 3)
            # startyear, 4) endyear, 5) inpath, 6) outpath
            ${startdir}/create_zonal_nbp_tables.r ${modelname} ${exp} ${startyear} \
                       ${endyear} ${tmpdir} ${updir}/zonal_nbp

            # 2.7) cleanup
            rm ${tmpdir}/*tmp*.nc
            rm ${tmpdir}/nbp_north.nc ${tmpdir}/nbp_south.nc ${tmpdir}/nbp_tropics.nc ${tmpdir}/nbp_global.nc
        fi
    else   # ${test_suite} -eq 1
        # call R script that summarises all the results in one table
        ${startdir}/create_summary_tables.r ${exp} "${vars}" ${startyear} ${endyear} \
                   ${PWD} ${updir}/results
    fi   # check $test_suite

    # TODO: check if another cleanup is necessary here

done # end experiment loop


# --------------------------------------------------------------------
# Calculations across experiments
# --------------------------------------------------------------------

# TODO: check if this works for test_suite -eq 1 (maybe define S2_nbp
# first depending on $test_suite)
cd ${updir}

# fLUC defined as NBP_S2 - NBP_S3 (note that this gives the flux from
# land to atmosphere due to LUC as asked for in the script)
if [[ ("${exp}" == *"S3"*) && \
      (-f S2/${modelname}_S2_nbp.nc || -f S2/${modelname}_S2_nbp.nc.gz) && \
      ("${vars}" == *"fLUC"*) ]] ; then

    if [[ ${compress_files} -eq 1 ]] ; then
        gunzip S2/${modelname}_S2_nbp.nc.gz
        gunzip S3/${modelname}_S3_nbp.nc.gz
        ${cdo} sub S2/${modelname}_S2_nbp.nc S3/${modelname}_S3_nbp.nc S3/tmp.nc
    fi

    ncrename -O -v nbp,fLUC S3/tmp.nc
    ncatted -O -a long_name,fLUC,o,c,"CO2 Flux to Atmosphere from Land Use Change" \
            S3/tmp.nc S3/${modelname}_S3_fLUC.nc
    # ncatted -O -a units,fLUC,o,c,"kg m-2 s-1" S3/tmp.nc S3/${modelname}_S3_fLUC.nc
    if [[ ${compress_files} -eq 1 ]] ; then
        gzip -f S3/${modelname}_S3_fLUC.nc
        gzip -f S2/${modelname}_S2_nbp.nc
        gzip -f S3/${modelname}_S3_nbp.nc
        chmod 775 S3/${modelname}_S3_fLUC.nc.gz
    else
        chmod 775 S3/${modelname}_S3_fLUC.nc
    fi

    rm S3/tmp.nc
fi

# End of Script!
