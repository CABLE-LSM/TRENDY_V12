#!/usr/bin/env bash

# biocomp
#SBATCH --ignore-pbs
#SBATCH --job-name=crunv12
#SBATCH --output=%x-%j.out
#SBATCH --error=%x-%j.out
# #SBATCH --partition=short
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=1
# # ca. 20 min for a single site
# # SBATCH --time=00:30:00
# ca. 25 hrs with 31 jobs on 1x1 grid
#SBATCH --time=29:00:00
#SBATCH --mem=4G
#SBATCH --mail-type=FAIL,STAGE_OUT,TIME_LIMIT,INVALID_DEPEND,END
#SBATCH --mail-user=matthias.cuntz@inrae.fr

# Gadi
# https://opus.nci.org.au/display/Help/How+to+submit+a+job
#PBS -N S0
#PBS -P rp23
#PBS -q normal
#PBS -p 600
#PBS -l walltime=10:30:00
#PBS -l mem=4GB
#PBS -l ncpus=1
#PBS -l storage=gdata/rp23+scratch/rp23+scratch/hh5
#PBS -l software=netCDF:MPI:Intel:GNU
#PBS -r y
#PBS -l wd
#PBS -j oe
#PBS -S /bin/bash

# --------------------------------------------------------------------
#
# Full Cable run with biomass spinup, POP, land-use change, etc.
#
# The run sequence is as follows:
#   1. Create a climate restart file using Cable's default vegetation distribution.
#   2. First phase of spinup with static land use, fixed atmospheric CO2 and
#      N deposition from 1700, and 30 years of repeated meteorology.
#      Zero initial biomass stocks, POP and climate.
#   3. Bring biomass stocks into equilibrium without restricting labile P and mineral N pools.
#      Repeat a and b several times.
#      a) Start from restart files.
#      b) Use dump files for the biophysics. Quasi-equilibrium of soil and litter pools
#         using an analytic steady-state solution.
#   4. Same as 3 but with limited labile P and mineral N pools.
#      Repeat a and b several times.
#   5. Final spinup for 200 years (necessary because step 4 does not give equilibrium in all cases)
#   6. Second phase of spinup with dynamic land use, atmospheric CO2 and N deposition.
#      a) Dynamic land use from 1580 to 1699, using still fixed atmospheric CO2 and
#         N deposition from 1700, and 30 years of repeated meteorology.
#      b) Run from 1700 to 1899 with dynmic land use, varying atmospheric CO2 and N deposition,
#         but still with 30 years of repeated meteorology.
#   7. Final historical run, everything dynamic from 1900 to 2020.
#
# Written,  Matthias Cuntz, Aug 2019, following the run scripts and namelists provided by Vanessa Haverd
# Modified, Jurgen Knauer, 2020      - gm_explicit, coordination, acclimation
#                                    - bios, plume, future runs
#           Matthias Cuntz, Mar 2021 - functions into run_cable-pop_lib.sh
#           Jurgen Knauer, Mar 2023  - modified to run CABLE in serial mode using pseudo-parallelisation
#
# --------------------------------------------------------------------


# --------------------------------------------------------------------
# Load Modules
# --------------------------------------------------------------------
if [[ -e ${HOME}/miniconda3/bin/conda ]] ; then  # biocomp
    eval "$(${HOME}/miniconda3/bin/conda 'shell.bash' 'hook' 2> /dev/null)"
    conda activate pystd
else  # gadi
    pdir=${isdir}
    . /etc/bashrc
    module purge
    module load intel-compiler/2021.5.0
    module load intel-mpi/2021.5.1
    module load netcdf/4.8.0
    export mpiexecdir=/apps/intel-mpi/2021.5.1/bin
    if [[ ! -z ${mpiexecdir} ]] ; then export mpiexecdir="${mpiexecdir}/" ; fi
fi


# --------------------------------------------------------------------
# Basic settings (parsed through from wrapper script)
# --------------------------------------------------------------------
isite=1  # 0/1: 0: use global forcing data with landmask; 1: use site data
# TRENDY experiment (S0, S1, S2, S3, S4, S5, S6)
experiment="S3"
# Name of the experiment (= name of output folder)
experiment_name='FR-Pue'
# Code directory
cablecode="${HOME}/prog/github/cable/cable.cable-pop_trendy"
if [[ ${isite} -eq 0 ]] ; then  # landmask
    # Script directory
    rundir="${HOME}/projects/cable/sites/${experiment_name}"
    # Data directory
    datadir="${HOME}/projects/coco2/cable-pop"
else  # site data
    # Script directory
    # rundir="${HOME}/projects/coco2/cable-pop/output/${experiment_name}"
    rundir="${HOME}/projects/cable/sites/${experiment_name}"
    # Data directory
    datadir="${HOME}/projects/coco2/cable-pop"
fi
# Global data dir
globaldatadir="/home/mcuntz/data"
# Cable executable
exe="${cablecode}/bin/cable"
# Global Surface file
SurfaceFile="${globaldatadir}/cable/CABLE-AUX/offline/gridinfo_CSIRO_1x1.nc"

# Using Global Meteorology
# Output directory of the run
runpath=${rundir}
if [[ ${isite} -eq 0 ]] ; then  # landmask
    # Global Meteorology
    MetPath="${globaldatadir}/met_forcing/CRUJRA2023/daily_1deg_met"
else  # site data
    # # Output directory of the run
    # runpath=.
    # Site Meteorology
    MetPath="${datadir}/input"
    # local files
    CO2NdepFile="${MetPath}/AmaFACE_co2npdepforcing_1850_2100_AMB_JK.csv"
    MetFile="${MetPath}/${experiment_name}_meteo.nc"
    sitelist="${datadir}/misc/coco2_sites.txt"
fi
# Land Mask used for this run
# Use latlon_landmask.py to extract latitudes/longitudes
# if not launched by run_trendy.sh, e.g.
#   python latlon_landmask.py glob_ipsl_1x1.nc 48.6742166667,7.06461666667 landmask_FR-Hes.nc
LandMaskFile="${runpath}/landmask_${experiment_name%%\.*}.nc"
# Global LUC
TransitionFilePath="${globaldatadir}/cable/LUH2/GCB_2023/1deg/EXTRACT"

# # Using Local Meteorology
# # Output directory of the run
# runpath="${HOME}/projects/cable/sites/${experiment_name}"
# # Land Mask used for this run
# LandMaskFile="${runpath}/mask/landmask_${experiment_name%%\.*}.nc"
# # Local Meteorology
# MetPath="${HOME}/projects/cable/sites/${experiment_name}/met"
# # Local LUC
# TransitionFilePath="${HOME}/projects/cable/sites/${experiment_name}/luh"


# --------------------------------------------------------------------
# Run Sequence
# --------------------------------------------------------------------
doclimate=1     # 1/0: Do/Do not create climate restart file
dofromzero=1    # 1/0  Do/Do not first spinup phase from zero biomass stocks
doequi1=1       # 1/0: Do/Do not bring biomass stocks into quasi-equilibrium with unrestricted P and N pools
    nequi1=3        #      number of times to repeat steps in doequi1  4
doequi2=1       # 1/0: Do/Do not bring biomass stocks into quasi-equilibrium with restricted P and N pools
    nequi2=15       #      number of times to repeat steps in doequi2  14
    nequi2a=5       #      number of times to repeat analytic spinup in this step 5
if [[ "${experiment}" == "S3" ]] ; then
    doiniluc=1      # 1/0: Do/Do not spinup with dynamic land use (initialise land use)
else
    doiniluc=0
fi
doinidyn=1      # 1/0: Do/Do not full dynamic spinup (transient run) from 1701 to 1900
dofinal=1       # 1/0: Do/Do not final run from 1901 to 2022

purge_restart=0  # Delete all restart files?


# --------------------------------------------------------------------
# CABLE Settings
# --------------------------------------------------------------------
# MetType
if [[ ${isite} -eq 0 ]] ; then  # landmask
    mettype="cru"   # "cru"
else  # site data
    mettype="site"  # "site"
fi
# Cable
read_fdiff=1        # 1/0: do/do not read in diffuse radiation fraction
call_blaze=0        # 1/0: do/do not call BLAZE
explicit_gm=0       # 1/0: explicit (finite) or implicit mesophyll conductance
Rubisco_params="Bernacchi_2002"   # "Bernacchi_2002" or "Walker_2013"
coordinate_photosyn=1 # 1/0: Do/Do not coordinate photosynthesis
coord=F               # T/F: version of photosyn. optimisation (optimised(F) or forced (T))
acclimate_photosyn=1  # 1/0: Do/Do not acclimate photosynthesis
call_pop=1          # 1/0: Do/Do not use POP population dynamics model, coupled to CASA
doc13o2=0           # 1/0: Do/Do not calculate 13C
c13o2_simple_disc=0 # 1/0: simple or full 13C leaf discrimination
# Parameter files
namelistpath="${rundir}/namelists"
filename_veg="${cablecode}/params/v12/def_veg_params.txt"
filename_soil="${cablecode}/params/v12/def_soil_params.txt"
casafile_cnpbiome="${cablecode}/params/v12/pftlookup.csv"
# Climate restart file
ClimateFile="${runpath}/${mettype}_climate_rst.nc"
# gm lookup tables
gm_lut_bernacchi_2002="${cablecode}/params/gm_LUT_351x3601x7_1pt8245_Bernacchi2002.nc"
gm_lut_walker_2013="${cablecode}/params/gm_LUT_351x3601x7_1pt8245_Walker2013.nc"
# 13C
filename_d13c_atm="${cablecode}/params/gm_LUT_351x3601x7_1pt8245_Bernacchi2002.nc"

if [[ ${isite} -eq 1 ]] ; then  # site data
    # no LUC in site level runs
    doiniluc=0
    # input years
    start_year=$(grep "${experiment_name}" ${sitelist} | cut -f 2)
    end_year=$(grep "${experiment_name}" ${sitelist} | cut -f 3)
    nyears=$(( ${end_year} - ${start_year} + 1 ))
    # transient years
    pre_indust=1850
    nloop_transient=$(( (${start_year} - 1 - ${pre_indust}) / ${nyears} ))
    start_year_transient=$(( ${start_year} - 1 - ${nloop_transient} * ${nyears} + 1 ))
    end_year_transient=$(( ${start_year_transient} + ${nloop_transient} * ${nyears} - 1 ))
    # spinup years
    nyear_spinup=30
    nloop_spin=$(( ${nloop_transient} / ${nyear_spinup} + 1 ))
    end_year_spin=$(( ${start_year_transient} - 1 ))
    start_year_spin=$(( ${end_year_spin} - ${nloop_spin} * ${nyears} + 1 ))
fi

# --------------------------------------------------------------------
# Setup and Functions
# --------------------------------------------------------------------

set -e

trap cleanup 1 2 3 6

pid=$$
isdir="${PWD}"
prog=$0
pprog=$(basename ${prog})
pdir=$(dirname ${prog})
tmp=${TMPDIR:-"/tmp"}

# Helper functions, most functions are in run_cable-pop_lib.sh
source ${isdir}/run_cable-pop_lib.sh

# usage of script
function usage()
{
    printf "${pprog} [-h]\n"
    printf "Runs Cable on a single grid cell with spinup, POP, land-use change, etc.\n"
    printf "Behaviour of the script is controlled by switches at the top of the script (ca. line 101ff).\n"
    printf "\n"
    printf "Options\n"
    printf "    -h    Prints this help screen.\n"
}

# cleanup at end or at trap
function cleanup()
{
    \rm -f ${tmp}/*.${pid}*
    exit 1
}


# --------------------------------------------------------------------
# Preparation
# --------------------------------------------------------------------
# Get options
while getopts "h" option ; do
    case ${option} in
        h) usage; exit;;
        *) printf "Error ${pprog}: unimplemented option.\n\n" 1>&2;  usage 1>&2; exit 1;;
    esac
done
shift $((${OPTIND} - 1))

# get directories
pdir=$(abspath ${pdir})
cd ${pdir}
mkdir -p ${runpath}
rdir=$(abspath ${runpath})
ndir=$(abspath ${namelistpath})

# prepare run directory
cd ${rdir}
mkdir -p logs
mkdir -p outputs
mkdir -p restart
cp ${exe} ./
iexe=$(basename ${exe})
cd ${pdir}

# OS
# Linux: "sed --in-place=.old" ; macOS/Unix: "sed -i .old"
if [[ $(uname -s) == Darwin ]] ; then  # macOS
    ised="sed -i .old"
else  # Linux
    ised="sed --in-place=.old"
fi


# --------------------------------------------------------------------
# Print Info
# --------------------------------------------------------------------
t1=$(date +%s)
printf "Started at %s\n" "$(date)"

printf "\nSetup\n"
printf "    Sequence\n"
printf "        experiment=${experiment}\n"
printf "        doclimate=${doclimate}\n"
printf "        dofromzero=${dofromzero}\n"
printf "        doequi1=${doequi1}\n"
printf "            nequi1=${nequi1}\n"
printf "        doequi2=${doequi2}\n"
printf "            nequi2=${nequi2}\n"
printf "        doiniluc=${doiniluc}\n"
printf "        doinidyn=${doinidyn}\n"
printf "        dofinal=${dofinal}\n"
printf "\n"
printf "    Options\n"
printf "        mettype=${mettype}\n"
printf "        explicit_gm=${explicit_gm}\n"
printf "        Rubisco_params=${Rubisco_params}\n"
printf "        coordinate_photosyn=${coordinate_photosyn}\n"
printf "        coord=${coord}\n"
printf "        acclimate_photosyn=${acclimate_photosyn}\n"
printf "        call_pop=${call_pop}\n"
printf "        doc13o2=${doc13o2}\n"
printf "        c13o2_simple_disc=${c13o2_simple_disc}\n"
printf "\n"
printf "    Directories\n"
printf "        rundir=${rundir}\n"
printf "        runpath=${runpath}\n"
printf "        exe=${exe}\n"
printf "        LandMaskFile=${LandMaskFile}\n"
printf "        SurfaceFile=${SurfaceFile}\n"
printf "        namelistpath=${namelistpath}\n"
printf "        filename_veg=${filename_veg}\n"
printf "        filename_soil=${filename_soil}\n"
printf "        casafile_cnpbiome=${casafile_cnpbiome}\n"
printf "        MetPath=${MetPath}\n"
printf "        ClimateFile=${ClimateFile}\n"
printf "        TransitionFilePath=${TransitionFilePath}\n"
printf "        gm_lut_bernacchi_2002=${gm_lut_bernacchi_2002}\n"
printf "        gm_lut_walker_2013=${gm_lut_walker_2013}\n"
printf "        filename_d13c_atm=${filename_d13c_atm}\n"
printf "\n"



# --------------------------------------------------------------------
# Write global namelists
# --------------------------------------------------------------------
# Write standard namelists with options that are common to all steps
# of the sequence. They can, however, be overwritten in later steps.

# global met namelist file
if [[ ${read_fdiff} -eq 1 ]] ; then
    fdiff_bool=.true.
else
    fdiff_bool=.false.
fi

if [[ ${isite} -eq 0 ]] ; then  # landmask
    cat > ${tmp}/sedtmp.${pid} << EOF
        rainFile     = "${MetPath}/pre/pre_<startdate>_<enddate>.nc"
        lwdnFile     = "${MetPath}/dlwrf/dlwrf_<startdate>_<enddate>.nc"
        swdnFile     = "${MetPath}/tswrf/tswrf_<startdate>_<enddate>.nc"
        presFile     = "${MetPath}/pres/pres_<startdate>_<enddate>.nc"
        qairFile     = "${MetPath}/spfh/spfh_<startdate>_<enddate>.nc"
        TmaxFile     = "${MetPath}/tmax/tmax_<startdate>_<enddate>.nc"
        TminFile     = "${MetPath}/tmin/tmin_<startdate>_<enddate>.nc"
        uwindFile    = "${MetPath}/ugrd/ugrd_<startdate>_<enddate>.nc"
        vwindFile    = "${MetPath}/vgrd/vgrd_<startdate>_<enddate>.nc"
        fDiffFile    = "${MetPath}/fd/fd_<startdate>_<enddate>.nc"
        CO2File      = "${MetPath}/co2/co2_17000101_20221231.txt"
        NDepFile     = "${MetPath}/ndep/NDep_<startdate>_<enddate>.nc"
        LandMaskFile = "${LandMaskFile}"
        rainRecycle = T
        lwdnRecycle = T
        swdnRecycle = T
        presRecycle = T
        qairRecycle = T
        TmaxRecycle = T
        TminRecycle = T
        uWindRecycle = T
        vWindRecycle = T
        fDiffRecycle = T
        CO2Method = "1700"
        NDepMethod = "1850"
        ReadDiffFrac = ${fdiff_bool}
        DThrs        = 3.0  ! **CABLE** timestep hours (not the met timestep)
EOF
else  # site data
    cat > ${tmp}/sedtmp.${pid} << EOF
        RunType       = "spinup"
        CO2NDepFile   = "${CO2NdepFile}"
        spinstartyear = ${start_year}
        spinendyear   = ${end_year}
        spinCO2  = 285
        spinNdep = 0.79
        spinPdep = 0.144
EOF
fi
applysed ${tmp}/sedtmp.${pid} ${ndir}/${mettype}.nml ${rdir}/${mettype}_${experiment}.nml

if [[ ${isite} -eq 0 ]] ; then  # landmask
    cp ${ndir}/met_names.nml ${rdir}
fi

# global landuse change namelist
if [[ ${isite} -eq 0 ]] ; then  # landmask
    cat > ${tmp}/sedtmp.${pid} << EOF
        TransitionFilePath = "${TransitionFilePath}"
        ClimateFile        = "${ClimateFile}"
        YearStart          = 1700
        YearEnd            = 2022
EOF
    applysed ${tmp}/sedtmp.${pid} ${ndir}/luc.nml ${rdir}/luc_${experiment}.nml
fi

# global Cable namelist
if [[ "${Rubisco_params}" == "Bernacchi_2002" ]] ; then
    filename_gm_lut=${gm_lut_bernacchi_2002}
elif [[ "${Rubisco_params}" == "Walker_2013" ]] ; then
    filename_gm_lut=${gm_lut_walker_2013}
else
    filename_gm_lut=""
fi

cat > ${tmp}/sedtmp.${pid} << EOF
    filename%met                       = ""
    filename%veg                       = "${filename_veg}"
    filename%soil                      = "${filename_soil}"
    filename%type                      = "${SurfaceFile}"
    filename%out                       = "outputs/${mettype}_out_cable.nc"
    filename%restart_in                = "restart/${mettype}_cable_rst.nc"
    filename%restart_out               = "restart/${mettype}_cable_rst.nc"
    casafile%cnpbiome                  = "${casafile_cnpbiome}"
    casafile%out                       = "outputs/${mettype}_out_casa.nc"
    casafile%cnpipool                  = "restart/${mettype}_casa"
    casafile%cnpepool                  = "restart/${mettype}_casa"
    cable_user%CASA_OUT_FREQ           = "monthly"
    cable_user%POP_restart_in          = "restart/pop_${mettype}_ini.nc"
    cable_user%POP_restart_out         = "restart/pop_${mettype}_ini.nc"
    cable_user%LUC_restart_in          = "restart/${mettype}_LUC_rst.nc"
    cable_user%LUC_restart_out         = "restart/${mettype}_LUC_rst.nc"
    cable_user%LUC_outfile             = "outputs/${mettype}_out_LUC.nc"
    cable_user%climate_restart_in      = "restart/${mettype}_climate_rst.nc"
    cable_user%climate_restart_out     = "restart/${mettype}_climate_rst.nc"
    cable_user%RunIden                 = "${mettype}"
    cable_user%MetType                 = "${mettype}"
    output%averaging                   = "monthly"
    output%grid                        = "land"
    output%vars5D                      = .FALSE.
    leaps                              = .false.
    cable_user%SOIL_STRUC              = "default"
    cable_user%Rubisco_parameters      = "${Rubisco_params}"
    cable_user%CALL_POP                = .false.
    cable_user%coordinate_photosyn     = .false.
    cable_user%acclimate_photosyn      = .false.
    cable_user%explicit_gm             = .false.
    cable_user%gm_LUT_file             = "${filename_gm_lut}"
    cable_user%CALL_BLAZE              = .false.
    cable_user%c13o2                   = .false.
    cable_user%c13o2_simple_disc       = .false.
    cable_user%c13o2_delta_atm_file    = "${filename_d13c_atm}"
    cable_user%c13o2_outfile           = "outputs/${mettype}_out_casa_c13o2.nc"
    cable_user%c13o2_restart_in_flux   = "restart/${mettype}_c13o2_flux_rst.nc"
    cable_user%c13o2_restart_out_flux  = "restart/${mettype}_c13o2_flux_rst.nc"
    cable_user%c13o2_restart_in_pools  = "restart/${mettype}_c13o2_pools_rst.nc"
    cable_user%c13o2_restart_out_pools = "restart/${mettype}_c13o2_pools_rst.nc"
    cable_user%c13o2_restart_in_luc    = "restart/${mettype}_c13o2_luc_rst.nc"
    cable_user%c13o2_restart_out_luc   = "restart/${mettype}_c13o2_luc_rst.nc"
EOF
if [[ ${isite} -eq 1 ]] ; then  # site data
    ${ised} -e "/filename%met/s|=.*|= \"${MetFile}\"|" ${tmp}/sedtmp.${pid}
fi
if [[ ${call_pop} -eq 1 ]] ; then
    ${ised} -e "/cable_user%CALL_POP/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
fi
if [[ ${coordinate_photosyn} -eq 1 ]] ; then
    ${ised} -e "/cable_user%coordinate_photosyn/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
fi
if [[ ${acclimate_photosyn} -eq 1 ]] ; then
    ${ised} -e "/cable_user%acclimate_photosyn/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
fi
if [[ ${explicit_gm} -eq 1 ]] ; then
    ${ised} -e "/cable_user%explicit_gm/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
fi
if [[ ${doc13o2} -eq 1 ]] ; then
    ${ised} -e "/cable_user%c13o2/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
    if [[ ${c13o2_simple_disc} -eq 1 ]] ; then
        ${ised} -e "/cable_user%c13o2_simple_disc/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
    fi
fi
if [[ ${call_blaze} -eq 1 ]] ; then
    ${ised} -e "/cable_user%CALL_BLAZE/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
fi
applysed ${tmp}/sedtmp.${pid} ${ndir}/cable.nml ${rdir}/cable_${experiment}.nml
irm ${tmp}/sedtmp.${pid}.old

# BLAZE (no changes at the moment)
if [[ ${call_blaze} -eq 1 ]] ; then
    cp ${ndir}/blaze.nml ${rdir}/blaze.nml
fi


# --------------------------------------------------------------------
# --------------------------------------------------------------------
# Start Runs according to sequence specified above
# --------------------------------------------------------------------
# --------------------------------------------------------------------

# delete all restart files if required
if [[ ${purge_restart} -eq 1 ]] ; then
    rm -f ${rdir}/restart/*
    # rm -f ${ClimateFile}
fi

# --------------------------------------------------------------------
# 1. Create climate restart file
# --------------------------------------------------------------------
if [[ ${doclimate} -eq 1 ]] ; then
    echo "1. Create climate restart file"
    rid="climate_restart"

    # Met forcing
    cp ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

    # LUC
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	cp ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
    fi

    # Cable
    cat > ${tmp}/sedtmp.${pid} << EOF
        filename%restart_in            = ""
        cable_user%CLIMATE_fromZero    = .true.
        cable_user%YearStart           = 1841
        cable_user%YearEnd             = 1860
        icycle                         = 2
        spincasa                       = .false.
        cable_user%CASA_fromZero       = .true.
        cable_user%CASA_DUMP_READ      = .false.
        cable_user%CASA_DUMP_WRITE     = .true.
        cable_user%CASA_SPIN_STARTYEAR = 1850
        cable_user%CASA_SPIN_ENDYEAR   = 1859
        cable_user%limit_labile        = .true.
        casafile%cnpipool              = ""
        cable_user%POP_fromZero        = .true.
        cable_user%POP_out             = "ini"
        cable_user%POP_restart_in      = ""
        cable_user%POPLUC              = .false.
        cable_user%POPLUC_RunType      = "static"
        cable_user%c13o2               = .false.
EOF
    applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

    # run model
    cd ${rdir}
    irm logs/log_cable.txt logs/log_out_cable.txt
    # valgrind --tool=massif --xtree-memory=full --pages-as-heap=yes ./${iexe} > logs/log_out_cable.txt
    ./${iexe} > logs/log_out_cable.txt
    cp restart/${mettype}_climate_rst.nc ${ClimateFile}  # new for TRENDY >= v11
    saveid ${rid} ${mettype} ${doc13o2}
    cd ${pdir}
fi


# --------------------------------------------------------------------
# 2. First spinup phase from zero biomass
# --------------------------------------------------------------------
if [[ ${dofromzero} -eq 1 ]] ; then
    echo "2. First spinup from zero biomass"
    rid="zero_biomass"

    # Met forcing
    cp ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

    # LUC
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	cp ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
    fi

    # Cable
    cat > ${tmp}/sedtmp.${pid} << EOF
        filename%restart_in               = ""
        cable_user%CLIMATE_fromZero       = .true.
        cable_user%YearStart              = 1841
        cable_user%YearEnd                = 1870
        icycle                            = 2
        spincasa                          = .false.
        cable_user%CASA_OUT_FREQ          = "monthly"
        cable_user%CASA_fromZero          = .true.
        cable_user%CASA_DUMP_READ         = .false.
        cable_user%CASA_DUMP_WRITE        = .false.
        cable_user%CASA_SPIN_STARTYEAR    = 1850
        cable_user%CASA_SPIN_ENDYEAR      = 1859
        cable_user%limit_labile           = .true.
        casafile%cnpipool                 = ""
        cable_user%POP_fromZero           = .true.
        cable_user%POP_out                = "ini"
        cable_user%POP_restart_in         = ""
        cable_user%POPLUC                 = .true.
        cable_user%POPLUC_RunType         = "static"
        cable_user%c13o2_restart_in_flux  = ""
        cable_user%c13o2_restart_in_pools = ""
EOF
    if [[ ${isite} -eq 1 ]] ; then  # site data
	${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
    fi
    applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

    # run model
    cd ${rdir}
    irm logs/log_cable.txt logs/log_out_cable.txt
    # valgrind --tool=massif --xtree-memory=full --pages-as-heap=yes ./${iexe} > logs/log_out_cable.txt
    ./${iexe} > logs/log_out_cable.txt
    saveid ${rid} ${mettype} ${doc13o2}
    cd ${pdir}
fi


# --------------------------------------------------------------------
# 3. Biomass into quasi-equilibrium with restricted N and P pools
# --------------------------------------------------------------------
if [[ ${doequi1} -eq 1 ]] ; then
    echo "3. Bring biomass into quasi-equilibrium with unrestricted N and P pools"
    for ((iequi1=1; iequi1<=${nequi1}; iequi1++)) ; do
        # 3a. 30 year run starting from restart files
        echo "   3a. 30 year spinup from accumulated biomass; iequi1=${iequi1}/${nequi1}"
        rid="spinup_limit_labile_${iequi1}"
        # rid="spinup_limit_labile${iequi}"

	# Met forcing
        cp ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

        # LUC
	if [[ ${isite} -eq 0 ]] ; then  # landmask
	    cp ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
	fi

        # Cable
        cat > ${tmp}/sedtmp.${pid} << EOF
            cable_user%CLIMATE_fromZero    = .false.
            cable_user%YearStart           = 1841
            cable_user%YearEnd             = 1860
            icycle                         = 2
            spincasa                       = .false.
            cable_user%CASA_fromZero       = .false.
            cable_user%CASA_DUMP_READ      = .false.
            cable_user%CASA_DUMP_WRITE     = .true.
            cable_user%CASA_SPIN_STARTYEAR = 1850
            cable_user%CASA_SPIN_ENDYEAR   = 1859
            cable_user%limit_labile        = .true.
            cable_user%POP_fromZero        = .false.
            cable_user%POP_out             = "ini"
            cable_user%POPLUC              = .true.
            cable_user%POPLUC_RunType      = "static"
EOF
	if [[ ! -f restart/pop_${mettype}_ini.nc ]] ; then
	    # no restart file if POP%np == 0
	    ${ised} -e "/cable_user%POP_fromZero/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
	    echo "cable_user%POP_restart_in = \"\"" >> ${tmp}/sedtmp.${pid}
	fi
	if [[ ${isite} -eq 1 ]] ; then  # site data
	    ${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
	fi
        applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

        # run model
        cd ${rdir}
        irm logs/log_cable.txt logs/log_out_cable.txt
        ./${iexe} > logs/log_out_cable.txt
	saveid ${rid} ${mettype} ${doc13o2}
        cd ${pdir}

        #
        # 3b. analytic quasi-equilibrium of biomass pools
	echo "   3b. Analytic solution of biomass pools"
        rid="spinup_analytic_limit_labile"
        # rid="spin_casa_limit_labile${iequi}"

        # Met forcing
        cp ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

        # LUC
	if [[ ${isite} -eq 0 ]] ; then  # landmask
	    cp ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
	fi

        # Cable
        cat > ${tmp}/sedtmp.${pid} << EOF
            cable_user%CLIMATE_fromZero    = .false.
            cable_user%YearStart           = 1841
            cable_user%YearEnd             = 1860
            icycle                         = 12
            spincasa                       = .true.
            cable_user%CASA_fromZero       = .false.
            cable_user%CASA_DUMP_READ      = .true.
            cable_user%CASA_DUMP_WRITE     = .false.
            cable_user%CASA_SPIN_STARTYEAR = 1841
            cable_user%CASA_SPIN_ENDYEAR   = 1860
            cable_user%limit_labile        = .true.
            cable_user%POP_fromZero        = .false.
            cable_user%POP_out             = "ini"
            cable_user%POPLUC              = .true.
            cable_user%POPLUC_RunType      = "static"
EOF
	if [[ ! -f restart/pop_${mettype}_ini.nc ]] ; then
	    ${ised} -e "/cable_user%POP_fromZero/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
	    echo "cable_user%POP_restart_in = \"\"" >> ${tmp}/sedtmp.${pid}
	fi
	if [[ ${isite} -eq 1 ]] ; then  # site data
	    ${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
	fi
        applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

        # run model
        cd ${rdir}
        irm logs/log_cable.txt logs/log_out_cable.txt
        ./${iexe} > logs/log_out_cable.txt
	saveid ${rid} ${mettype} ${doc13o2}
        cd ${pdir}
    done
fi


# --------------------------------------------------------------------
# 4. Biomass into quasi-equilibrium without restricted N and P pools
# --------------------------------------------------------------------
if [[ ${doequi2} -eq 1 ]] ; then
    echo "4. Bring biomass into quasi-equilibrium with restricted N and P pools"
    for ((iequi2=1; iequi2<=${nequi2}; iequi2++)) ; do
        # 4a. 30 year run starting from restart files
        echo "   4a. 30 year spinup from accumulated biomass; iequi2=${iequi2}/${nequi2}"
        # rid="spinup_nutrient_limited"
        rid="spinup_nutrient_limited_${iequi2}"

	# Met forcing
        cp ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

	# LUC
	if [[ ${isite} -eq 0 ]] ; then  # landmask
	    cp ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
	fi

	# Cable
        cat > ${tmp}/sedtmp.${pid} << EOF
            cable_user%CLIMATE_fromZero    = .false.
            cable_user%YearStart           = 1841
            cable_user%YearEnd             = 1860
            icycle                         = 2
            spincasa                       = .false.
            cable_user%CASA_fromZero       = .false.
            cable_user%CASA_DUMP_READ      = .false.
            cable_user%CASA_DUMP_WRITE     = .true.
            cable_user%CASA_SPIN_STARTYEAR = 1850
            cable_user%CASA_SPIN_ENDYEAR   = 1859
            cable_user%limit_labile        = .false.
            cable_user%POP_fromZero        = .false.
            cable_user%POP_out             = "ini"
            cable_user%POPLUC              = .true.
            cable_user%POPLUC_RunType      = "static"
EOF
	if [[ ! -f restart/pop_${mettype}_ini.nc ]] ; then
	    ${ised} -e "/cable_user%POP_fromZero/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
	    echo "cable_user%POP_restart_in = \"\"" >> ${tmp}/sedtmp.${pid}
	fi
	if [[ ${isite} -eq 1 ]] ; then  # site data
	    ${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
	fi
        applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

        # run model
        cd ${rdir}
        irm logs/log_cable.txt logs/log_out_cable.txt
        ./${iexe} > logs/log_out_cable.txt
	saveid ${rid} ${mettype} ${doc13o2}
        cd ${pdir}

	#
        # 4b. analytic quasi-equilibrium of biomass pools
        if [[ ${iequi2} -le ${nequi2a} ]] ; then
            echo "   4b. Analytic solution of biomass pools"
            # rid="spinup_analytic"
            rid="spinup_analytic_${iequi2}"
            # Met forcing
            cp ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

            # LUC
	    if [[ ${isite} -eq 0 ]] ; then  # landmask
		cp ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
	    fi

            # Cable
            cat > ${tmp}/sedtmp.${pid} << EOF
                cable_user%CLIMATE_fromZero    = .false.
                cable_user%YearStart           = 1841
                cable_user%YearEnd             = 1860
                icycle                         = 12
                spincasa                       = .true.
                cable_user%CASA_fromZero       = .false.
                cable_user%CASA_DUMP_READ      = .true.
                cable_user%CASA_DUMP_WRITE     = .false.
                cable_user%CASA_SPIN_STARTYEAR = 1841
                cable_user%CASA_SPIN_ENDYEAR   = 1860
                cable_user%limit_labile        = .false.
                cable_user%POP_fromZero        = .false.
                cable_user%POP_out             = "ini"
                cable_user%POPLUC              = .true.
                cable_user%POPLUC_RunType      = "static"
EOF
	    if [[ ! -f restart/pop_${mettype}_ini.nc ]] ; then
		${ised} -e "/cable_user%POP_fromZero/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
		echo "cable_user%POP_restart_in = \"\"" >> ${tmp}/sedtmp.${pid}
	    fi
	    if [[ ${isite} -eq 1 ]] ; then  # site data
		${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
	    fi
            applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

            # run model
            cd ${rdir}
            irm logs/log_cable.txt logs/log_out_cable.txt
            ./${iexe} > logs/log_out_cable.txt
	    saveid ${rid} ${mettype} ${doc13o2}
            cd ${pdir}
        fi
    done
fi


# --------------------------------------------------------------------
# 5. First dynamic land use (Initialise land use)
# --------------------------------------------------------------------
if [[ ${doiniluc} -eq 1 ]] ; then
    echo "5. First dynamic land use (initialise land use)"
    rid="init_land_use"

    # Met forcing
    YearStart=1580  # should be the same as in the global luc.nml file!
    YearEnd=1699
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	cp ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml
    else  # site data
	cat > ${tmp}/sedtmp.${pid} << EOF
    RunType = "transient"
EOF
	applysed ${tmp}/sedtmp.${pid} ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml
    fi

    # LUC
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	cat > ${tmp}/sedtmp.${pid} << EOF
    	    YearStart = ${YearStart}
    	    YearEnd   = ${YearEnd}
EOF
	applysed ${tmp}/sedtmp.${pid} ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
    fi

    # Cable
    cat > ${tmp}/sedtmp.${pid} << EOF
        cable_user%CLIMATE_fromZero     = .false.
        cable_user%YearStart            = ${YearStart}
        cable_user%YearEnd              = ${YearEnd}
        icycle                          = 12
        spincasa                        = .false.
        cable_user%CASA_OUT_FREQ        = "annually"
        cable_user%CASA_fromZero        = .false.
        cable_user%CASA_DUMP_READ       = .true.
        cable_user%CASA_DUMP_WRITE      = .false.
        cable_user%CASA_SPIN_STARTYEAR  = 1841
        cable_user%CASA_SPIN_ENDYEAR    = 1860
        cable_user%limit_labile         = .false.
        cable_user%POP_fromZero         = .false.
        cable_user%POP_out              = "ini"
        cable_user%POPLUC               = .true.
        cable_user%POPLUC_RunType       = "init"
        cable_user%LUC_restart_in       = ""
        cable_user%c13o2_restart_in_luc = ""
EOF
    if [[ ! -f restart/pop_${mettype}_ini.nc ]] ; then
	${ised} -e "/cable_user%POP_fromZero/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
	echo "cable_user%POP_restart_in = \"\"" >> ${tmp}/sedtmp.${pid}
    fi
    if [[ ${isite} -eq 1 ]] ; then  # site data
	${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
    fi
    applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

    # run model
    cd ${rdir}
    irm logs/log_cable.txt logs/log_out_cable.txt
    ./${iexe} > logs/log_out_cable.txt
    saveid ${rid} ${mettype} ${doc13o2}
    cd ${pdir}
fi


# --------------------------------------------------------------------
# 6. Transient run
# --------------------------------------------------------------------
if [[ ${doinidyn} -eq 1 ]] ; then
    echo "6. Transient run (full dynamic spinup)"

    # Met forcing
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	YearStart=1700
	YearEnd=1900
    else  # site data
	YearStart=${start_year_transient}
	YearEnd=${end_year_transient}
    fi
    rid=${YearStart}_${YearEnd}

    if [[ ${isite} -eq 0 ]] ; then  # landmask
	if [[ "${experiment}" == "S0" ]] ; then
            cat > ${tmp}/sedtmp.${pid} << EOF
            	Run = "S0_TRENDY"
            	CO2Method = "1700"
            	NDepMethod = "1850"
EOF
	elif [[ "${experiment}" == "S1" || "${experiment}" == "S2" || "${experiment}" == "S3" ]] ; then
            cat > ${tmp}/sedtmp.${pid} << EOF
            	Run = "S1_TRENDY"
            	CO2Method = "Yearly"
            	NDepMethod = "Yearly"
EOF
	fi
    else  # site data
	cat > ${tmp}/sedtmp.${pid} << EOF
             RunType = "transient"
EOF
    fi
    applysed ${tmp}/sedtmp.${pid} ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

    # LUC
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	if [[ "${experiment}" == "S3" ]] ; then
	    cat > ${tmp}/sedtmp.${pid} << EOF
    		YearStart = ${YearStart}
    		YearEnd   = ${YearEnd}
EOF
	else
	    cat > ${tmp}/sedtmp.${pid} << EOF
    		YearStart = 1700
    		YearEnd   = ${YearEnd}
EOF
	fi
	applysed ${tmp}/sedtmp.${pid} ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
    fi

    # Cable
    if [[ "${experiment}" == "S3" ]] ; then
	POPLUC_RunType="restart"
    else
        POPLUC_RunType="static"
    fi

    cat > ${tmp}/sedtmp.${pid} << EOF
        cable_user%CLIMATE_fromZero    = .false.
        cable_user%YearStart           = ${YearStart}
        cable_user%YearEnd             = ${YearEnd}
        icycle                         = 2
        spincasa                       = .false.
        cable_user%CASA_fromZero       = .false.
        cable_user%CASA_DUMP_READ      = .false.
        cable_user%CASA_DUMP_WRITE     = .false.
        cable_user%CASA_SPIN_STARTYEAR = 1850
        cable_user%CASA_SPIN_ENDYEAR   = 1859
        cable_user%limit_labile        = .false.
        cable_user%POP_fromZero        = .false.
        cable_user%POP_out             = "ini"
        cable_user%POPLUC              = .true.
        cable_user%POPLUC_RunType      = "${POPLUC_RunType}"
EOF
    if [[ ! -f restart/pop_${mettype}_ini.nc ]] ; then
	${ised} -e "/cable_user%POP_fromZero/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
	echo "cable_user%POP_restart_in = \"\"" >> ${tmp}/sedtmp.${pid}
    fi
    if [[ ${isite} -eq 1 ]] ; then  # site data
	${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
    fi
    applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

    # run model
    cd ${rdir}
    irm logs/log_cable.txt logs/log_out_cable.txt
    ./${iexe} > logs/log_out_cable.txt
    saveid ${rid} ${mettype} ${doc13o2}
    cd ${pdir}
fi


# --------------------------------------------------------------------
# 7. Final centennial run
# --------------------------------------------------------------------
if [[ ${dofinal} -eq 1 ]] ; then
    echo "7. Final centennial run"

    # Met forcing
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	YearStart=1901
	YearEnd=2022
    else  # site data
	YearStart=${start_year}
	YearEnd=${end_year}
    fi
    rid=${YearStart}_${YearEnd}

    if [[ ${isite} -eq 0 ]] ; then  # landmask
	if [[ "${experiment}" == "S0" ]] ; then
            cat > ${tmp}/sedtmp.${pid} << EOF
            	Run = "S0_TRENDY"
            	CO2Method = "1700"
            	NDepMethod = "1850"
EOF
	elif [[ "${experiment}" == "S1" ]] ; then
            cat > ${tmp}/sedtmp.${pid} << EOF
            	Run = "S1_TRENDY"
            	CO2Method = "Yearly"
            	NDepMethod = "Yearly"
EOF
	elif [[ "${experiment}" == "S2" || "${experiment}" == "S3" ]] ; then
            cat > ${tmp}/sedtmp.${pid} << EOF
            	Run = "S2_TRENDY"
            	CO2Method = "Yearly"
            	NDepMethod = "Yearly"
            	rainRecycle = .false.
            	lwdnRecycle = .false.
            	swdnRecycle = .false.
            	presRecycle = .false.
            	qairRecycle = .false.
            	TmaxRecycle = .false.
            	TminRecycle = .false.
            	uWindRecycle = .false.
            	vWindRecycle = .false.
            	fDiffRecycle = .false.
EOF
	fi
    else  # site data
	cat > ${tmp}/sedtmp.${pid} << EOF
            RunType = "historical"
EOF
    fi
    applysed ${tmp}/sedtmp.${pid} ${rdir}/${mettype}_${experiment}.nml ${rdir}/${mettype}.nml

    # LUC
    if [[ ${isite} -eq 0 ]] ; then  # landmask
	if [[ "${experiment}" == "S3" ]] ; then
	    cat > ${tmp}/sedtmp.${pid} << EOF
    		YearStart = ${YearStart}
    		YearEnd   = ${YearEnd}
EOF
	else
	    cat > ${tmp}/sedtmp.${pid} << EOF
    		YearStart = 1700
    		YearEnd   = ${YearEnd}
EOF
	fi
	applysed ${tmp}/sedtmp.${pid} ${rdir}/luc_${experiment}.nml ${rdir}/luc.nml
    fi

    # Cable
    if [[ "${experiment}" == "S3" ]] ; then
	POPLUC_RunType="restart"
    else
        POPLUC_RunType="static"
    fi

    cat > ${tmp}/sedtmp.${pid} << EOF
        cable_user%CLIMATE_fromZero    = .false.
        cable_user%YearStart           = ${YearStart}
        cable_user%YearEnd             = ${YearEnd}
        icycle                         = 2
        spincasa                       = .false.
        cable_user%CASA_fromZero       = .false.
        cable_user%CASA_DUMP_READ      = .false.
        cable_user%CASA_DUMP_WRITE     = .false.
        cable_user%CASA_SPIN_STARTYEAR = 1850
        cable_user%CASA_SPIN_ENDYEAR   = 1859
        cable_user%limit_labile        = .false.
        cable_user%POP_fromZero        = .false.
        cable_user%POP_out             = "ini"
        cable_user%POPLUC              = .true.
        cable_user%POPLUC_RunType      = "${POPLUC_RunType}"
EOF
    if [[ ! -f restart/pop_${mettype}_ini.nc ]] ; then
	${ised} -e "/cable_user%POP_fromZero/s|=.*|= .true.|" ${tmp}/sedtmp.${pid}
	echo "cable_user%POP_restart_in = \"\"" >> ${tmp}/sedtmp.${pid}
    fi
    if [[ ${isite} -eq 1 ]] ; then  # site data
	${ised} -e "/cable_user%POPLUC[^_]/s|=.*|= .false.|" ${tmp}/sedtmp.${pid}
    fi
    applysed ${tmp}/sedtmp.${pid} ${rdir}/cable_${experiment}.nml ${rdir}/cable.nml

    # run model
    cd ${rdir}
    irm logs/log_cable.txt logs/log_out_cable.txt
    ./${iexe} > logs/log_out_cable.txt
    saveid ${rid} ${mettype} ${doc13o2}
    cd ${pdir}
fi


# --------------------------------------------------------------------
# Finish
# --------------------------------------------------------------------
cd ${isdir}

t2=$(date +%s)
dt=$((t2-t1))
printf "\n"
if [[ ${dt} -lt 60 ]] ; then
    printf "Finished at %s   in %i seconds.\n" "$(date)" ${dt}
else
    dm=$(echo "(${t2}-${t1})/60." | bc -l)
    printf "Finished at %s   in %.2f minutes.\n" "$(date)" ${dm}
fi

exit
