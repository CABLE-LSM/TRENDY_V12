#!/usr/bin/env bash

# Make links in format var_startdate_enddate.nc
# to the CRUJRA met files of TRENDY v12 (2023)
#
# CO2 and nitrogen deposition are linked by hand (hard-coded below).
#
# Similar to Python script of Lachlan Whyborn
#    https://gist.github.com/Whyborn/97c57babc7a531e0605c9d44802a7ce1
#
# Written, Matthias Cuntz, July 2024

set -e

# input/output directories
# Gadi
# idir="/g/data/rp23/data/no_provenance/met_forcing/crujra_1x1_1d/v2.4/"
# odir="/g/data/rp23/experiments/2024-03-12_CABLE4-dev/lw5085/data_links/"
#
# biocomp
# if run in met directory daily_1deg
idir=${PWD}
odir=../daily_1deg_met

# make absolute from relative directories
cd ${idir}
idir=${PWD}
cd -

mkdir -p ${odir}
cd ${odir}
odir=${PWD}

# directories with meteo files (without co2 and ndep directories)
cd ${idir}
dirs=$(find . -maxdepth 1 -type d | sed -e 's|./||' -e 's|^[.]||' -e 's|co2||' -e 's|ndep||')

for d in ${dirs} ; do
    mkdir -p ${odir}/${d}
    cd ${odir}/${d}
    for i in ${idir}/${d}/*${d}* ; do
	# filenames are like:
	#    pres/crujra.v2.4.5d.pres.2022.365d.noc.daymean.1deg.nc
	# or like
	#    fd/fd_v12_2022.daymean.1deg.nc
        yr=${i##*${d}}
        if [[ ${i} == *crujra* ]] ; then
            yr=${yr:1:4}
        else
            yr=${yr:5:4}
        fi
	echo ln ${i} ${d}_${yr}0101_${yr}1231.nc
        ln -sf ${i} ${d}_${yr}0101_${yr}1231.nc
    done
done

# Link co2 and nitrogen forcing by hand
mkdir -p ${odir}/co2
cd ${odir}/co2
echo ln ${idir}/co2/global_co2_ann_1700_2022.txt co2_17000101_20221231.txt
ln -sf ${idir}/co2/global_co2_ann_1700_2022.txt co2_17000101_20221231.txt

mkdir -p ${odir}/ndep
cd ${odir}/ndep
echo ln ${idir}/ndep/NOy_plus_NHx_dry_plus_wet_deposition_1850_2099_annual.1deg.nc NDep_18500101_20991231.nc
ln -s ${idir}/ndep/NOy_plus_NHx_dry_plus_wet_deposition_1850_2099_annual.1deg.nc NDep_18500101_20991231.nc

exit
