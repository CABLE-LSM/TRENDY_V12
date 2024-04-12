[//]: # (Author: Lachlan Whyborn)
[//]: # (Date Modified: )

# TRENDY v12 Experiment Configuration

This repository contains the auxiliary scripts required to run the TRENDY configuration on the RP23 NCI project on Gadi.

## Usage

Copy these scripts to do the desired run directory on Gadi. Access to the HH5 project is also required to run the python landmask script. The changes needed to be made by the user are in the ```run_TRENDY.sh``` script and are:

* Specify the location of the ```cable``` repository, by setting the ```cablecode``` variable on line 46.
* Set the experiment to run by setting the ```experiment``` variable on line 26. The experiment names correspond to a set of internal configuration options.

It's still not entirely clear what each of the experiments do. This will be clarified after discussions with the original custodians of the configuration.

## Resources

The default job configuration, with 100 parallel runs, takes ~8 hours, 450GB of memory and ~12000 files, so ensure that the file system you are running on has the required resources.
