[//]: # (Author: Lachlan Whyborn, Matthias Cuntz)
[//]: # (Date Modified: 2024-05-02)

# TRENDY v12 Experiment Configuration

This repository contains the auxiliary scripts required to run the TRENDY configuration on the biocomp cluster with files in /home/mcuntz.

## Usage

Copy these scripts to do the desired run directory on biocomp. A valid anaconda environment with the name pystd is assumed. The changes needed to be made by the user are in the ```run_TRENDY.sh``` script and are:

* Specify the location of the ```cable``` repository, by setting the ```cablecode``` variable on line 46.
* Set the experiment to run by setting the ```experiment``` variable on line 26. The experiment names correspond to a set of internal configuration options.

## Resources

The default job configuration, with 100 parallel runs, takes ~8 hours, 450GB of memory and ~12000 files, so ensure that the file system you are running on has the required resources.
