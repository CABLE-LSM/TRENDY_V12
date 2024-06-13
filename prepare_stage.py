#!/usr/bin/env python3

# Author: Lachlan Whyborn
# Last Modified: Thu 13 Jun 2024 06:25:40 PM AEST

import f90nml
import argparse
import os
from prep_namelists import merge_namelists, keyword_replace
from prep_inputs import symlink_inputs

def prepare_stage(StageName, RestartDir, Run, Cycle):
    """Build the namelists that CABLE will use for stage of a configuration, and do the symlinking of inputs to pre-defined locations for consistency."""
    # Master stage preparation function. We pass each of the individual stages out to
    # separate functions. The stages are:
    #   - Merge the master namelists and the stage namelists
    #   - Symlink the input files to pre-defined locations specified in namelists
    #   - Update the namelists to point to the new symlinked locations
    # This way the user in their preparation still points to the original location of the
    # input data, but the generated namelist options point to the generated symlinks.

    # The setup is such that the user namelists contain internal namelists referring to
    # each of the CABLE required namelists. For example, TRENDY runs of CABLE require
    # ```cable.nml``` (with internal namelist ```&cablenml```), ```luc.nml``` (with
    # internal namelist ```&lucnml``` and ```cru.nml``` (with internal namelist 
    # ```&crunml```. The user's stage configuration namelist file can then contain
    # namelists &cablenml, &lucnml and &crunml.

    # So we want to open the user's namelist file, and use the namelists within that
    # file to determine which master and input namelists to open.
    with open(f"stage_configurations/{StageName}.nml", 'r') as nmlFile:
        StageNamelist = f90nml.read(nmlFile)

    for NMLName in StageNamelist.keys():
        # First, open the master namelist file associated with this namelist
        with open(f"namelists/{NMLName[:-3]}.nml") as nmlMasterFile:
            MasterNamelist = f90nml.read(nmlMasterFile)

        # Merge the stage namelist into the master namelist. To make sure we're comparing
        # apples to apples, we need to pull out the contents of the namelist in the file
        merge_namelists(MasterNamelist[NMLName], StageNamelist[NMLName])

        # We have a series of keywords to replace
        # For now, we define the series of replacements here. Maybe in the future, we will
        # delegate this out to an external file
        keyword_replace(MasterNamelist, "<restartdir>", f"{RestartDir}")
        keyword_replace(MasterNamelist, "<run>", f"{Run}")
        keyword_replace(MasterNamelist, "<homedir>", os.getcwd())
        
        # Before symlinking, we need to ensure that the target directories for the input
        # symlinks exist
        TargetDir = os.path.join(os.getcwd(), "results", StageName)
        TargetDir = os.path.join(TargetDir, f"run{Run}") if Run else TargetDir
        TargetDir = os.path.join(TargetDir, f"cycle{Cycle}") if Cycle else TargetDir

        os.makedirs(TargetDir, exist_ok = True)
        os.makedirs(os.path.join(TargetDir, "inputs/ancillary"), exist_ok = True)
        os.makedirs(os.path.join(TargetDir, "inputs/restart"), exist_ok = True)

        # Build the symlinks to the predefined input locations then modify the master
        # namelist. The location of the input namelists is in the same directory as the
        # script.
        InputName = os.path.join(os.path.dirname(os.path.realpath(__file__)), f"input_namelists", f"{NMLName[:-3]}_inputs.nml")
        with open(InputName) as nmlInputFile:
            InputNamelist = f90nml.read(nmlInputFile)

        symlink_inputs(MasterNamelist[NMLName], InputNamelist[NMLName], TargetDir)

        # Now we can write the modified namelist to disk
        with open(os.path.join(TargetDir, f"{NMLName[:-3]}.nml"), 'w') as WriteFile:
            MasterNamelist.write(WriteFile, force = True)

    return 0

if __name__ == "__main__":
    # Prep the argument parser to read the command line arguments
    ArgParser = argparse.ArgumentParser(description = "Prepare the namelists to run a stage of a configuration.")
    ArgParser.add_argument("StageName", help = "Where to write the results to.", type = str)
    ArgParser.add_argument("RestartDir", nargs = '?', help = "Location of the previous stage.", type = str, default = "")
    ArgParser.add_argument('-r', '--run', nargs = '?', dest = "Run", help = "The run ID in the pseudo-parallel run.", default = None)
    ArgParser.add_argument('-c', '--cycle', nargs = '?', dest = "Cycle", help = "The cycle number in a multi-stage run.", default = None)

    args = ArgParser.parse_args()

    prepare_stage(args.StageName, args.RestartDir, args.Run, args.Cycle)
