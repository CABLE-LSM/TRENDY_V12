#!/usr/bin/env python3

# Author: Lachlan Whyborn
# Last Modified: Tue 28 May 2024 15:27:47

import argparse
import yaml
import os
import re

def ReplaceOption(ConfigOption, OptionValue, FileText):
    """Replaces the line in FileText containing ConfigOption with 
    ConfigOption = OptionValue."""
    # Perform the in-place substitution for the config option.
    # We pass it out to a function for the special handling of fortran's booleans
    FortranBooleans = [".FALSE.", ".TRUE.", "T", "F"]

    if isinstance(OptionValue, str):
        # Taking the uppercase version simplifies the replacement process, so we catch any case combination of TRUE and FALSE.
        if OptionValue.upper() in FortranBooleans:
            # It's a fortran boolean, don't include quotations
            FileText = re.sub(f"{ConfigOption}(.*?)\n", f"{ConfigOption} = {OptionValue}\n", FileText, re.DOTALL)
        else:
            # Is a string option, include quotations
            FileText = re.sub(f"{ConfigOption}(.*?)\n", f"{ConfigOption} = \"{OptionValue}\"\n", FileText, re.DOTALL)
    else:
        # A numeric value
        FileText = re.sub(f"{ConfigOption}(.*?)\n", f"{ConfigOption} = {OptionValue}\n", FileText, re.DOTALL)

    return FileText

def BuildNamelists(StageName, RestartDir, Run, Cycle):
    # Here we make the changes to the default namelists for the current stage.
    # The changes to the defaults are contained in the respective config YAML files
    # We read in the original namelists as a string, and make string replacements
    # for the desired options.

    # The building of the TargetDir and RestartDir is handled by the bash script.
    # RestartDir is the location that we're getting the restart files from, and 
    # TargetDir is the location of the current simulation stage.

    # We allow the user to prescribe any options that should be overwritten
    with open(f"stage_configurations/{StageName}.yml", 'r') as Cfg:
        ConfigOptions = yaml.safe_load(Cfg)

    # Build the target directory which will be the location the stage is run from
    # If Run and Cycle are not none, append them to the path
    TargetDir = os.path.join(os.getcwd(), StageName)
    TargetDir = os.path.join(TargetDir, f"run{Run}") if Run else TargetDir
    TargetDir = os.path.join(TargetDir, f"cycle{Cycle}") if Cycle else TargetDir

    # Ensure target directory is present
    os.makedirs(f"{TargetDir}/namelists/", exist_ok = True)

    print(f"Writing the namelists to {TargetDir}/namelists.")
    # We have an entry in the configoptions for each namelist, so we can iterate through
    for Namelist, NamelistOptions in ConfigOptions.items():
        ReadFile = f"namelists/{Namelist}.nml"
        WriteFile = f"{TargetDir}/namelists/{Namelist}.nml"
        with open(ReadFile, 'r') as ReadFrom:
            # Read in the file as a string
            FileText = ReadFrom.read()

            # Now we can use regex to find and replace the desired entries in the namelist
            # We have a list of things to change from the ConfigOptions, everything should
            # remain as is.
            # Sometimes we have no changes in the namelist, so the Namelist options will be
            # None
            if NamelistOptions is not None:
                for ConfigOption, OptionValue in NamelistOptions.items():
                    # This finds the string "ConfigOption*\n", and replaces it
                    # with the desired option.
                    # The function has the special handling for Fortran's booleans, 
                    # as python won't recognise .TRUE. and .FALSE. as booleans.
                    FileText = ReplaceOption(ConfigOption, OptionValue, FileText)

            # Usually the simulations will involve reading restart data from a previous
            # simulation. The locations for the restart data is assisted by a series of
            # placeholders.
            # The most common case is that we get the restart file from the prior stage,
            # which we access via <restartdir>.
            FileText = FileText.replace("<restartdir>", f"{RestartDir}")

            # Another case is when we want to re-use the data from a specific stage, in
            # which case we need to retrieve the data from the correct run. We use <run>
            # as a placeholder for run{Run} (note that Run is already a 3 digit 0 filled
            # string in the input.
            FileText = FileText.replace("<run>", f"run{Run}")

            # When constructing filepaths, often we want the starting directory, which
            # we denote with the <homedir> placeholder.
            FileText = FileText.replace("<homedir>", os.getcwd())

            # Open the target WriteFile and write the new namelist to it
            with open(WriteFile, 'w+') as WriteTo:
                # Write the new string to file
                WriteTo.write(FileText)

if __name__ == "__main__":
    # Prep the argument parser to read the command line arguments
    ArgParser = argparse.ArgumentParser(description = "Prepare the namelists to run a stage of a configuration.")
    ArgParser.add_argument("StageName", help = "Where to write the results to.", type = str)
    ArgParser.add_argument("RestartDir", nargs = '?', help = "Location of the previous stage.", type = str, default = "")
    ArgParser.add_argument('-r', '--run', nargs = '?', dest = "Run", help = "The run ID in the pseudo-parallel run.", default = None)
    ArgParser.add_argument('-c', '--cycle', nargs = '?', dest = "Cycle", help = "The cycle number in a multi-stage run.", default = None)

    args = ArgParser.parse_args()

    BuildNamelists(args.StageName, args.RestartDir, args.Run, args.Cycle)
