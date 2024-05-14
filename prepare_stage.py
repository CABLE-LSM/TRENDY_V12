# Author: Lachlan Whyborn
# Last Modified: Wed 08 May 2024 01:06:27 PM AEST

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

def BuildNamelists(NamelistDir, TargetDir, ConfigFile, NRuns):
    # Here we make the necessary changes to the namelists for the
    # climate spinup stage.
    # We should be able to read in the file as a string, then use a replace
    # to rewrite the desired configuration options, and write the new string
    # to the new namelist file.
    # We allow the user to prescribe any options that should be overwritten
    with open(ConfigFile, 'r') as Cfg:
        ConfigOptions = yaml.safe_load(Cfg)

    # Ensure target directory is present
    os.makedirs(f"{TargetDir}", exist_ok = True)
    for Run in range(1, NRuns+1):
        os.makedirs(f"{TargetDir}/run{Run:03d}/namelists/", exist_ok = True)

    # We have an entry in the configoptions for each namelist, so we can iterate through
    for Namelist, NamelistOptions in ConfigOptions.items():
        ReadFile = f"{NamelistDir}/{Namelist}.nml"
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
                    # This finds the string "ConfigOption*\n", and replaces it with the desired
                    # option.
                    # The function has the special handling for Fortran's booleans, 
                    # as python won't recognise .TRUE. and .FALSE. as booleans.
                    FileText = ReplaceOption(ConfigOption, OptionValue, FileText)

            # At the end, we can make the run-appropriate substitutions and then write to file
            for Run in range(1, NRuns+1):
                os.makedirs(f"{TargetDir}/run{Run:03d}", exist_ok = True)
                WriteFile = f"{TargetDir}/run{Run:03d}/namelists/{Namelist}.nml"
                with open(WriteFile, 'w+') as WriteTo:
                    # Modify any instances of <run> to the desired run___
                    RunText = FileText.replace("<run>", f"run{Run:03d}")

                    # Write the new string to file
                    WriteTo.write(RunText)

if __name__ == "__main__":
    # Prep the argument parser to read the command line arguments
    ArgParser = argparse.ArgumentParser(description = "Prepare the namelists to run a stage of a configuration.")
    ArgParser.add_argument("NamelistDir", help = "Location of the base namelists.")
    ArgParser.add_argument("TargetDir", help = "Directory of the current stage.")
    ArgParser.add_argument("ConfigFile", help = "YAML file containing the modified config options.")
    ArgParser.add_argument("NRuns", help = "Number of simultaneous serial runs in the job.", type = int)

    args = ArgParser.parse_args()

    BuildNamelists(args.NamelistDir, args.TargetDir, args.ConfigFile, args.NRuns)

