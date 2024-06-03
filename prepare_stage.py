#!/usr/bin/env python3

# Author: Lachlan Whyborn
# Last Modified: Tue 28 May 2024 15:27:47

import f90nml
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

def KeywordReplace(Input, Target, Replacement):
    """Replace the Target string in InputString with Replacement. This is effectively a recursive wrapper around the typical replace function."""

    # We have 3 options:
    #   - The input is a string, so we do a simple replacement
    #   - The input is a dictionary, so we call this function on each of its values
    #   - The input is anything else, in which case leave it
    if isinstance(Input, str):
        return Input.replace(Target, Replacement)
    elif isinstance(Input, dict):
        for Option, OptionValue in keys:
            Input[Option] = KeywordReplace(OptionValue, Target, Replacement)
        return Input
    else
        return Input

def BuildNamelists(StageName, RestartDir, Run, Cycle):
    # Here we make the changes to the default namelists for the current stage.
    # The changes to the defaults are contained in the respective config YAML files
    # We read in the original namelists as a string, and make string replacements
    # for the desired options.

    # The building of the TargetDir and RestartDir is handled by the bash script.
    # RestartDir is the location that we're getting the restart files from, and 
    # TargetDir is the location of the current simulation stage.

    # We allow the user to prescribe any options that should be overwritten,
    # using reduced namelists of the changed options.
    # We're using the f90nml library so Fortran users can keep their namelist
    # formatting for the replacement namelists.
    # Note that as of 03/06/24, there is an undocumented change in the f90nml
    # library, namelists are not loaded as dictionaries by default, rather as
    # Namelist custom types. These custom types seem to behave like dictionaries
    # for all intents and purposes.
    with open(f"stage_configurations/{StageName}.nml", 'r') as Cfg:
        StageOptions = f90nml.read(Cfg)

    # Build the target directory which will be the location the stage is run from
    # If Run and Cycle are not none, append them to the path
    TargetDir = os.path.join(os.getcwd(), "results", StageName)
    TargetDir = os.path.join(TargetDir, f"run{Run}") if Run else TargetDir
    TargetDir = os.path.join(TargetDir, f"cycle{Cycle}") if Cycle else TargetDir

    # Ensure target directory is present
    os.makedirs(f"{TargetDir}", exist_ok = True)

    # We have an entry in the configoptions for each namelist, so we can iterate through
    for Namelist, NamelistOptions in StageOptions.items():
        ReadFile = f"namelists/{Namelist}.nml"
        WriteFile = f"{TargetDir}/{Namelist}.nml"
        # Open the original namelists
        with open(ReadFile, 'r') as ReadFrom:
            # Read in the namelist
            MasterNamelist = f90nml.read(ReadFrom)

        # We know that each namelist file only contains one namelist, so we can cheat
        # a little bit and just take the first entry from the namelist values
        MasterNamelist = next(iter(MasterNamelist.values()))

        # We can simply update the master namelist with the values in the stage options
        MasterNamelist.update(NamelistOptions)

        # Now go through and make any keyword substitutions
        for Option, OptionValue in MasterNamelist.items():
            # Unfortunately, the f90nml library handles struct values as nested namelists
            # i.e. an entry "cable%option : <value>" would be represented as a sub namelist
            # with entry {"cable: {"option: <value>"}}, rather than {"cable%option: <value}.
            # So we need to apply a recursive replace function
            # Only apply to string objects
            MasterNamelist[Option] = KeywordReplace(OptionValue, "<restartdir>", f"{RestartDir}")
            MasterNamelist[Option] = KeywordReplace(OptionValue, "<run>", f"{Run}")
            MasterNamelist[Option] = KeywordReplace(OptionValue, "<homedir>", os.getcwd())

        # Open the target WriteFile and write the new namelist to it
        with open(WriteFile, 'w+') as WriteTo:
            # Write the new string to file
            MasterNamelist.write(WriteTo)

if __name__ == "__main__":
    # Prep the argument parser to read the command line arguments
    ArgParser = argparse.ArgumentParser(description = "Prepare the namelists to run a stage of a configuration.")
    ArgParser.add_argument("StageName", help = "Where to write the results to.", type = str)
    ArgParser.add_argument("RestartDir", nargs = '?', help = "Location of the previous stage.", type = str, default = "")
    ArgParser.add_argument('-r', '--run', nargs = '?', dest = "Run", help = "The run ID in the pseudo-parallel run.", default = None)
    ArgParser.add_argument('-c', '--cycle', nargs = '?', dest = "Cycle", help = "The cycle number in a multi-stage run.", default = None)

    args = ArgParser.parse_args()

    BuildNamelists(args.StageName, args.RestartDir, args.Run, args.Cycle)
