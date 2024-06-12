#!/usr/bin/env python3

# Author: Lachlan Whyborn
# Last Modified: Fri 07 Jun 2024 15:19:20

import f90nml
import argparse
import yaml
import os
import re

def RecursiveUpdate(MasterDict, UpdateDict):
    """Recursively update nested dictionaries, with the combined result in MasterDict."""
    
    # Iterate through the values in the UpdateDict, with special handling for specific value types.
    for Key, Value in UpdateDict.items():
        if isinstance(Value, dict):
            # In the instance that the value is another dictionary we either:
            #   a) Add the new dictionary as a value in the MasterDict, if MasterDict does not already have that key
            #   b) RecursiveUpdate the existing value in MasterDict with the value from UpdateDict
            if Key in MasterDict.keys():
                RecursiveUpdate(MasterDict[Key], Value)
            else:
                MasterDict[Key] = Value
        else:
            # In all other instances, we can just replace the value
            MasterDict[Key] = Value


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
    """Replace all instances of Target in Input with Replacement."""

    for Key, Value in Input.items():
        # We have 3 options:
        #   - The value is a string, so we do a simple replacement
        #   - The value is a dictionary, so we call this function on it
        #   - The value is anything else, in which case leave it
        if isinstance(Value, str):
            Input[Key] = Value.replace(Target, Replacement)
        elif isinstance(Value, dict):
            KeywordReplace(Value, Target, Replacement)

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
    # The namelists in the stage configurations must match the names of the namelist files.
    # The current convention in CABLE is that the namelist in the file is the same name
    # as the filename appended with nml.
    # E.g. cable.nml contains the namelist &cablenml, cru.nml contains &crunml.
    # This allows us to use the namelist titles to grab the relevant master namelist.
    for NamelistTitle, StageNamelist in StageOptions.items():
        ReadFile = f"namelists/{NamelistTitle[:-3]}.nml"
        WriteFile = f"{TargetDir}/{NamelistTitle[:-3]}.nml"
        # Open the original namelists
        with open(ReadFile, 'r') as ReadFrom:
            # Read in the namelist
            MasterNamelist = f90nml.read(ReadFrom)

        # Unfortunately the patch() f90nml tool doesn't work in this instance, since it can't
        # selectively patch individual namelists within a larger namelist file. The intrinsic
        # dict.update() doesn't work either, because of the way f90nml handles derived types
        # as nested namelists. Instead, set up a recursive function to step through the nested
        # namelists.
        RecursiveUpdate(MasterNamelist[f"{NamelistTitle}"], StageNamelist)

        # Now go through and make any keyword substitutions
        KeywordReplace(MasterNamelist, "<restartdir>", f"{RestartDir}")
        KeywordReplace(MasterNamelist, "<run>", f"{Run}")
        KeywordReplace(MasterNamelist, "<homedir>", os.getcwd())
        # for Option, OptionValue in MasterNamelist.items():
            # # Unfortunately, the f90nml library handles struct values as nested namelists
            # # i.e. an entry "cable%option : <value>" would be represented as a sub namelist
            # # with entry {"cable: {"option: <value>"}}, rather than {"cable%option: <value}.
            # # So we need to apply a recursive replace function
            # # Only apply to string objects
            # MasterNamelist[Option] = KeywordReplace(OptionValue, "<restartdir>", f"{RestartDir}")
            # MasterNamelist[Option] = KeywordReplace(OptionValue, "<run>", f"{Run}")
            # MasterNamelist[Option] = KeywordReplace(OptionValue, "<homedir>", os.getcwd())

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
