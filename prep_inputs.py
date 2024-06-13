"""
Prepare the inputs to a simulation using symlinks
The intention is to have all the inputs in a central location
in the simulation directory.

The way we this is currently is by pre-defining a set of
namelists that pertain to input and restart files. These namelists
contain the names that each of the input/restart files will ALWAYS
be bound to. We use the user-defined namelists contained in
stage_configurations to identify the location of the desired inputs,
then symlink that file to the fixed input file locations.
"""

import os
import f90nml

def symlink_to_target(Target: str, LinkName: str):
    """Create a symlink at LinkName that points to Target."""

    # Attempt to create the symlink
    proc = subprocess.run(["ln", "-s", Target, LinkName])

    # Check the return code to ensure successful completion
    proc.check_returncode()

def symlink_inputs(StageNamelist: dict, InputsNamelist: dict):
    """Create symlinks between inputs defined in StageNamelist at the locations prescribed by InputsNamelist. Returns a copy of StageNamelist with the input entries changed as per InputsNamelist."""

    for Key, Value in InputsNamelist.items():
        if Key in FinalNamelist.keys():
            # Only perform the substitution if the entry exists. Unfortunately, CABLE often only checks that the string for a given input is non-empty when deciding whether to use a restart file, rather than checking a boolean option then checking for the file.
            if isinstance(Value, dict):
                # If the value is a dictionary (i.e. a nested type in the namelist),
                # recursively call the function.
                create_symlinks(FinalNamelist[Key], Value)
            else:
                # We're at a lowest level entry, build the symlink and modify the namelist
                symlink_to_target(FinalNamelist[Key], Value)
                StageNamelist[Key] = Value
