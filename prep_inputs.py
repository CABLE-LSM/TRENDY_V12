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
import subprocess

def symlink_to_target(Target: str, LinkName: str):
    """Create a symlink at LinkName that points to Target."""

    # Attempt to create the symlink
    proc = subprocess.run(["ln", "-sf", Target, LinkName])

    # Check the return code to ensure successful completion
    proc.check_returncode()

def symlink_suffix(Target: str, InputGlob: str):
    """Handle inputs that are set in the namelist with a prefix and the internals set a series of suffixes."""
    # We know that the Target will refer to a series of files, matching the given prefix.
    FileList = glob(Target + "*")

    # To build the symlinks, we need to capture the suffixes from each of the files in the list
    for File in FileList:
        FileSuffix = re.search(Target + "(.*)", File).group(1)
        symlink_to_target(Target + FileSuffix, re.sub("<suffix>", FileSuffix, InputGlob))

    # Now we set the correct namelist option for the input
    return InputGlob.replace("<suffix>", "")

def symlink_glob(Target: str, InputGlob: str):
    """Handle inputs that are globbed in a different fashion to the suffix inputs, which at the moment is just the met inputs."""
    # Get the list of files matching the glob, replacing any <> keywords with *
    FileList = glob(re.sub("(<.*?>)", "*", Target))

    # For each of the files, we're just going to split the files into the path and the file, and append the file from the Target
    # to the path from the InputGlob
    for File in FileList:
        TargetFilePath, TargetFileName = os.path.split(File)
        InputFilePath, InputFileName = os.path.split(File)
        symlink_to_target(File, os.path.join(InputFilePath, TargetFileName))

    # Set the correct namelist option for the input
    _, TargetFileName = os.path.split(Target)
    InputFilePath, _ = os.path.split(InputGlob)
    return os.path.join(TargetFileName, InputFilePath)
    
def symlink_inputs(StageNamelist: dict, InputsNamelist: dict, Prefix: str):
    """Create symlinks between inputs defined in StageNamelist at the locations prescribed by InputsNamelist. Returns a copy of StageNamelist with the input entries changed as per InputsNamelist."""

    for Key, Value in InputsNamelist.items():
        if Key in StageNamelist.keys():
            # Only perform the substitution if the entry exists. Unfortunately, CABLE often only checks that the string for a given input is non-empty when deciding whether to use a restart file, rather than checking a boolean option then checking for the file.
            if isinstance(Value, dict):
                # If the value is a dictionary (i.e. a nested type in the namelist),
                # recursively call the function.
                symlink_inputs(StageNamelist[Key], Value, prefix)
            elif isinstance(Value, str):
                if StageNamelist[Key]:
                    # Only perform the action if the original string is not empty
                    # Unfortunately, there are some options in the namelists which provide prefixes for a series of input/output files.
                    # This makes symlinking to them not a trivial exercise.
                    # The temporary measure for handling this will be to use keywords in the prescribed inputs namelist, denoted using <>
                    if bool(re.search(r"(<suffix>)", Value)):
                        # The instance where we specify the suffix in the input namelist
                        StageNamelist[Key] = symlink_suffix(StageNamelist[Key], os.path.join(Prefix, Value))
                    elif bool(re.search(r"(<.*?>)", Value)):
                        # Instances where there was a user namelist options, most likely <startdate> and <enddate>
                        StageNamelist[Key] = symlink_glob(StageNamelist[Key], os.path.join(Prefix, Value))
                    else:
                        symlink_to_target(StageNamelist[Key], os.path.join(Prefix, Value))
                        StageNamelist[Key] = Value
            else:
                # Should only be string or dict
                raise ValueError("Input option is not a string")

