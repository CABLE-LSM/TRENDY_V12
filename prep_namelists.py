# Author: Lachlan Whyborn
# Last Modified: Mon 24 Jun 2024 08:35:58 PM AEST

import os

def merge_namelists(MasterNamelist: dict, UpdateNamelist: dict):
    """Recursively update the MasterDict with UpdateDict, with the combined result stored in place in MasterDict."""
    
    # Iterate through the values in the UpdateDict, with special handling for specific value types.
    for Key, Value in UpdateNamelist.items():
        if isinstance(Value, dict):
            # In the instance that the value is another dictionary we either:
            #   a) Add the new dictionary as a value in the MasterDict, if MasterDict does not already have that key
            #   b) RecursiveUpdate the existing value in MasterDict with the value from UpdateDict
            if Key in MasterNamelist.keys():
                merge_namelists(MasterNamelist[Key], Value)
            else:
                MasterNamelist[Key] = Value
        else:
            # In all other instances, we can just replace the value
            MasterNamelist[Key] = Value

def keyword_replace(Input: dict, ReplacementDict: dict):
    """Replace all instances of Target string in Input dictionary with Replacement string."""

    for Key, Value in Input.items():
        # We have 3 options:
        #   - The value is a string, so we do a simple replacement
        #   - The value is a dictionary, so we call this function on it
        #   - The value is anything else, in which case leave it
        if isinstance(Value, str):
            for Keyword, Replacement in ReplacementDict.items():
                Input[Key] = Input[Key].replace(Keyword, Replacement)
        elif isinstance(Value, dict):
            keyword_replace(Value, ReplacementDict)

def process_keywords(MasterNamelist: dict, PreviousStage: str, PreviousStep, Run: str):
    """Replace keywords denoted by <> with their desired values, built using RestartDir and Run."""

    # Currently, we have a hard-coded set of keywords to replace. In future, we may export this a user-modifiable
    # file. We do need to build a way of determining paths to outputs from multi-step stages. For now, act under
    # the assumption that the user will only want to directly refer to results from the final cycle of a multi-step
    # stage.
    ReplaceDict = {"<prevstage>": f"{PreviousStage}", "<prevstep>": f"{PreviousStep}", "<run>": f"{Run}", "<homedir>": os.getcwd()}
    keyword_replace(MasterNamelist, ReplaceDict)
