# Author: Lachlan Whyborn
# Last Modified: Thu 13 Jun 2024 03:35:13 PM AEST

def merge_namelists(MasterDict: dict, UpdateDict: dict):
    """Recursively update the MasterDict with UpdateDict, with the combined result stored in place in MasterDict."""
    
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

def keyword_replace(Input: dict, Target: str, Replacement: str):
    """Replace all instances of Target string in Input dictionary with Replacement string."""

    for Key, Value in Input.items():
        # We have 3 options:
        #   - The value is a string, so we do a simple replacement
        #   - The value is a dictionary, so we call this function on it
        #   - The value is anything else, in which case leave it
        if isinstance(Value, str):
            Input[Key] = Value.replace(Target, Replacement)
        elif isinstance(Value, dict):
            KeywordReplace(Value, Target, Replacement)
