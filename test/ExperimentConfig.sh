# Provide a description of the experiment here
# Describe each stage

# High-level configuration options
NRUNS=4                     # The number of serial runs
CREATE_LANDMASK=1           # Whether to build the landmask
CLEAN=0                     # Whether to empty the target repositories before running
RUN_FLAGS=(1 1 1 1 1 1 1)   # Which stages to run. The description of each stage
                            # should be included in the preamble of this file

# Options for the landmask creation
LANDMASK_FILE="path/to/landmask"    # Location of the landmask file
EXTENT="global"                     # Extent of the landmask- "global" or 
                                    # "<lon_min>,<lon_max>,<lat_min>,<lat_max>

# Options for the stages. This controls how many times to run each stage, the name of the configuration files for each stage and the possible components of each stage.
# The entries in RUN_FLAGS, REPEATS_PER_STAGE and STAGE_NAMES are correlated, and these
# arrays must be the same length.
REPEATS_PER_STAGE=(1 1 1 1 15 1 15)     # How many times to run each stage
STAGE_NAMES=("climate_spinup" "zero_biomass" "unrestricted_NP,analytic_unrestricted_NP" "restricted_NP,analytic_unrestricted_NP")

# The STAGE_NAMES controls both the names of the configuration files for each stage, and 
# the components of each stage. The stages are seperated by spaces, while sub-steps within a stage are separated by commas.
# For example, a stage named "unrestricted_NP,analytic_unrestricted_NP" means the stage
# has 2 components, named "unrestricted_NP" and "analytic_unrestricted_NP"

# If you want to start an experiment from existing data, you can set a previous directory
# to get the restart data from
RESTART_DATA=""
