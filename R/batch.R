rm(list = ls())
source("init.R")

RELOAD_COLIBRI_DATA <- F

start.time <- Sys.time()

#------------------------------------------------------------------------------------
# Analyze branch that sets "trigger csegs"
#------------------------------------------------------------------------------------
OPTS$nppstress <- load_settings_nppstress(60, 45, OPTS,
                                        block.type = 'blocks_event')

# nppstress_preprocess_features(OPTS, reload.colibri.data = RELOAD_COLIBRI_DATA,
#                               compute_trigdata = T,
#                               subset.hrq90 = T)
#
#source("experiments.R")



#------------------------------------------------------------------------------------
# Analyze branch to report
#------------------------------------------------------------------------------------
OPTS$nppstress <- load_settings_nppstress(240, 180, OPTS,
                                        block.type = 'blocks_event')

# nppstress_preprocess_features(OPTS, reload.colibri.data = RELOAD_COLIBRI_DATA,
#                              compute_trigdata = F, #very important to be FALSE!
#                              subset.hrq90 = T)
source("experiments.R")
source("experiments_abs.R")


