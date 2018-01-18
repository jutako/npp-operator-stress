## This script needs to be only run once
# It is included in the repository to clearly show how the data has been modified.


## Main analysis branch
OPTS$nppstress <- load_settings_nppstress(240, 180, OPTS,
                                          block.type = 'blocks_event')
dir.imdata <- OPTS$nppstress$dir.imdata

# Stress data
dfile <- file.path(dir.imdata, 'self_stress.rds')
self.stress <- readRDS(dfile)
self.stress$value <- rnorm(nrow(self.stress))
saveRDS(self.stress, dfile)

# Relative baseline corrected data
dfile <- file.path(dir.imdata, 'mergedfeatures_data_long.rds')
fdsl <- readRDS(dfile)
fdsl$value <- rnorm(nrow(fdsl))
saveRDS(fdsl, dfile)


# Absolute baseline corrected data
dfile <- file.path(dir.imdata, 'feature_data_long_abs-scaled.rds')
fdla <- readRDS(dfile)

fdla$value <- rnorm(nrow(fdla))

bld <- attr(fdla, 'baseline')
bld <- bld %>%
        ungroup %>%
        mutate(mean = rnorm(nrow(bld)),
               median = rnorm(nrow(bld)),
               sd = rnorm(nrow(bld)),
               min = rnorm(nrow(bld)),
               max = rnorm(nrow(bld)) )
attr(fdla, 'baseline') <- bld

saveRDS(fdla, dfile)



## "time range selector" analysis branch
OPTS$nppstress <- load_settings_nppstress(60, 45, OPTS,
                                          block.type = 'blocks_event')
dir.imdata <- OPTS$nppstress$dir.imdata

# Trigger calculation segment data
dfile <- file.path(dir.imdata, 'triggerdata_blocks_event_win60_ovrl45_ibi_medianhr.rds')
fdsl <- readRDS(dfile)
fdsl$value <- rnorm(nrow(fdsl))
saveRDS(fdsl, dfile)
