# Loading and preprocessing of features for the nppstress HRV manuscript
# Note:
# * has to be called as in nppstress_batch.R
# * this script does not create output directories, make sure the OPTS
#   setting function does (e.g., load_settings_nppstress() does).
#
# Inputs:
#   reload.colibri.data
#       if true, features are loaded from colibri result files (slow)
#       if false, same data are loaded from an existing rds file
#
# Set OPTS$nppstress like this:
# rm(list=ls())
# source("init.R")
# OPTS <- loadSettings()
# OPTS$nppstress <- load_settings_nppstress(60, 45, OPTS)
# OPTS$nppstress <- load_settings_nppstress(240, 180, OPTS)

# Note: Be careful to select a proper set of baseline tasks as "blocks_general"
# have their $task NA!
#
nppstress_preprocess_features <- function(OPTS,
                                         reload.colibri.data = T,
                                         compute_trigdata = T,
                                         subset.hrq90 = T){
  #  interactive debug:
  # reload.colibri.data <- T
  # compute_trigdata <- T
  # subset.hrq90 <- T

  require(data.table)

  #----------------------------------------------------------------------------
  ## Options
  #----------------------------------------------------------------------------
  blockmode <- OPTS$nppstress$block.type
  winlen <- OPTS$nppstress$winlen
  winovrl <- OPTS$nppstress$winovrl
  baselineTasks <- OPTS$nppstress$baseline.tasks
  trig.var <- 'ibi_medianhr'
  #trig.prc <- 0.9
  trig.data.file <- OPTS$nppstress$trig.data.file

  # directories
  dir.basedata <- OPTS$nppstress$dir.basedata
  dir.imdata <- OPTS$nppstress$dir.imdata
  dir.export <- OPTS$nppstress$dir.export
  colibri.dir = file.path(OPTS$nppstress$dir.basedata, 'colibri')


  #----------------------------------------------------------------------------
  ## Load data
  #----------------------------------------------------------------------------
  # Note: HRV data is missing from:
  #   * P3 post-base
  #   * P1 AO

  # P3 post-base not removed from stress data as inclusion seems not to mess things up.

  self.stress <- getStressLevelsAsFeatures(OPTS$file.stress.levels)
  self.stress$variable = 'self.stress'
  self.stress <- filter(self.stress, !(subject %in% c('CORE201504','CORE201507')))
  # subject 04 was never actually measured, 07 managed to skip the stress questionnaire

  obj.stress = as.tibble(getObjectiveStressAsFD(OPTS)) %>%
              mutate(variable = 'obj.stress') %>%
              filter( part %in% sprintf("P%d",1:6) ) %>% #to remove P0
              filter( !(subject %in% c('CORE201504')) )
  # subject 04 was never actually measured



  if (reload.colibri.data){
    #fdlst = getFeatureData(colibri.dir, OPTS,  regexp = ".*201501.*", ) #colibri branch: master

    # colibri branch: multisig
    fdlst = getFeatureData(colibri.dir, OPTS, signals = c('ibi', 'Accelerometer_XYZ'))
    #fdlst = getFeatureData(colibri.dir, OPTS, signals = c('ibi', 'Accelerometer_XYZ'), regexp = ".*20150[1,2].*")
    #fdlst = getFeatureData(colibri.dir, OPTS, signals = 'ibi', regexp = ".*201501.*")
    #fdlst = getFeatureData(colibri.dir, OPTS, signals = 'Accelerometer_XYZ', regexp = ".*201501.*")

    fdl <- fdlst$raw
    saveRDS(fdl,  file.path(dir.basedata, 'feature_data_long.rds'))
    write.table(fdl, file = file.path(dir.basedata, 'feature_data_long.csv'),
                sep = ';', row.names = F)
  } else {
    fdl <- readRDS(file.path(dir.basedata, 'feature_data_long.rds'))
  }


  #----------------------------------------------------------------------------
  ## Add factor $task to data that does not have it
  #----------------------------------------------------------------------------
  if (blockmode == 'blocks_general'){
    events <- readStructureEvents(OPTS)

    # add factor $task since it is heavily used
    fdl <-  fdl %>%
            group_by(part) %>%
            do(add_task(., events))
  }


  #----------------------------------------------------------------------------
  ## Add features
  #----------------------------------------------------------------------------
  # Some features are added here to get them seamlessly integrated

  # Compute sdfrac = sd1/sd2
  fdsub <-  fdl %>%
            filter(variable %in% c('ibi_sd1','ibi_sd2')) %>%
            spread(variable, value) %>%
            mutate(ibi_sdfrac = ibi_sd1 / ibi_sd2) %>%
            select(-ibi_sd1, -ibi_sd2) %>%
            gather(key = "variable", value = "value", ibi_sdfrac)

  fdl <- rbind(fdl, fdsub)
  rm(fdsub)


  #----------------------------------------------------------------------------
  ## Rescale data
  #----------------------------------------------------------------------------
  hrvd <- rescaleFeatureData(fdl,
                             baseTaskArr = baselineTasks,
                             baseMeasure = 'mean',
                             rescaleFun = rfdf_relative,
                             balanceTasks = T)
  saveRDS(hrvd,  file.path(dir.imdata, 'feature_data_long_scaled.rds'))

  self.stress <- rescaleStressCORE(self.stress,
                                   baseTaskArr = baselineTasks)
  obj.stress <- rescaleStressCORE(obj.stress,
                                  baseTaskArr = baselineTasks)

  # Check for missing values using tidy data
  # tmp <- tbl_df(tidyr::spread(hrvd, variable, value))
  # complete.match <- !is.na(tmp$meanhr)
  # sum(!complete.match)

  # Absolute change with respect to baseline (added 24.3.2017)
  hrvd_abs <- rescaleFeatureData(fdl,
                             baseTaskArr = baselineTasks,
                             baseMeasure = 'mean',
                             rescaleFun = rfdf_absolute,
                             balanceTasks = T)
  saveRDS(hrvd_abs,  file.path(dir.imdata, 'feature_data_long_abs-scaled.rds'))

  #----------------------------------------------------------------------------
  ## Find and save timestamps for "high/low extreme of trig.var"
  ## or load this information for later use
  #----------------------------------------------------------------------------
  # Note: The computation has been updated 9.11.2017 with the inclusion
  # of 10% percentile and which_extreme factor

  if (compute_trigdata){

    trgd_high <- plyr::ddply(subset(hrvd, variable == trig.var),
                             .(part, role, task),
                             function(x){get.quantile.obs(x, 'value', 0.9)})
    trgd_high$which_extreme <- 'high'

    trgd_low <- plyr::ddply(subset(hrvd, variable == trig.var),
                             .(part, role, task),
                             function(x){get.quantile.obs(x, 'value', 0.1)})
    trgd_low$which_extreme <- 'low'

    trig.data <- rbind(trgd_high, trgd_low)
    savename <- sprintf('triggerdata_%s_win%d_ovrl%d_%s.rds',
                        blockmode, winlen, winovrl, trig.var)
    saveRDS(trig.data, file.path(dir.imdata, savename))

    # Quality control
    #dplyr::arrange(trig.data, part, task, role)

    savedir <- file.path(dir.imdata,'trigQC')
    if (!dir.exists(savedir)){ dir.create(savedir, showWarnings = F, recursive = F) }

    # timing of events
    p <- ggplot(data = trig.data, mapping = aes(x = timestamp, y = value))
    p <- p + geom_hline(aes(yintercept = 0))
    p <- p + geom_point(aes(color = task))
    p <- p + facet_grid(which_extreme ~ part, scales = 'free_x')
    #p
    savename <- sprintf('triggertiming_%s.png', trig.var)
    ggsave(filename = file.path(savedir, savename),
           plot = p,
           units = 'mm', width = 400, height = 100)

    # value profiles
    p <- ggplot(data = trig.data,
                mapping = aes(x = task, y = value, color = role, group = role))
    p <- p + geom_line()
    p <- p + geom_point()
    p <- p + facet_grid(part ~ which_extreme)
    #p
    savename <- sprintf('triggerprofiles_%s.png', trig.var)
    ggsave(filename = file.path(savedir, savename),
           plot = p,
           units = 'mm', width = 300, height = 300)

  } else {
    trig.data <- readRDS(trig.data.file)
  }


  #----------------------------------------------------------------------------
  ## Subset all variables to the "90th quantile of HR median"
  #----------------------------------------------------------------------------
  # Note: Rest of the stuff in this function can be done only if task factor has
  # been set.

  if (subset.hrq90){

    # Note: This part of the analysis pipe has been changed on
    # 9.11.2017 to have different extreme values for different
    # tasks.
    hrvds <- subset_feature_data(hrvd, trig.data, OPTS$nppstress$winlen)

    saveRDS(hrvds, file.path(dir.imdata, 'HRV_data_long_scaled_triggersubset.rds'))
    write.table(hrvds,
                file = file.path(dir.imdata, 'HRV_data_long_scaled_triggersubset.csv'),
                sep = ';', row.names = F)

    # Sanity checks:
    fdst <- data.table::as.data.table(hrvds)
    # check that there is only one variable per part-role-task
    if ( max(unique(fdst[, .N, by = .(part, role, task, variable)]$N)) != 1 ){
      browser()
    }

    # check that there is only one unique timestamp per part-role-task
    # unique(fdst[, length(unique(timestamp)),
    #             by = .(part, role, task, variable)]$V1)

    # Check timestamp differences between data and trig.data
    # Suspiciously large differences are usually due to:
    # 1. there is only one segment in the whole block (normal-run)
    # 2. the trigger timestamp appears in last segment of the block (only whole
    #    segments are allowed in a block)
    # trig.data$trigts <- trig.data$timestamp
    # data.match <- hrvds$variable == "medianhr"
    # tmp <- dplyr::left_join(tbl_df(hrvds[data.match,c('part','task','role','timestamp')]),
    #                         tbl_df(trig.data[,c('part','task','role','trigts')]) )
    # tmp$timestamp <- tmp$timestamp - winlen/2
    # tmp$trigts <- tmp$trigts - 60/2
    # tmp$diff <- tmp$trigts - tmp$timestamp
    # unique(tmp$diff)
    # xtabs(~diff, data = tmp)
    # hist(as.numeric(tmp$diff))

    #----------------------------------------------------------------------------
    ## Aggregate self.stress
    #----------------------------------------------------------------------------
    na.rm = T
    self.stress.agg <- as_tibble(self.stress) %>%
                  select(part ,task, role, subject, variable, value) %>%
                  group_by(role, part, task, variable) %>%
                  summarise(n = n(),
                            min = min(value, na.rm = na.rm),
                            max = max(value, na.rm = na.rm)) %>%
                  gather("aggvar", "value", n:max) %>%
                  unite(variable, variable, aggvar, sep="_") %>%
                  mutate(timestamp = NA)

    # old code:
    # na.rm = T
    # self.stress.agg <- tbl_df(self.stress[,c('part','task','role','subject','variable','value')]) %>%
    #   dplyr::group_by(role, part, task, variable) %>%
    #   dplyr::summarise(n = n(),
    #                    min = min(value, na.rm = na.rm),
    #                    max = max(value, na.rm = na.rm))
    # self.stress.agg <- tidyr::gather(self.stress.agg, "aggvar", "value", 5:ncol(self.stress.agg))
    # self.stress.agg <- tidyr::unite(self.stress.agg, variable, variable, aggvar, sep="_")
    # self.stress.agg$timestamp <- NA

    #----------------------------------------------------------------------------
    ## Combine features and stress
    #----------------------------------------------------------------------------
    # fd = Feature Data
    # Note: self.stress.agg converted to data.frame as rbinding objects of differing
    # types does not work on all Linux systems consistently. (Problems at mylly.)
    fd <- rbind(dplyr::select(hrvds, role, part, task, timestamp, variable, value),
                dplyr::select(as.data.frame(self.stress.agg), role, part, task, timestamp, variable, value),
                dplyr::select(obj.stress, role, part, task, timestamp, variable, value))
    #fd$role <- droplevels(fd$role) #char, cannot drop...
    fd <- as_tibble(fd)
    fd$part <- as.character(fd$part) #todo: prevent variables from becoming factors!


    #----------------------------------------------------------------------------
    ## Exclude subjects that have medication
    #----------------------------------------------------------------------------
    # Add field subject temporarily to be able to filter easily:
    mcmeas <- as.tibble(getMeasConfMaster(OPTS$mc.master.file)$measurement)
    #mcmeas <- create_factors(mcmeas, OPTS)
    fd <- dplyr::left_join(fd, mcmeas[,c('part','role','subject')],
                           by = c('part','role'))
    exclude.sbj.arr <- sprintf('CORE2015%02d', OPTS$nppstress$exclude.subjects)
    fd <- fd %>%
          filter(!(subject %in% exclude.sbj.arr))

    saveRDS(fd,  file.path(dir.imdata, 'mergedfeatures_data_long.rds'))
    write.table(fd, file = file.path(dir.imdata, 'mergedfeatures_data_long.csv'),
                sep = ';', row.names = F)

    # Done later at experiments:
    # fdw <- spread(select(fd, -timestamp), variable, value)
    # write.table(fdw, file = file.path(dir.export, 'mergedfeatures_data_wide.csv'),
    #             sep = ';', row.names = F)



    #----------------------------------------------------------------------------
    ## Compute percentiles for each variable
    #----------------------------------------------------------------------------
    #csig <- 'Accelerometer_XYZ'
    #dplyr::filter(signal == csig) %>%

    fd <- readRDS(file.path(dir.imdata, 'feature_data_long_scaled.rds'))
    # Note: need to reload to get something to summarize on. Without reloading
    # a wrong subset of feature data is used.

    na.rm = T
    qtype = 3 #SAS definition, observed data point?
    fds <-  fd %>%
      dplyr::group_by(role, part, task, variable) %>%
      dplyr::summarise(n = n(),
                       min = ifelse(is.na(value[1]), NA,
                                    min(value, na.rm = na.rm)),
                       prc10 = quantile(value, 0.1, type = qtype, na.rm = na.rm),
                       prc25 = quantile(value, 0.25, type = qtype, na.rm = na.rm),
                       prc50 = quantile(value, 0.5, type = qtype, na.rm = na.rm),
                       prc75 = quantile(value, 0.75, type = qtype, na.rm = na.rm),
                       prc90 = quantile(value, 0.9, type = qtype, na.rm = na.rm),
                       max = ifelse(is.na(value[1]), NA,
                                    max(value, na.rm = na.rm)),
                       mean = mean(value, na.rm = na.rm)) %>%
      tidyr::gather("aggvar", "value", n:mean) %>%
      tidyr::unite(aggvariable, variable, aggvar, sep="_")
    # ifelse() used to avoid Inf -Inf resulting in value being NA and
    # getting omitted due to na.rm = T


    # Remove role-part-task combinations that do not contain HR data
    # Note: this is unnecessary for some analysis branches

    # differing timestamps between stress and HRV data
    # Such rows might exist because:
    # * obj.stress is defined also for subjects that were never measured
    # * post-base is missing for some crews
    # tmp <- tidyr::spread(fds, aggvariable, value)
    # complete.match <- !is.na(tmp$meanhr_mean)
    # #db tmp2 <- tmp[!complete.match,]
    # #db View(tmp2)
    # tmp <- tmp[complete.match,]
    # fds <- tidyr::gather(tmp, "aggvariable", "value", 4:ncol(tmp))
    # #db tmp3 <- xtabs(~ aggvariable + task, fds)
    # #db unique(tmp3)

    # Remove mid-base as there is no data for it
    fds <-  fds %>% filter(task != 'mid-base')
    fds$task <- droplevels(fds$task)

    fnamebody <- 'feature_percentiles_long'
    saveRDS(fds,  file.path(dir.imdata, sprintf('%s.rds', fnamebody)) )
    write.table(fds, file = file.path(dir.imdata, sprintf('%s.csv', fnamebody)),
                sep = ';', row.names = F)

  } # end of "if (subset.hrq90)"

} # end of function "nppstress_preprocess_features"
