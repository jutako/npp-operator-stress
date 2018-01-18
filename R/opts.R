loadSettings <- function(basedir.project){

  # directories
  basedir.analysis <- file.path(basedir.project, 'analysis')
  basedir.data <- file.path(basedir.project, 'data')
  dir.events <- file.path(basedir.project, 'data','events')
  #dir.events.noldus <- file.path(basedir.project, 'data', 'noldus', '20160422')
  dir.events.noldus <- file.path(basedir.project, 'data', 'noldus', 'with_common_events')
  dir.questionnaires <- file.path(basedir.project, 'data','questionnaires')
  save.basedir <- file.path(basedir.project, 'analysis')

  #files
  mc.master.file  <- file.path(basedir.project, 'data','measurement_config_CORE2015.xlsx');
  file.stress.levels <- file.path(basedir.project, 'data','questionnaires',
                                  'CORE_stress-questionnaire_end-of-session.xlsx')


  # factor autocreation
  crew <- list(
    levels = sprintf('C%d', 0:6),
    labels = sprintf('C%d', 0:6)
  )
  part <- list(
    levels = sprintf('P%d', 0:6),
    labels = sprintf('P%d', 0:6)
  )
  task <- list(
    levels = c('pre-base','run1','mid-base','normal-run','run2','run3','post-base'),
    labels = c('PreBL','SCN1','MidBL','NO','SCN2','SCN3','PostBL')
  )
  role <- list(
    levels = c('SS','RO','TO','APO'),
    labels = c('VP','RO','TO','AO')
  )
  autofactors <- list(part = part,
                      crew = crew,
                      task = task,
                      role = role)

  # factors
  role <- list(
    levels = c('SS','RO','TO','APO'),
    labels = c('VP','RO','TO','AO')
  )
  strev <- list(
    levels = c('pre-base','run1','mid-base','normal-run','run2','run3','post-base'),
    labels = c('PreBL','SCN1','MidBL','NO','SCN2','SCN3','PostBL')
  )
  feat <- list(
    levels = c('obj.stress_max', 'self.stress_max',
               'meanhr_max','rmssd_min','pnnx50_mean',
               'lf_max','hf_min','lfhf_max',
               'scrNum', 'scrAmpl', 'scrAmplSum'),
    labels = c('instSTRESS', 'operSTRESS',
               'HR','RMSSD','PNN50',
               'LF','HF','LFHF',
               'scrNum', 'scrAmpl', 'scrAmplSum')
  )
  factors <- list(role = role,
                  strev = strev,
                  feat = feat)

  A4 <- list(width.in = 8.3, height.in = 11.7)

  OPTS <- list(basedir.project = basedir.project,
               basedir.data = basedir.data,
               basedir.analysis = basedir.analysis,
               dir.events = dir.events,
               dir.events.noldus = dir.events.noldus,
               dir.questionnaires = dir.questionnaires,
               file.stress.levels = file.stress.levels,
               mc.master.file = mc.master.file,
               save.basedir = save.basedir,
               factors = factors,
               autofactors = autofactors,
               A4 = A4)

  OPTS
}


# Settings for 'npp-stress' analysis.branch
# Frequently used:
# winlen =  c(240, 60)
# winovrl = c(180, 45)
#
# block.type can be one of ['blocks_event', 'blocks_general'],
# manuscript uses 'blocks_event' but continuous time series available
# only from 'blocks_general'. The latter does not set factor 'task'
# in any way!
load_settings_nppstress <- function(winlen, winovrl, OPTS,
                                   block.type = 'blocks_event'){

  # if (winlen==60 & winovrl == 45){
  #   # This branch creates the trig.data.file
  #   trig.data.file <- NULL
  # } else {
    trig.data.file = file.path(OPTS$basedir.analysis,
    'HRV_blocks_event/colibri_hrv_win60_ovrl45/npp-stress/data/triggerdata_blocks_event_win60_ovrl45_ibi_medianhr.rds')
  #}

  analysis.branch = 'npp-stress'
  # baselineTasks <- switch(block.type,
  #                         blocks_event = c('normal-run','post-base'),
  #                         blocks_general = 'whole_measurement')
  baselineTasks <- c('normal-run','post-base')
  exclude.subjects = c(15,17) #have medication

  branch.id = sprintf('colibri_hrv_win%d_ovrl%d_%s', winlen, winovrl, analysis.branch)
  dir.base = get.analysis.dir.core(OPTS, block.type, winlen, winovrl)
  dir.branch = get.analysis.branchdir(OPTS, block.type, winlen, winovrl, analysis.branch)

  # features to analyze
  # note: Using self.stress_max since tasks for which we would like to report
  # minimum value (normal-run and post-base) have only one question and hence
  # min = max and we use max for simplicity.
  nppstress.feat <- list(
    levels = c('obj.stress', 'self.stress_max',
               'ibi_medianhr','ibi_rmssd','ibi_stdev',
               'ibi_sdfrac','ibi_sd1','ibi_sd2',
               'ibi_lf','ibi_hf','ibi_lfhf','ibi_lf.nu','ibi_hf.nu',
               'Accelerometer_XYZ_mean', "Accelerometer_XYZ_sd",
               "ibi_triangular.index", "ibi_sampen",
               "ibi_vlf", "ibi_meanhr", "ibi_tinn"),
    labels = c('predSTRESS', 'repSTRESS',
               'HR','RMSSD','STD',
               'SD1/SD2','SD1','SD2',
               'LF','HF','LF/HF','LFnu','HFnu',
               'ACT','ACTsd',
               'TRIAGind','SAMPEN',
               'VLF','HRmean','TINN')
  )

  nppstress.feat.manu <- list(
    levels = c('obj.stress', 'self.stress_max',
               'ibi_medianhr','ibi_rmssd','ibi_stdev',
               'ibi_lf','ibi_hf','ibi_lfhf',
               'ibi_sd1','ibi_sd2','ibi_sdfrac',
               'Accelerometer_XYZ_mean'),
    labels = c('predSTRESS', 'repSTRESS',
               'HR','RMSSD','STD',
               'LF','HF','LF/HF',
               'SD1','SD2', 'SD1/SD2',
               'ACT')
  )

  nppstress.strev <- list(
    levels = c('pre-base','run1','mid-base','normal-run','run2','run3','post-base'),
    labels = c('PreBL','SCN1','MidBL','NO','SCN2','SCN3','PostBL'))


  nppstress <- list(
    block.type = block.type,
    winlen = winlen,
    winovrl = winovrl,
    analysis.branch = analysis.branch,
    branch.id = branch.id,
    baseline.tasks = baselineTasks,
    trig.data.file = trig.data.file,
    exclude.subjects = exclude.subjects,
    dir.base = dir.base,
    dir.basedata = file.path(dir.base, 'data'),
    dir.branch = dir.branch,
    dir.imdata = file.path(dir.branch, 'data'),
    dir.export = file.path(dir.branch, 'export'),
    dir.pubfig = file.path(dir.branch, 'pubfigs'),
    dir.fig = file.path(dir.branch, 'figs'),
    dir.restab = file.path(dir.branch, 'pubtabs'),
    factors = list (feat = nppstress.feat,
                    feat_manu = nppstress.feat.manu,
                    strev = nppstress.strev),
    colors.3.qualitative = c('#1b9e77','#d95f02','#7570b3')
  )

  # Create directories
  dir.lst.create(nppstress[grep('dir\\..*', names(nppstress) , value = T)])

  nppstress
}
