require(lubridate)
require(tidyr)
require(dplyr)
require(xlsx)
require(VennDiagram)
require(stringr)

#' Extracts filename without extension. For '/a/b.c' returns 'b'.
filebody <- function(filename){
  strsplit(basename(filename),'\\.')[[1]][1]
}


#' Saves a data.frame tbl to disk in pdf and csv formats
#' Use format() on tbl data to set rounding.
table2disk <- function(tbl, savepath, savenamebody,
                       pdf.height = 11,
                       pdf.width = 8.5){

  pdf(file.path(savepath, paste0(savenamebody,'.pdf')), height = pdf.height, width = pdf.width)
  gridExtra::grid.table(tbl)
  dev.off()

  write.table(tbl,
              file.path(savepath, paste0(savenamebody,'.csv')),
              sep = ';',
              row.names = F)
}


# Generate analysis basepath (canonical way)
get.analysis.basedir.core <- function(OPTS, blockmode){
  if (blockmode %in% c('blocks_general',
                       'blocks_event',
                       'blocks_mblno',
                       'blocks_noldus',
                       'blocks_prepost_w300',
                       'blocks_noldus_ajo2alk_cw600')){
    file.path(OPTS$basedir.analysis, sprintf('HRV_%s',blockmode))
  } else{
    stop(sprintf('Unknown value for blockmode: %s. Cannot proceed.', blockmode))
  }
}


# Generate analysis path (canonical way)
get.analysis.dir.core <- function(OPTS, blockmode, segment.length, segment.overlap){
  file.path(get.analysis.basedir.core(OPTS, blockmode),
            sprintf("colibri_hrv_win%d_ovrl%d", segment.length, segment.overlap))
}

get.analysis.branchdir <- function(OPTS, blockmode, segment.length, segment.overlap, analysis.branch){
  file.path(get.analysis.dir.core(OPTS, blockmode, segment.length, segment.overlap),
            analysis.branch)
}

# Generate other paths
get.analysis.datadir.core <- function(analysis.dir){
  file.path(analysis.dir, 'data')
}

get.analysis.figdir.core <- function(analysis.dir){
  file.path(analysis.dir, 'figs')
}


#' Create all directories in path.lst
dir.lst.create <- function(path.lst,
                           showWarnings = F,
                           recursive = T){
  for (path in path.lst){
    dir.create(path, showWarnings = showWarnings, recursive = recursive)
  }
}


# Subset and normalize data for plotting
makePlotData <- function(resdf, subjectArr, varArr, scaleData = T){

  pd <- subset(resdf, (subject %in% subjectArr) & (variable %in% varArr) )
  pd$variable <- droplevels(pd$variable)

  # to normalized units?
  if (scaleData){ pd <- ddply(pd, .(casename,variable), scaleDf) }

  pd
}


# Apply scale() with ddply() to a molten data.frame
scaleDf <- function(df, center = T, scale = T){
  #browser()
  df$value <- scale(df$value, center = center, scale = scale)
  df
}


# Shuffle and compute fun(x1, x2)
shuffleNfun <- function(x1, x2, fun){
  fun(sample(x1, length(x1), replace = F), x2)
}


## ggplot poincare
ggplot_poincare <- function(ibidf) {

  N <- nrow(ibidf)
  ibipd <- data.frame(xdata = ibidf$ibi[1:(N-1)],
                      ydata = ibidf$ibi[2:N],
                      t = ibidf$t[1:(N-1)])

  ## Determine the centre of mass
  xm <- mean(ibipd$xdata)
  ym <- mean(ibipd$ydata)

  ## Get the values for SD1 and SD2
  sd1 <- ibi_sd1(ibidf$ibi)
  sd2 <- ibi_sd2(ibidf$ibi)

  ## Calculate coordinates for the sd1 and sd2 lines
  sd2d  <- sqrt(2 * sd2^2 / sqrt(2))
  sd1d  <- sqrt(2 * sd1^2 / sqrt(2))

  x.sd2.1 <- xm - sd2d
  y.sd2.1 <- ym - sd2d
  x.sd2.2 <- xm + sd2d
  y.sd2.2 <- ym + sd2d

  x.sd1.1 <- xm - sd1d
  y.sd1.1 <- ym + sd1d
  x.sd1.2 <- xm + sd1d
  y.sd1.2 <- ym - sd1d

  ggplot(ibipd, aes(x=xdata, y=ydata)) + geom_point()

  #   plot(x, y, cex = 0.5, pch = 20)
  #   points(xm, ym, pch = 21, col = "black", bg = "red", lwd = 3, cex = 3)
  #
  #   segments(x.sd2.1, y.sd2.1, x.sd2.2, y.sd2.2, col = "red", lwd = 2)
  #   segments(x.sd1.1, y.sd1.1, x.sd1.2, y.sd1.2, col = "red", lwd = 2)
  #
  #   text(0.9*x.sd1.1, 1.1*y.sd1.1, "SD1", cex = 1, col = "red")
  #   text(1.1*x.sd2.2, 1.1*y.sd2.2, "SD2", cex = 1, col = "red")

  #require(plotrix)
  #draw.ellipse(x = xm, y = ym, a = (2 / sqrt(2))*sd1d, b = (2 / sqrt(2))*sd2d, angle = -45, border = "red", lwd = 3)
  ## abline(a = 0, b = 1, col = "blue")
}


# Plot any longish time series
core_plot_timeseries <- function(datavec, timevec){
    pd <- data.frame(value = datavec, time = timevec)
    p <- ggplot(pd, aes(x = time, y = value))
    p <- p + geom_point(size = 0.5)
    p <- p + geom_line(size = 0.1)
    p <- p + theme_light()
    p
  }


# Plot data from recording$signal
plot_colibri_timeseries <- function(recording, signalID){
  p <- core_plot_timeseries( recording$signal[[signalID]]$data,
                            recording$signal[[signalID]]$t)
  p <- p + labs(y = sprintf(recording$properties$casename))
  p
}


montage.plot.recording <- function(srcpath, destpath, rec.plot.fun,
                                 dirpattern = '*.rds'){
  # Get a list of Colibri recording files
  flst <- dir(srcpath, pattern = dirpattern)

  # Make one plot per recording
  for (i in seq_along(flst)){
    cat(sprintf("%d: %s\n",i, flst[[i]]))

    recfile <- file.path(srcpath, flst[[i]])
    recording <-  load_recording(recfile)

    p <- rec.plot.fun(recording)
    savefile <- file.path(savepath,sprintf('%02d_%s_subfig.png', i,
                                           strsplit(flst[[i]],'\\.')[[1]][[1]]))
    ggsave(filename = savefile, plot = p, units = 'mm',
           width = 100, height = 50)
  }

  # Combine into one large png
  sys.call <- sprintf("montage -tile 2x11 -geometry 600x300+10+10 %s*_subfig.png %smontage.pdf",
                      destpath, destpath)
  system(sys.call)
}


# Check how source data looks like
# inspect_signal <- function(recording, blockID, signalID){
#   require(lubridate)
#   require(dygraphs)
#
#   block <- subset(recording$conf$blocks, blockid == blockID)
#   tmp <- extract_segment_block(recording, blockid = blockID, signal = signalID)
#
#   op <- options(digits.secs = 3) #increase accuracy
#   tstmp <- lubridate::force_tz(recording$properties$zerotime, tzone='EET')
#   myxts <- xts::xts(x = tmp$data,
#                     order.by = tstmp + tmp$t)
#
#   myChart <-dygraph(myxts, main = signalID) %>% dyRangeSelector()
#   myChart
# }


# ----------------------------------------------------------------------------------
## Getting and setting data

# Read event files into R (somewhat obsolete)
# See readme.txt next to the event files.
readEvents <- function(evfile){
  df <- read.table(evfile, sep=";", as.is = T, header = T)
  df$timestamp <- lubridate::ymd_hms(df$timestamp, tz="UTC")
  df
}


# Old version of reading event files into R, set timestamps to correct time
# See readme.txt next to the event files.
readEventsObsolete <- function(evfile){
  #evfile <- '/ukko/projects/CORE_2015-2019/data/events/events_P0_pilot.csv'
  df <- read.table(evfile, sep=",", as.is=T)
  names(df) <- c("timestamp","event")

  df$timestamp <- lubridate::dmy_hms(df$timestamp, tz="UTC")
  wti <- lubridate::interval(ymd(20151025, tz = "UTC"),
                             ymd(20160329, tz = "UTC")) #wintertime interval
  if (df$timestamp[1] %within% wti){
    df$timestamp <- df$timestamp + 2*60*60 #add time difference
  } else {
    #summertime
    df$timestamp <- df$timestamp + 3*60*60 #add time difference
  }

  df
}


# Read measurement structure events
readStructureEvents <- function(OPTS){
  df <- read.xlsx2(OPTS$mc.master.file, sheetName = 'blocks_event',
                   stringsAsFactors = F)
  df$timestamp <- str_to_timestamp(df$starttime)
  df$starttime <- str_to_timestamp(df$starttime)
  df$stoptime <- str_to_timestamp(df$stoptime)
  df$event <- as.character(df$task)
  #df$task <- ordered(df$task, levels=OPTS$factors$strev$levels)
  df <- dplyr::select(df, blockid, part, timestamp, task, event, starttime, stoptime)
  df
}


# A wrapper to handle the fact that different sessions have their Noldus events in
# different locations
getNoldusEvents <- function(part){
  if (part %in% c('P3')){
    events <- readNoldusEvents(as.integer(substr(part,2,2)), OPTS,
                               srcdir = '/ukko/projects/CORE_2015-2019/data/noldus/20160428',
                               adjustTime = T)
  } else {
    events <- readNoldusEvents(as.integer(substr(part,2,2)), OPTS,
                               srcdir = OPTS$dir.events.noldus,
                               adjustTime = T)
  }
  events
}
# loaded in FIOH time


# Read Noldus events
readNoldusEvents <- function(cpart, OPTS,
                             srcdir = OPTS$dir.events.noldus,
                             adjustTime = T,
                             dataColumns = list(event='Comment', actor='Subject', behavior='Behavior')){
  #srcdir <- '/ukko/projects/CORE_2015-2019/data/noldus/20160422'
  #srcdir <- '/ukko/projects/CORE_2015-2019/data/noldus/20160428'
  #cpart <- 3
  cfile <- dir(srcdir,
               pattern = sprintf('vuoro%d.*txt', cpart))
  nd <- read.table(file.path(srcdir, cfile), header = T, sep = ";",
                   fileEncoding = 'UTF-16LE', stringsAsFactors = F)
  nd$timestampstr <- paste(nd$Date_dmy, trimws(nd$Time_Absolute_hms), sep='_')
  nd$timestamp <- lubridate::parse_date_time(nd$timestampstr, '%d-%m-%y_%H:%M:%S', tz = 'UTC')
  nd$part <- sprintf('P%d', cpart)

  keep.vars <- c('part','timestampstr','timestamp')

  # Rename columns according to specification
  for (colname in names(dataColumns)){
    if (dataColumns[[colname]] %in% names(nd)){
      names(nd) <- stringr::str_replace(names(nd), dataColumns[[colname]], colname)
      keep.vars <- c(keep.vars, colname)
    }
  }
  #nd <- dplyr::rename(nd, event = Comment)

  nd <- nd[, keep.vars]
  nd$noldus.time <- nd$timestamp

  strevents <- readStructureEvents(OPTS)
  tdiff <- subset(strevents, part == sprintf('P%d',cpart) & event == 'run2')$timestamp -
           subset(nd, event == 'ajo2')$timestamp
  if (length(tdiff) == 1){
    nd$timestamp <- nd$timestamp + tdiff
    nd$physio.time <- nd$timestamp
    nd$tdiff <- tdiff
  } else {
    stop()
  }

  # Save for reference
  fnbody <- strsplit(cfile,'\\.')[[1]][[1]]
  savefile <- file.path(OPTS$basedir.data, 'noldus', sprintf("%s_export.csv", fnbody))
  write.table(nd, file = savefile, row.names = F, sep = ";")

  attr(nd, 'tdiff') <- tdiff
  nd
}


## Read supervisor rated performance
# Note: all questions do not have subquestions!
readCrewPerformance <- function(OPTS){
  perf.file <- file.path(OPTS$dir.questionnaires, 'CORE_supervisor_rated_crew_performance.xlsx')
  d <- read.xlsx2(perf.file, sheetName = 'runs_combined',
                  colClasses = c('character', rep('numeric',36)))

  d <- tidyr::gather(d, 'item', 'rating', 2:37) #make long format
  all.items <- d
  d <- tidyr::separate(d, item, c('task','item')) #separate task and rating item

  #separate "main items" from "sub items"
  sq.match <- stringr::str_detect(d$item, 'sq')
  d.sq <- subset(d, sq.match)
  d.sq$subitem <- stringr::str_sub(d.sq$item, 3, -1) #sub item
  d.sq$item <- stringr::str_sub(d.sq$item, 1, 2) #main item of sub item

  d.q <- subset(d, !sq.match) #only main items

  list(main.item = d.q, sub.item = d.sq, all.items = all.items)
}


#' Instructor evaluation of the stressfullness of the tasks
getObjectiveStress <- function(){
  data.frame(task = c('pre-base','run1','normal-run','run2','run3','post-base'),
             obj.stress = c(1, 4.333, 1, 6.5, 6.67, 1),
             stringsAsFactors = F)
}


## Convert stress levels such that they can be plotted along with features
getObjectiveStressAsFD <- function(OPTS){
  obj.stress <- getObjectiveStress()
  mc <- getMeasConfMaster(OPTS$mc.master.file)

  tmp1 <- obj.stress$obj.stress
  names(tmp1) <- obj.stress$task
  osf <- cbind(mc$measurement, as.data.frame(t(tmp1)) )

  osf <- tidyr::gather(osf, task, obj.stress, ncol(mc$measurement)+(1:nrow(obj.stress)) )
  osf <- osf[,c('casename','subject','role','part','task','obj.stress')]
  #osf$task <- ordered(osf$task, levels=OPTS$factors$strev$levels)

  # add 'starttime' and 'stoptime'
  strevents <- readStructureEvents(OPTS) #from MC file
  tmp2 <- subset(strevents, event %in% unique(osf$task))

  osf <- dplyr::left_join(osf, tmp2[,c('part','task','starttime','stoptime')],
                             by=c('part','task'))

  osf$variable <- 'stresslevel'
  osf <- dplyr::rename(osf, value = obj.stress)

  # osf$role <- ordered(osf$role,
  #                     levels = c('SS','RO','TO','APO'),
  #                     labels = c('supervisor','reactor','turbine','aux.panel'))
  # osf$role <- ordered(osf$role,
  #                     levels = OPTS$factors$role$levels,
  #                     labels = OPTS$factors$role$labels)

  osf$timestamp <- osf$starttime
  osf
}


## Read stress level assessments
getStressLevels <- function(stressfile){
  df <- xlsx::read.xlsx(stressfile, sheetName='data',
                        colClasses = c('character', rep('numeric',10)),
                        stringsAsFactors = F)
  df <- tidyr::gather(df, task, stresslevel, -subject)
  df <- tidyr::separate(df, task, c("timeidx","task","subtask"), sep="_")
  df$timeidx <- gsub('X','',df$timeidx)
  df$task <- gsub('\\.','-',df$task) #undo modifications by read.xlsx
  #df$task <- ordered(df$task, levels = OPTS$factors$strev$levels)
  #df$task <- droplevels(df$task)
  df$timeidx <- as.numeric(df$timeidx)
  df
}
#sl <- getStressLevels(OPTS$file.stress.levels)


## Convert stress levels such that they can be plotted along with features
getStressLevelsAsFeatures <- function(stressfile){

  ## Load required data
  mc <- getMeasConfMaster(OPTS$mc.master.file)
  strevents <- readStructureEvents(OPTS) #from MC file
  stress <- getStressLevels(stressfile)

  ## Add factors
  # add 'part'
  stress <- dplyr::left_join(stress, mc$measurement[,c('subject','part')], by="subject")

  # add 'starttime' and 'stoptime'
  tmp <- subset(strevents, event %in% unique(stress$task))
  stress <- dplyr::left_join(stress, tmp[,c('part','task','starttime','stoptime')],
                             by=c('part','task'))
  #left_join would have made copies of rows in stress according to how many there are in strevents

  # add timestamp
  offset.df <- data.frame(timeidx        = c(1,   2,    3,   4,    5,   6,    7,    8,    9,    10),
                          offset.factor =  c(0.5, 0.25, 0.5, 0.75, 0.5, 0.33, 0.66, 0.33, 0.66, 0.5))
  stress <- dplyr::left_join(stress, offset.df, by="timeidx")
  stress$timestamp <- stress$starttime + stress$offset.factor*(stress$stoptime -  stress$starttime)

  stress$variable <- 'stresslevel'
  stress <- dplyr::rename(stress, value = stresslevel)

  stress$fulltask <- paste(stress$task, stress$subtask, sep='_')
  stress$fulltask <- ordered(stress$fulltask,
                             levels =  c("pre-base_qst","run1_tests","run1_problem","run1_opeations",
                                        "normal-run_run","run2_problem","run2_operations","run3_problem",
                                        "run3_operations","post-base_qst"))

  # add classifier 'role'
  mc$measurement$casename <- as.character(mc$measurement$casename) #to avoid warning by join

  stress <- dplyr::left_join(stress, mc$measurement[,c('subject','role')], by="subject")
  stress
}


#' Scale vector values to unit interval [0,1]
#' min(x) -> 0, max(x) -> 1
scale_to_unit_interval <- function(x, na.rm = T){
  (x-min(x, na.rm = na.rm)) / (max(x, na.rm = na.rm) - min(x, na.rm = na.rm))
}


#' Standard error
standard_error <- function (x, na.rm = T)
{
  sd(x, na.rm = na.rm)/sqrt(length(x))
}


#' Scale stress data to unit interval
rescaleStressUniInt <- function(stressdf){
  workfun <- function(df){
    df$value <- scale_to_unit_interval(df$value)
    df
  }
  stressdf <- ddply(stressdf, .(subject, variable), workfun)
  stressdf
}


#' Rescale stress values - a custom way
rescaleStressCORE <- function(stressdf,
                               baseTaskArr = c('normal-run'),
                               baseMeasure = 'mean'){

  # compute baselines
  bldf <- subset(stressdf, task %in% baseTaskArr) %>%
    dplyr::group_by(subject) %>%
    dplyr::summarise(n = n(), mean = mean(value),
                     min = min(value), max = max(value))

  # normalize to baseline
  relate_to_baseline <- function(df){
    match <- (bldf$subject %in% df$subject)
    if (sum(match) > 1){ browser() } #catch anomalies
    df$value <- 1 + (df$value - as.numeric(bldf[match, baseMeasure]))
    df
  }

  resdfr <- ddply(stressdf, .(subject), relate_to_baseline)
  attr(resdfr, 'baseline') <- bldf
  resdfr
}


#' Read features from several Colibri recording -files from disk
#' Output is a data frame with features
read_all_recordings <- function(basepath, signals, patext = "") {
  file_list <- list.files(basepath,
                          pattern = paste0(".*",patext,".rds"),
                          full.names = TRUE)

  cat(sprintf("Reading file %s ...\n", file_list[[1]]))
  rec <- load_recording(file_list[[1]])
  out <- collect_results(rec, signals = signals)

  for (f in file_list[-1]) {
    cat(sprintf("Reading file %s ...\n", f))
    rec <- load_recording(f)
    tmp <- collect_results(rec, signals = signals)
    out <- rbind(out, tmp)
  }
  out
}


getFeatureData <- function(srcdir, OPTS, signals = 'ibi', regexp = ''){
  # Read in feature data (this step takes some time...)
  resdf <- read_all_recordings(srcdir, signals = signals, regexp) #from CORE utils

  #resdf$task <- ordered(resdf$task, levels = OPTS$factors$strev$levels)
  # todo: setting task as a factor is a bad idea as it has different values in
  # different branches

  resdf <- tidyr::unite_(resdf, 'variable', c('signal','variable'))
  restdf <- dplyr::tbl_df(resdf)

  # compute baselines
  bldf <- subset(resdf, task %in% c('pre-base', 'post-base')) %>%
    dplyr::group_by(casename, variable) %>%
    dplyr::summarise(n=n(), mean=mean(value), median=median(value), sd=sd(value))
  saveRDS(bldf, file = file.path(dirname(srcdir), 'prepost_baseline_values.rds'))
  #head(bldf)

  # normalize to baseline
  relate_to_baseline <- function(df){
    match <- (bldf$casename %in% df$casename) & (bldf$variable %in% df$variable)
    if (sum(match) > 1){ browser() }
    df$value <- df$value / as.numeric(bldf[match,'mean'])
    df
  }
  resdfr <- ddply(resdf, .(casename, task, variable), relate_to_baseline)

  # average data for each subject
  resdfs <- resdfr %>%
    dplyr::group_by(casename, part, task, variable) %>%
    dplyr::summarise(n=n(), mean=mean(value), median=median(value), sd=sd(value), iqr=IQR(value, na.rm=T))

  # add classifier 'role'
  mc <- getMeasurementConfig('blocks_event', OPTS, autofactors = F)
  # todo: autofactors is a bad idea as e.g. task has different values in
  # different branches

  mc$measurement$casename <- as.character(mc$measurement$casename) #to avoid warning by join
  resdf <- dplyr::left_join(resdf, mc$measurement[,c('casename','role')], by="casename")
  resdfr <- dplyr::left_join(resdfr, mc$measurement[,c('casename','role')], by="casename")
  resdfs <- dplyr::left_join(resdfs, mc$measurement[,c('casename','role')], by="casename")

  list(raw = resdf, relative = resdfr, relative.sbj.ave = resdfs, srcdir = srcdir)
}


#' Rescale HRV feature data
#' To be used with getFeatureData()
#'
rescaleFeatureData <- function(resdf,
                               rescaleFun = rfdf_relative,
                               baseTaskArr = c('pre-base', 'post-base'),
                               baseMeasure = 'mean',
                               balanceTasks = T){

  # compute baselines
  if (baseTaskArr[[1]] == 'all'){
    # use whole measurement (e.g. when $task is not available)
    bldf <- resdf %>%
            dplyr::group_by(casename, variable) %>%
            dplyr::summarise(n = n(),
                              mean = mean(value, na.rm = T),
                              median = median(value, na.rm = T),
                              sd = sd(value, na.rm = T),
                              min = min(value, na.rm = T),
                              max = max(value, na.rm = T))
    resdfr <- ddply(resdf, .(casename, variable), rescaleFun, bldf = bldf, baseMeasure = baseMeasure)

  } else {

    if (balanceTasks){
      # aggregate first within task, between tasks
      bldf <- resdf %>%
        subset(task %in% baseTaskArr) %>%
        group_by(casename, variable, task) %>%
        summarise(n = n(),
                  trg_stat = mean(value, na.rm = T)) %>%
        group_by(casename, variable) %>%
        summarise(n = n(),
                  mean = mean(trg_stat, na.rm = T),
                  median = median(trg_stat, na.rm = T),
                  sd = sd(trg_stat, na.rm = T),
                  min = min(trg_stat, na.rm = T),
                  max = max(trg_stat, na.rm = T))

    } else {
      # pool tasks
      bldf <- resdf %>%
              subset(task %in% baseTaskArr) %>%
              group_by(casename, variable, task) %>%
              summarise(n = n(),
                         mean = mean(value, na.rm = T),
                         median = median(value, na.rm = T),
                         sd = sd(value, na.rm = T),
                         min = min(value, na.rm = T),
                         max = max(value, na.rm = T))
    }

    resdfr <- ddply(resdf, .(casename, task, variable), rescaleFun, bldf = bldf, baseMeasure = baseMeasure)
  }
  attr(resdfr, 'baseline') <- bldf
  resdfr
}

#' Relative normalization
rfdf_relative <- function(df, bldf, baseMeasure){
  match <- (bldf$casename %in% df$casename) & (bldf$variable %in% df$variable)
  if (sum(match) > 1){ browser() } #catch anomalies
  df$value <- df$value / as.numeric(bldf[match, baseMeasure])
  df
}

#' Absolute normalization
rfdf_absolute <- function(df, bldf, baseMeasure){
  match <- (bldf$casename %in% df$casename) & (bldf$variable %in% df$variable)
  if (sum(match) > 1){ browser() } #catch anomalies
  df$value <- df$value - as.numeric(bldf[match, baseMeasure])
  df
}


#' Variation partitioning for a three variable Venn diagram
#'
#' This function implements variation partitioning:
#' https://mb3is.megx.net/gustame/constrained-analyses/variation-partitioning
#'
#' The results of shared.variance.decomp() are combatible to those of
#' vegan::varpart().
#'
#' Example:
#' # create test data
#' cov_mat <- matrix(c(1  , 0.3, 0.5,
#'                     0.3, 1,   0.8,
#'                     0.5, 0.8, 1), nrow = 3, byrow = T)
#' d <- MASS::mvrnorm(n = 1e6, mu = rep(0,3), Sigma = cov_mat)
#'
#' # compute partitioning
#' sv <- shared.variance.decomp(d[,1], d[,2], d[,3])
#'
#' # pair-wise shared variations
#' cor(d)^2
#' (rsq <- cov_mat^2)
#'
#' # compare theoretical to actual
#' sv_ind <- 1
#' rsq[1,2]
#' sum(sv$areas[sv_ind,c(4,7)])
#'
#' rsq[1,3]
#' sum(sv$areas[sv_ind, c(6,7)])
#'
#' rsq[2,3]
#' sum(sv$areas[sv_ind, c(5,7)])
#'
#' sv$A %*% t(as.matrix(sv$areas[sv_ind,]))
#' sv$areas
#'
#' # compare to vegan::varpart()
#' (mod <- vegan::varpart(d[,3], d[,1], d[,2])) #use 1 and 2 to explain 3
#' plot(mod)
#'
#' # second row or $areas = using 1 and 2 to explain 3 to resolve area 1-2-3
#' mod$part$fract$R.squared[1]
#' sum(sv$areas[2, c(7,6)])
#'
#' mod$part$fract$R.squared[2]
#' sum(sv$areas[2, c(7,5)])
#'
#' mod$part$fract$R.squared[3]
#' sum(sv$areas[2, c(7,5,6)])
#'
shared.variance.decomp <- function(x1, x2, x3){

  # create a data.frame
  daf <- data.frame(x1 = x1, x2 = x2, x3 = x3)
  daf <- subset(daf, complete.cases(daf))

  rsq <- cor(daf)^2
  lm23.1 <- lm(x1 ~ x2 + x3, data = daf)
  lm12.3 <- lm(x3 ~ x1 + x2, data = daf)
  lm13.2 <- lm(x2 ~ x1 + x3, data = daf)

  # Equation to solve: A*areas = b
  A <-  matrix(c(0,0,0,1,0,0,1,
                 0,0,0,0,1,0,1,
                 0,0,0,0,0,1,1,

                 1,0,0,1,0,1,1,
                 0,1,0,1,1,0,1,
                 0,0,1,0,1,1,1,

                 0,0,0,1,0,1,1,
                 0,0,0,0,1,1,1,
                 0,0,0,1,1,0,1,

                 0,0,0,0,0,0,1), nrow = 10, byrow = T)
  # column names: A1, A2, A3, A12, A23, A13, A123,
  # where A referst to "area" and the numbers indicate which input variables
  # share the respective area

  b <- c(rsq[1,2], rsq[2,3], rsq[1,3],
         1, 1, 1,
         summary(lm23.1)$r.squared,
         summary(lm12.3)$r.squared,
         summary(lm13.2)$r.squared,
         min(rsq) )
  names(b) <- c('A12','A23','A13',
                'whole 1','whole 2','whole 3',
                'A23-1','A12-3','A13-2',
                'min-paired-R2')

  # Compute some possible solutions
  select_match_lst <- list(
    c(T,T,T, T,T,T, T,F,F, F),
    c(T,T,T, T,T,T, F,T,F, F),
    c(T,T,T, T,T,T, F,F,T, F),
    c(T,T,T, T,T,T, F,F,F, T)
  )
  areas <- matrix(NA, nrow = length(select_match_lst)+1, ncol = 7)
  for (i in 1:length(select_match_lst)){
    As <- A[ select_match_lst[[i]], ]
    bs <- b[ select_match_lst[[i]] ]
    areas[i,] <- solve(As, bs)
  }
  areas[5, ] <- as.vector(MASS::ginv(A) %*% b)
  areas <- as.data.frame(areas)
  names(areas) <- c('1','2','3','1-2','2-3','1-3','1-2-3')
  # same order as columns of A
  row.names(areas) <- c('23-1','12-3','13-2','min-paired-R2','ginv')

  # Solve the equation
  #areas <- solve(A, b) #applicable only if A is square!
  #areas <- as.vector(MASS::ginv(A) %*% b)
  #names(areas) <- c('1','2','3','1-2','2-3','1-3','1-2-3')
  # same order as columns of A

  list(A = A, b = b, areas = areas)
}


#' Venn diagram base on output of shared.variance.decomp
draw.triple.venn.core <- function(areas, varnames = c('x1','x2','x3')){
  areas[areas < 0] <- 0 #round negative elements to zero
  areas <- floor(areas * 100) #scale to full prc
  plot.new()
  VennDiagram::draw.triple.venn(
                  area1 = 100,
                  area2 = 100,
                  area3 = 100,
                  n12   = areas[4] + areas[7],
                  n23   = areas[5] + areas[7],
                  n13   = areas[6] + areas[7],
                  n123  = areas[7],
                  category = varnames,
                  euler.d = T,
                  scaled = F,
                  print.mode = 'raw')
}



#' Given a data.frame, column name and percentage value 'prc' returns
#' the row whose value in column is the 'prc':th quantile of all values in
#' that column.
#' Can be used to e.g. find timestamps of certain percentiles.
get.quantile.obs <- function(daf, variable, prc){
  if(prc < 0 | 1 < prc){stop('Input \'prc\' should be [0,1].')}
  prc.ind <- max(1, ceiling(nrow(daf) * prc))
  #using ceiling to select reasonably for very small datasets (e.g. 2 rows)
  daf <- daf[order(daf[[variable]]), , drop = F]
  daf[prc.ind,]
}


#' Find endpoints of a UNIMODAL density peak
#'
#' Operates on pdf type of data such as the output of stats::density()
#'
#' Example:
#'   set.seed(34)
#'   d <- rnorm(100)
#'   dens <- stats::density(d, n = 1000)
#'   pe <- peak_endpoints(dens$y)
#'   ep <- pe$endpoints
#'
#'   par(mfrow = c(1,1))
#'   plot(dens$x, dens$y)
#'   points(dens$x[ep], dens$y[ep], col = 'red', pch = 16)
#'   lines(range(dens$x), rep(pe$threshold,2), col = 'red')
peak_endpoints <- function(densv,
                           threshold = min(densv) +
                              0.001*diff(range(densv)) ){
  require(dplyr, stats)

  # sanity check
  if (all(densv < threshold)){
    stop('All density values below threshold, no peak to analyze.')
  }

  # Find peak and tails
  is_below_th <- densv < threshold
  peak_idx <- which.max(densv)

  # Determine left border
  left_border_idx <- dplyr::last( which(is_below_th[1:peak_idx]) )
  if (is.na(left_border_idx)){
    left_border_idx <- 1
  }

  # Determine right border
  right_border_idx <- peak_idx - 1 + dplyr::first( which(is_below_th[peak_idx:length(densv)]) )
  if (is.na(right_border_idx)){
    right_border_idx <- length(densv)
  }

  # Return
  list(endpoints = c(left_border_idx, right_border_idx),
       threshold = threshold)
}


#' Standardized density shape
#'
#' Produces a density profile of given length based on a density
#' estimate.
#' Two different types of shape available.
#'
density_shape <- function(dvec, length_out,
                          method = 'probth',
                          n_density = 1000,
                          ...){

  out <- switch(method,
         probth = density_shape_probth(dvec, length_out,
                                       n_density = n_density),
         center = density_shape_center(dvec, length_out,
                                       n_density = n_density, ...),
         cdfsdth = density_shape_cdfsdth(dvec, length_out))

  if (is.null(out)){
    stop('Unknown value for "method".')
  }
  out
}


# A equal probability threshold version of density shape
density_shape_probth <- function(dvec, length_out, n_density = 1000){

  dens <- stats::density(dvec, n = n_density)

  # Find equal probability threshold endpoints and define interval
  pe <- peak_endpoints(dens$y)
  ep <- pe$endpoints

  # Interpolate shape based on the interval
  at_points <- seq(dens$x[ep[1]], dens$x[ep[2]],
                   length.out = length_out)
  dens_apr <- stats::approx(dens$x, dens$y, xout = at_points)

  list(x = 1:length_out,
       y = dens_apr$y/sum(dens_apr$y))
}


# A centered version of density shape
density_shape_center <- function(dvec, length_out,
                                 n_density = 1000, rel_width = 0.5){

  dens <- stats::density(dvec, n = n_density)

  # Find mode and set interval around it
  mode_idx <- which.max(dens$y)
  ep <- mode_idx + c(-floor((rel_width/2) * length(dens$y)),
                      floor((rel_width/2) * length(dens$y)) )

  # Check for overflows
  if (ep[1] < 1) {
    ep[1] <- 1
    warning('Cannot create symmetric shape. Selecting index 1.')
  }
  if (length(dens$y) < ep[2]) {
    warning('Cannot create symmetric shape. Selecting largest possible index.')
    ep[2] <- length(dens$y)
  }

  # Interpolate shape
  at_points <- seq(dens$x[ep[1]], dens$x[ep[2]],
                   length.out = length_out)
  dens_apr <- stats::approx(dens$x, dens$y, xout = at_points)

  list(x = 1:length_out,
       y = dens_apr$y/sum(dens_apr$y))
}

# A cumulative density function shape using sd() based limits
density_shape_cdfsdth <- function(dvec, length_out, sdth = 3){

  # Find emprical cdf
  cdf <- stats::ecdf(dvec)

  # Find endpoints
  esd <- sd(dvec)
  value_range <- mean(dvec) + sdth * c(-esd, esd)
  at_points <- seq(value_range[[1]], value_range[[2]],
                   length.out = length_out)

  list(x = at_points,
       y = cdf(at_points))
}


#'Cohen's d from
#' http://stackoverflow.com/questions/15436702/estimate-cohens-d-for-effect-size
#'
cohens_d <- function(x, y) {
  lx <- sum(complete.cases(x)) - 1
  ly <- sum(complete.cases(y)) - 1
  md  <- abs(mean(x, na.rm = T) -
             mean(y, na.rm = T))        ## mean difference (numerator)
  csd <- lx * var(x, na.rm = T) + ly * var(y, na.rm = T)
  csd <- csd /(lx + ly)
  csd <- sqrt(csd)                     ## common sd computation

  cd  <- md/csd                        ## cohen's d
}
