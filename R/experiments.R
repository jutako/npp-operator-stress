# This script is used to procude figures and tables for the
# Psychophysiology manuscript
#
# This script:
# * loads HRV and stress data
# * makes plots
# * makes result tables

# Note: Has to be called as in batch.R

require(ggplot2)
require(gridExtra)
require(VennDiagram)
require(dplyr)
require(tidyr)
require(stringr)
require(reshape2)

blockmode <- OPTS$nppstress$block.type
winlen <- OPTS$nppstress$winlen
winovrl <- OPTS$nppstress$winovrl

# Note: for debug runs use do.bootstrap = T and N_BOOT = 50 to save time. Too
# small bootstrap value result in an error in BCa correction...
do.bootstrap <- T #use TRUE only if you want to update bootstrap results
N_BOOT <- 50 #1000 or 50 for debug
# Notice: N_BOOT <- 1001 takes about 12 h to complete...

dir.imdata <- OPTS$nppstress$dir.imdata
dir.export <- OPTS$nppstress$dir.export
outdir.figs <- OPTS$nppstress$dir.pubfig#file.path(branch.dir, 'pubfigs')
outdir.rtb <- OPTS$nppstress$dir.restab#file.path(branch.dir, 'pubtabs')

aggvarArr <- OPTS$nppstress$factors$feat$levels
colors.three.qualitative <- OPTS$nppstress$colors.3.qualitative#c('#1b9e77','#d95f02','#7570b3')

A4_ASPR = 210 / 297 # A4 aspect ratio



#----------------------------------------------------------------------------
## Task durations
#----------------------------------------------------------------------------
# TODO: hard to make open without revealing too much about operators...

# task.savepath <- file.path(OPTS$basedir.analysis, 'events')
# dir.create(task.savepath, recursive = T, showWarnings = F)
#
# # Events stored to MC file
# strevents <- readStructureEvents(OPTS) #from MC file
# strevents$dur <- strevents$stoptime - strevents$starttime
# strevents <- dplyr::rename(strevents, crew = part)
#
# # Durations in table form
# evdur <- strevents[,c('crew','task','dur')]
# evdur <- set.plot.data.factors(evdur, OPTS$nppstress$factors)
# evdur$dur <- as.numeric(evdur$dur, units = 'mins')
# evdur$crew <- ordered(stringr::str_replace(evdur$crew, 'P', 'C'))
# evdur <- dplyr::tbl_df(evdur)
#
# # Statistics for manuscript
# evstat <- evdur %>%
#           group_by(task) %>%
#           summarize(n = n(), mean = mean(dur), sd = sd(dur),
#                     min = min(dur), max = max(dur))
# table2disk(evstat, task.savepath, 'task_durations_minutes_stats')
#
# # Individual durations
# evdur_wide <- tidyr::spread(evdur, task, dur)
#
# table2disk(evdur_wide, task.savepath, 'task_durations_minutes')
# table2disk(summary(evdur_wide), task.savepath, 'task_durations_summary',
#            pdf.height = 8.5, pdf.width = 11)
#
#
# # Durations as figure
#
# # What is going on?
# # strevents %>%
# #   dplyr::group_by(task) %>%
# #   dplyr::summarise(n = n(),
# #                   min = min(dur),
# #                   max = max(dur),
# #                   mean = mean(dur)) %>%
# #   View()
#
# ggp_evdur <- function(evd){
#   p <- ggplot(data = evd, aes(x = as.numeric(task), y = dur, color = crew, shape = crew))
#   p <- p + geom_line()
#   p <- p + geom_point(size = 3)
#   p <- p + scale_y_continuous(limits = c(0, 55))
#   p <- p + scale_x_continuous(breaks = 1:length(levels(evd$task)),
#                               labels = levels(evd$task))
#   p <- p + theme_light()
#   p <- p + labs(y = 'Duration (min)', x = 'Scenario')
#   p
# }
#
# pd <- set.plot.data.factors(strevents, OPTS$nppstress$factors)
# #print(ggp_evdur(pd))
# ggsave(filename = file.path(task.savepath, 'MCevents_durations.png'),
#        plot = ggp_evdur(pd))


#----------------------------------------------------------------------------
## Load feature data
#----------------------------------------------------------------------------
cat('\n\nLoading data: ... \n')

# Note: to be able to run this, run first:
# source("merge_data.R"), using the appropriate settings of course.

fdsl <- readRDS(file.path(dir.imdata, 'mergedfeatures_data_long.rds'))
fdsl <- dplyr::tbl_df(fdsl)
# Note: this data contains only the selected calculation segments, not all!

#mc <- getMeasConfMaster(OPTS$mc.master.file) #needed for excluded stuff

fdsw <- tidyr::spread(dplyr::select(fdsl, -timestamp), variable, value)
write.table(fdsw, file = file.path(dir.export, 'mergedfeatures_data_wide.csv'),
            sep = ';', row.names = F)


# Impute missing data for ANOVA
mean.imputation <- function(dvec){
  dvec[is.na(dvec)] = mean(dvec, na.rm=TRUE)
  dvec
}

# Apply for a tibble
mean.imputation.df <- function(daf, varArr){
  for (cvar in varArr){
    daf[,cvar] <- mean.imputation(daf[[cvar]])
  }
  daf
}

# Create ANOVA -dataset
# Replace NA values with the average value of other same role subjects from the
# same task. Only post-base has missing values for physiological features.
variableArr <- unique(fdsl$variable)
fd_anova <-   fdsl %>%
              select(-timestamp) %>%
              spread(variable, value) %>%
              group_by(task, role) %>% #group by role to retain some variability
              #summarise(n = n())
              do(mean.imputation.df(., variableArr)) %>%
              gather(key = 'variable', value = 'value', one_of(variableArr)) %>%
              spread(task, value) %>%
              arrange(variable, subject) %>%
              filter(!( (subject == 'CORE201507') &
                        (variable %in% c('self.stress_max','self.stress_min','self.stress_n')) ))

# Note: data is spread(first call to spread()) to make missing values "visible"
# as missing factor combinations do not explicitly appear in long format data
# However, this also creates self.stress* variables for subjects who
# did not answer the questionnaire and imputation populates these with values.
# The last filter removes the affected rows.

#fd_anova %>% View()


write.table(fd_anova,
            file = file.path(dir.export, 'mergedfeatures_data_anova.csv'),
            sep = ';', row.names = F)

# Without imputation:
# fdsw <- tidyr::spread(dplyr::select(fdsl, -timestamp), task, value) %>%
#         dplyr::arrange(variable, subject)
# write.table(fdsw, file = file.path(dir.export, 'mergedfeatures_data_anova.csv'),
#             sep = ';', row.names = F)


#----------------------------------------------------------------------------
## Test subjects
#----------------------------------------------------------------------------
# TODO: hard to make open without revealing too much about operators...

# # Subset to subjects actually included
# # Note: tricky since data has (role, part) but subject data only (subject, role) ->
# # hence the ugly data merging
# sbjtab <- dplyr::left_join(mc$subject[,c('subject','birthdate','hasMedication','role','weight','height')],
#                            mc$measurement[,c('subject','part')],
#                             by = "subject")
# sbjtab <- as_tibble(sbjtab) %>%
#           mutate(height = parse_number(height),
#                  weight = parse_number(weight),
#                  BMI = weight / (height/100)^2 )
#
# # subset sbjtab to subjects from which we have data in fdsl
# tmp <-  fdsl %>%
#         group_by(role, part) %>%
#         summarise(n = n())
# sbjtab <- semi_join(sbjtab, tmp, by = c('role','part')) %>%
#           arrange(subject)
# rm(tmp)
#
# # Compute subject ages on a specific date
# today <- as.Date('03.12.2015', "%d.%m.%Y")
# sbjtab$age <- as.numeric(today - sbjtab$birthdate, units = 'days')/365
#
# # Store on disk
# # List subjects
# table2disk(sbjtab, outdir.rtb, 'included_subjects')
# # Summary statistics for the manuscript
# tmp.tb <- summary(sbjtab[,c('age','height','weight','BMI')])
# table2disk(tmp.tb, outdir.rtb, 'subject_statistics')
# rm(tmp.tb)
#
#
# # Summarise
# tb <- sbjtab[,c('age','height','weight','BMI')]
# tb <- tidyr::gather(tb, 'variable', 'value', 1:4)
# tb <- subset(tb, !is.na(value))
# sbjsumm <-  tb %>%
#             group_by(variable) %>%
#             summarise(  n = n(),
#                         mean = mean(value, na.rm = T),
#                         sd = sd(value, na.rm = T),
#                         min = min(value, na.rm = T),
#                         max = max(value, na.rm = T))
#
# sbjsumm <- format(sbjsumm, digits = 4)
#
# table2disk(sbjsumm, outdir.rtb, 'subject_statistics2')



#----------------------------------------------------------------------------
## Time series plot: task on horizontal axis, with butterfly
#----------------------------------------------------------------------------
# Figure 2 in physchophysiology_2016 manuscript
cat('\n\nPlotting time series - option B: ... \n')

pd1 <- set.plot.data.factors(fdsl, OPTS$nppstress$factors)
# Note: For factor levels not listed in second arguments NA factor levels are introduced.
#pd1 <- fdsl
pd1$gvar <- paste(pd1$role, pd1$part, pd1$variable, sep = "_")


fdsls <- fdsl[!is.na(fdsl$value),] %>%
  dplyr::group_by(task, variable) %>%
  dplyr::summarise(n = n(),
                   min = min(value, na.rm=T),
                   max = max(value, na.rm=T),
                   mean = mean(value, na.rm=T),
                   sd = sd(value, na.rm=T),
                   se = standard_error(value, na.rm=T))
pd2 <- set.plot.data.factors(fdsls, OPTS$nppstress$factors)




#---------------------------------------------
# manuscript "time series butterfly" -figure

w.in <- 10
h.in <- 1.2*w.in
w.px <- 800
h.px <- 1.5*w.px

varset <- OPTS$nppstress$factors$feat_manu$levels
fvarset <- map.values(varset,
                      OPTS$nppstress$factors$feat_manu$levels,
                      OPTS$nppstress$factors$feat_manu$labels)
colset = c(rep(rgb(0/255, 68/255, 130/255),2),
           rep(rgb(49/255, 49/255, 149/255), length(varset)-2))
letset <- paste0(letters[1:length(varset)],')')
plst <- make.plot.set(fvarset, colset, letset, pd2, pd1, NULL)
# note: 'letset' not used by make.plot.set() anymore, passing it just for compatibility
gtlst <- adjust_gtables(plst)

h.mm <- 220
w.mm <- A4_ASPR * h.mm
cp <- cowplot::plot_grid(plotlist = gtlst, nrow = 3, ncol = 4,
                         labels = letset)

savefile <- file.path(outdir.figs, 'mean_feature_values_butterfly.pdf')
cowplot::ggsave(savefile, cp, width = h.mm, height = w.mm, units = 'mm')

savefile <- file.path(outdir.figs, 'mean_feature_values_butterfly.eps')
cowplot::ggsave(savefile, cp, width = h.mm, height = w.mm, units = 'mm',
                device = cairo_ps)
# note: cairo_ps needed to draw transparent objects

savefile <- file.path(outdir.figs, 'mean_feature_values_butterfly.png')
png(filename = savefile, units = 'px', width = w.px, height = h.px)
do.call(grid.arrange, c(gtlst, list(ncol = 2)) )
#render_plot(plst2)
#do.call(grid.arrange, c(plst2, list(ncol = 1)) )
dev.off()

# cleanup
rm(pd1, pd2, fdsls, plst, gtlst, savefile)



#----------------------------------------------------------------------------
## Similarity measures (correlation) of task patterns
#----------------------------------------------------------------------------
# pubfig: Fig xx
cat('\n\nComputing similarity measures: ... \n')

# Compute correlations
#Note: errors occur if NA values have been removed from fdsl

# Compute similarity using simfun as measure
ddply.compute.similarity <- function(daf, stressdf, simfun){
  daf <- dplyr::arrange(daf, part, role, task)
  stresstmp <- dplyr::filter(stressdf, (role == unique(daf$role)) &
                               (part == unique(daf$part)) &
                               (task %in% unique(daf$task)) )
  daf <- inner_join(daf, stresstmp, by = 'task')

  #if ( is.na(simfun(daf$value.x, daf$value.y)) ){ browser() }

  data.frame(n = nrow(daf),
             cor = simfun(daf$value.x, daf$value.y))
}

crd1 <- ddply(fdsl, .(role, part, variable), ddply.compute.similarity,
              stressdf = dplyr::filter(fdsl, variable == 'obj.stress'),
              simfun = cor)
crd1$stressvar <- 'predSTRESS'
# here some NA's come from odd nnx values and missing self stress reports



crd2 <- ddply(fdsl, .(role, part, variable), ddply.compute.similarity,
              stressdf = dplyr::filter(fdsl, variable == 'self.stress_max'),
              simfun = cor)
crd2$stressvar <- 'repSTRESS'
# here some NA's come from missing self reports of stress

crd3 <- ddply(fdsl, .(role, part, variable), ddply.compute.similarity,
              stressdf = dplyr::filter(fdsl, variable == 'Accelerometer_XYZ_mean'),
              simfun = cor)
crd3$stressvar <- 'ACCmeanXYZ'
# here some NA's come from odd nnx values and missing self stress reports

crd <- dplyr::tbl_df(rbind(crd1, crd2, crd3))


# ----------------------------------------------------------------------------
# Visualize
crd_plot <- dplyr::tbl_df(rbind(crd1, crd2, crd3))
crd_plot$stressvar <- ordered(crd_plot$stressvar,
                         levels = c('predSTRESS','repSTRESS','ACCmeanXYZ'),
                         labels = c('predSTRESS','repSTRESS','ACCmeanXYZ'))
plotVarArr <- setdiff(OPTS$nppstress$factors$feat_manu$levels,
                      c('obj.stress','self.stress_max','Accelerometer_XYZ_mean'))


pd <- dplyr::filter(crd_plot, (variable %in% plotVarArr) &
                         (stressvar %in% c('predSTRESS','repSTRESS')))
pd$variable <- ordered(as.character(pd$variable),
                       levels = OPTS$nppstress$factors$feat_manu$levels,
                       labels = OPTS$nppstress$factors$feat_manu$labels)

## One version with facet_wrap()
plot_corbox <- function(pd){
  p <- ggplot(data = pd)
  p <- p + geom_boxplot(aes(x = stressvar, y = cor))
  p <- p + geom_jitter(aes(x = stressvar, y = cor), alpha = 0.2,
                       position = position_jitter(height = 0, width = 0.25))
  p <- p + geom_hline(aes(yintercept = 0), color = 'red', size = 1)
  p <- p + facet_wrap(~ variable, nrow = 2)
  p <- p + scale_y_continuous(limits = c(-1,1))
  p <- p + theme_light()
  #p <- p + theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))
  p <- p + labs(x = 'STRESS EVALUATION', y = 'CORRELATION COEFFICIENT')
  p
}

p <-  plot_corbox(pd) +
      labs(y = '')

pd2 <- crd_plot %>%
      filter((variable == 'self.stress_max') &
             (stressvar == c('predSTRESS'))) %>%
      mutate(variable = ordered(variable,
                                levels = OPTS$nppstress$factors$feat_manu$levels,
                                labels = OPTS$nppstress$factors$feat_manu$labels))
p2 <- plot_corbox(pd2) +
      labs(x='')


# w.mm <- 280 #240
# h.mm <- (2/3)*w.mm
h.mm <- 290
w.mm <- A4_ASPR * h.mm
cp <- cowplot::plot_grid(p2, p, nrow = 1, rel_widths = c(1, 5))

savefile <- file.path(outdir.figs, 'cor_distributions.pdf')
cowplot::ggsave(savefile, cp, width = h.mm, height = w.mm, units = 'mm')

savefile <- file.path(outdir.figs, 'cor_distributions.eps')
cowplot::ggsave(savefile, cp, width = h.mm, height = w.mm, units = 'mm',
                device = cairo_ps)

rm(pd2, p, p2, cp, savefile)


## Another version with cowplot arrangement

# no facet_wrap
plot_corbox_onefeat <- function(pd){
  p <- ggplot(data = pd)
  p <- p + geom_boxplot(aes(x = stressvar, y = cor))
  p <- p + geom_jitter(aes(x = stressvar, y = cor), alpha = 0.2,
                       position = position_jitter(height = 0, width = 0.25))
  p <- p + geom_hline(aes(yintercept = 0), color = 'red', size = 1)
  p <- p + facet_grid(.~variable)
  p <- p + scale_y_continuous(limits = c(-1,1))
  p <- p + theme_light()
  #p <- p + theme(axis.text.x = element_text(angle = -45, hjust = 0, vjust = 1))
  #p <- p + labs(x = 'STRESS EVALUATION', y = 'CORRELATION COEFFICIENT')
  p <- p + labs(x = '', y = '')
  p
}
#plot_corbox_onefeat(filter(pd, variable == 'HR'))

varset <- setdiff(levels(pd$variable),c('predSTRESS','repSTRESS','ACT'))
plst <- list()
for (i in seq_along(varset)){
  p <- plot_corbox_onefeat(filter(pd, variable == varset[[i]]))
  plst <- c(plst, list(p))
}

pd2 <-  crd_plot %>%
        filter((variable == 'self.stress_max') &
                 (stressvar == c('predSTRESS'))) %>%
        mutate(variable = ordered(variable,
                                  levels = OPTS$nppstress$factors$feat_manu$levels,
                            labels = OPTS$nppstress$factors$feat_manu$labels))
p2 <- plot_corbox(pd2) +
      labs(x='', y = '')

h.mm <- 290
w.mm <- A4_ASPR * h.mm
cp <- cowplot::plot_grid(plotlist = c(plst, list(p2)), nrow = 2)

savefile <- file.path(outdir.figs, 'cor_distributions2.pdf')
cowplot::ggsave(savefile, cp, width = h.mm, height = w.mm, units = 'mm')

savefile <- file.path(outdir.figs, 'cor_distributions2.eps')
cowplot::ggsave(savefile, cp, width = h.mm, height = w.mm, units = 'mm',
                device = cairo_ps)

rm(pd, pd2, p, p2, cp, savefile, crd_plot)


# ----------------------------------------------------------------------------
# Result table
ct <-   crd %>%
        group_by(variable, stressvar) %>%
        summarise(n = n(),
                   mean = mean(cor, na.rm = T),
                   sd = sd(cor, na.rm = T),
                   se = standard_error(cor, na.rm = T) ) %>%
        ungroup()


#ct <- set.plot.data.factors(ct, OPTS$nppstress$factors)
ct <- arrange(ct, stressvar, variable)
options(digits = 2)
filter(ct, stressvar == 'repSTRESS') %>% select(c('variable','mean','sd'))
filter(ct, stressvar == 'predSTRESS') %>% select(c('variable','mean','sd'))


# TODO: this is not actually used?
# # Test significance using T-test
#
# # Compute t-test for a subset of data 'daf'
# workfun <- function(daf){
#   tr <- t.test(daf$predSTRESS, daf$repSTRESS, paired = T, na.rm = T)
#   data.frame( n = nrow(daf),
#               t = tr$statistic,
#               df = tr$parameter,
#               p.value = tr$p.value,
#               method = tr$method,
#               alternative = tr$alternative)
# }
#
# res <- ddply(tidyr::spread(crd, stressvar, cor), .(variable), workfun)
# subset(res, (res$p.value < 0.05) &
#          !(variable %in% c('obj.stress_max','self.stress_max')) )
# rm(workfun)


# Bootstrap mean correlation values to get 95% CI
# For a raw data bootstrap, see below.
library(boot)

# function to obtain mean of data
dmean <- function(data, indices) {
  d <- data[indices] # allows boot to select sample
  mean(d, na.rm = T)
}

# Do bootstrap for a subset of data 'daf'
workfun <- function(daf){

  all_equal <-  near(daf$cor, daf$cor[1]) %>%
                all()
  all_near_one <- near(abs(daf$cor),1) %>%
                  all()
  if ( all_equal | all_near_one ){
    # all correlations close to 1 or equal -> cannot compute boot()
    data.frame(bca.low = NA,
               bca.up  = NA)
  } else {
    results <- boot(data = daf$cor,
                    statistic = dmean,
                    R = N_BOOT)

    # Non-parametric bootstrap confidence intervals
    bci <- boot.ci(results, type="bca")
    # note: might end up in error if N_BOOT = 10...
    data.frame(bca.low = bci$bca[4],
               bca.up  = bci$bca[5])
  }
}


bci.res <- ungroup(crd) %>%
            filter(!is.na(cor)) %>%
            group_by(stressvar, variable) %>%
            do(workfun(.)) %>%
            ungroup()
rm(workfun)

corr.res <- dplyr::left_join(ct, bci.res, by = c("stressvar", "variable")) %>%
            arrange(stressvar, variable)
corr.res

# options(digits = 2)
# subset(bci.res, stressvar == 'repSTRESS')
# subset(bci.res, stressvar == 'predSTRESS')


table2disk(corr.res, outdir.rtb, 'correlations')
# pdf(file.path(outdir.rtb, 'correlations.pdf'), height=11, width=8.5)
# gridExtra::grid.table(corr.res)
# dev.off()



#----------------------------------------------------------------------------
## Shuffle feature data to get NULL model for mean correlation
#----------------------------------------------------------------------------
# pubfig: Fig xx
cat('\n\nBootstrapping correlations: ... \n')


# Define a special similarity function:
# This defines the null distribution: shuffle time for variable x1
# which will be the physiology variable
shuffleNcor <- function(x1, x2){ cor(sample(x1, length(x1), replace = F), x2) }


# A faster version
cat('\n\nShuffling data to get mean correlation NULL: ... \n')

# Compute shuffled correlations
# Idea:
# 1. spread to get a tibble with variables in columns
# 2. apply shuffling to easch subject-variable pair separately

# Compute for a single subject
myboot_ss <- function(tb, cvar, svar, B){
  # br <- tibble(b = 1:100) %>%
  #       group_by(b) %>%
  #       do( tibble(cor = shuffleNcor(tb[[cvar]], tb[[svar]])) )
  # br
  # => this was soooo slow

  br <- tibble(b = 1:B, cor = NA, variable = cvar)
  for (i in 1:nrow(br)){
    br$cor[[i]] <- shuffleNcor(tb[[cvar]], tb[[svar]])
  }
  br
  # => this was reasonably fast
}

# Compute for a group of subjects
myboot <- function(fdw, cvar, svar, B){
  fdw %>%
    group_by(subject) %>%
    do(myboot_ss(., cvar, svar, B = B)) %>%
    ungroup() #to tidy up the output
}


set.seed(42) #to get reproducibility
if (do.bootstrap){

  start.time <- Sys.time()

  # Actual computation call
  var_arr <- unique(fdsl$variable)

  fdsw <- fdsl %>%
    select(-timestamp) %>%
    spread(variable, value) %>%
    filter( !(is.na(ibi_medianhr) | is.na(self.stress_max)) )
  #remove P3 post-base and CORE201507 (who did not answer stress qst)


  bootres1 <- ldply(var_arr,
                   function(el){myboot(fdsw, el, 'obj.stress', B = 1000)})
  bootres_mean1 <-  as_tibble(bootres1) %>%
                    group_by(variable, b) %>%
                    summarise(cor.n = n(), cor.mean = mean(cor))  %>%
                    mutate(stressvar = 'predSTRESS')

  bootres2 <- ldply(var_arr,
                    function(el){myboot(fdsw, el, 'self.stress_max', B = 1000)})
  bootres_mean2 <-  as_tibble(bootres2) %>%
                    group_by(variable, b) %>%
                    summarise(cor.n = n(), cor.mean = mean(cor))  %>%
                    mutate(stressvar = 'repSTRESS')

  bootres3 <- ldply(var_arr,
                    function(el){myboot(fdsw, el, 'Accelerometer_XYZ_mean', B = 1000)})
  bootres_mean3 <-  as_tibble(bootres3) %>%
                    group_by(variable, b) %>%
                    summarise(cor.n = n(), cor.mean = mean(cor)) %>%
                    mutate(stressvar = 'ACCmeanXYZ')

  cor_boot_mean <- bind_rows(bootres_mean1, bootres_mean2, bootres_mean3)

  stop.time <- Sys.time()
  stop.time - start.time

  saveRDS(cor_boot_mean, file.path(outdir.figs,'cor_bootstrap_data.rds'))
  saveRDS(bind_rows(bootres1, bootres2, bootres3),
          file.path(outdir.figs,'cor_bootstrap_data_raw.rds'))

  rm(fdsw, var_arr)

} else {
  cor_boot_mean <- readRDS(file.path(outdir.figs,'cor_bootstrap_data.rds'))
}


#----------------------------------------------------------------------------
## Compare shuffled and observed data
#----------------------------------------------------------------------------
cat('\n\nVisualizing results: ... \n')

crds <- crd %>%
  dplyr::group_by(stressvar, variable) %>%
  dplyr::summarise(n = n(), cor.mean = mean(cor, na.rm = T))


## Plot data in original units
#plotVarArr <- c('meanhr', 'lfhf')
plotVarArr <- OPTS$nppstress$factors$feat$levels
stressvar.arr <- c('repSTRESS', 'predSTRESS')

for (i in 1:length(stressvar.arr)){
  svar <- stressvar.arr[i]

  p_list = list()
  for (cvar in plotVarArr){
    # Shuffled null model mean correlations
    pd <- dplyr::filter(cor_boot_mean, variable == cvar & stressvar == svar )
    #pd <- set.plot.data.factors(pd, OPTS)
    bs.mean <- mean(pd$cor.mean)
    bs.sd <- sd(pd$cor.mean)
    pd$z <- (pd$cor.mean - bs.mean)/bs.sd

    # Observed mean correlations
    pd2 <- dplyr::filter(crds, variable == cvar & stressvar == svar)
    #pd2 <- set.plot.data.factors(pd2, OPTS)
    pd2$z <- (pd2$cor.mean - bs.mean)/bs.sd

    # Compare
    p <- ggplot(data = pd)
    p <- p + geom_histogram(aes(x = cor.mean), binwidth = 0.02)
    p <- p + geom_vline(data = pd2,
                        aes(xintercept = cor.mean),
                        color = "red")
    p <- p + labs(title = sprintf('%s', cvar),
                  x = 'mean correlation')
    p <- p + scale_x_continuous(limits = c(-1,1))
    p <- p + theme_light()
    p_list <- c(p_list, list(p))
  }

  savefile <- file.path(outdir.figs,
                        sprintf('meancorr_bootnull-vs-observed_%s.png',
                                stringr::str_replace(svar, '/', '') ))
  #png(savefile, width = 400, height = 200)
  png(savefile, width = 15, height = 30, units = 'cm', res = 200)
  do.call(grid.arrange, c(p_list, nrow = floor(length(plotVarArr)/2)) )
  dev.off()

  rm(svar, p_list, savefile)
}
rm(crds)



#---------------------------------------------------------
# Make result table
#---------------------------------------------------------
cat('\n\nMean correlation result tables: ... \n')

aggvarArr <- OPTS$nppstress$factors$feat_manu$levels

bsd <- readRDS(file.path(outdir.figs,'cor_bootstrap_data.rds'))
bsd <- dplyr::filter(bsd, (variable %in% aggvarArr))

crds <- crd %>%
  dplyr::group_by(stressvar, variable) %>%
  dplyr::summarise(n = n(), cor.mean = mean(cor, na.rm = T))


# Compute obs.z and empirical.p for each feature-stress variable pair
# To be used as part of a split-apply-combine -strategy
# bsdf: simulated correlation means
# crds: observed correlation means
workfun <- function(bsdf, crds){

  # define z-scores for simulated means
  bsd.mean <- mean(bsdf$cor.mean, na.rm = T)
  bsd.sd <- sd(bsdf$cor.mean, na.rm = T)
  bsdf$z <- (bsdf$cor.mean - bsd.mean)/bsd.sd

  # select observed mean (scalar)
  obs.mean <- dplyr::filter(crds,
                    (variable == as.character(unique(bsdf$variable))) &
                    (stressvar == as.character(unique(bsdf$stressvar))) )$cor.mean

  # two-sided empirical p-value based on simulations:
  # empirical.p <- (n.more.extreme + 1)/(B + 1)
  # using absolute z-scores for this

  B <- nrow(bsdf)
  data.frame(
    B.boot = B,
    obs.z = abs((obs.mean - bsd.mean) / bsd.sd),
    empirical.p = (sum(abs(bsdf$z) >= abs((obs.mean - bsd.mean)/bsd.sd)) + 1) /(B + 1)
  )
}

bsc.res <- ddply(bsd, .(stressvar, variable), workfun, crds = crds) %>%
            arrange(stressvar, variable)
rm(workfun)
corr.res2 <-  left_join(corr.res, bsc.res, by = c("stressvar", "variable")) %>%
              arrange(stressvar, variable)

saveRDS(corr.res2,
        file.path(outdir.rtb, 'correlations_boot.rds'))

tmp_tb <- corr.res2
tmp_tb$empirical.p <- format(corr.res2$empirical.p
                               , big.mark = ','
                               , digits = 4
                               , format = 'f') #just one column to higher accuracy
table2disk(tmp_tb, outdir.rtb, 'correlations_boot', pdf.height=11, pdf.width=11)
rm(tmp_tb)


# -------------------------------------------------------------------
# Make a publication ready table of correlations
#
# Saving in latex booktabs style was not easy ->
# see also correlation_table.Rmd for the final table

# Explanations of columns:
# * bci (bootstrap confidence interval)
#   Nonparametric bootstrap confidence intervals for for observed mean correlation
#   using B = N_BOOT and BCa correction.
#   using boot::boot.ci()

# * z (z-score)
#   (obs.corr - mean(sim.corr))/sd(sim.corr)

# * obs.z
#   abs( (obs.corr - mean(sim.corr))/sd(sim.corr) ) i.e. a measure of how exceptional
#   the obs.corr value is measured in units of simulated mean correlation sd()

# * sig
#   empirical.p <- (n.more.extreme + 1)/(B + 1)
#   computed based on abs(z.score) thus two tailed version

stats_arr <- c('mean','sd','bca.low','bca.up','obs.z','empirical.p')
var_arr <- OPTS$nppstress$factors$feat_manu$levels

# interpret p-value as string
pval2str <- function(pval){
  if (0.05 < pval){
    'ns.'
  } else if (0.01 < pval) {
    '*'
  } else if (0.001 < pval) {
    '**'
  } else {
    '***'
  }
}

# vectorized version
pval2str_vec <- function(pval_arr){
  sapply(pval_arr, pval2str)
}


corr.res3 <-  corr.res2 %>%
              filter(stressvar %in% c('predSTRESS','repSTRESS'), variable %in% var_arr) %>%
              mutate(variable = ordered(as.character(variable),
                                        levels = OPTS$nppstress$factors$feat_manu$levels,
                                        labels = OPTS$nppstress$factors$feat_manu$labels)) %>%
              select(stressvar, variable, one_of(stats_arr)) %>%
              gather(key = stat, value = value, one_of(stats_arr)) %>%
              mutate(value = formatC(value, digits = 2, width = 3, format = 'f')) %>%
              unite('stressvar_stat', stressvar, stat) %>%
              spread(stressvar_stat, value) %>%
              mutate(repSTRESS_sig = pval2str_vec(repSTRESS_empirical.p),
                     predSTRESS_sig = pval2str_vec(predSTRESS_empirical.p),
                     repSTRESS_meansd = paste0(repSTRESS_mean, ' / ', repSTRESS_sd),
                     predSTRESS_meansd = paste0(predSTRESS_mean, ' / ', predSTRESS_sd),
                     repSTRESS_bci = paste0('[',repSTRESS_bca.low, ', ', repSTRESS_bca.up,']'),
                     predSTRESS_bci = paste0('[',predSTRESS_bca.low, ', ', predSTRESS_bca.up,']')) %>%
              select(variable,
                      repSTRESS_meansd, repSTRESS_bci, repSTRESS_obs.z, repSTRESS_sig,
                      predSTRESS_meansd, predSTRESS_bci, predSTRESS_obs.z, predSTRESS_sig)

corr.res3 %>% View()

# require(xtable)
# tb <- xtable(corr.res3)
# View(print(tb, type="html"))

# require(kable)
# kable(corr.res3)

# require(pander)
# pander(iris)

# require(knitr)
# require(kableExtra)
# kbt <- kable(corr.res3,
#               col.names = c('Feature',
#                     'mean +/- SD','bootstrap CI','z-score','sig',
#                     'mean +/- SD','bootstrap CI','z-score','sig'),
#               align = c('l', rep('c',8)),
#               format = 'latex',
#               booktabs = T,
#               caption = 'Table xx')  %>%
#       add_header_above(c(" ", "Self reported stress" = 4, "Predicted stress" = 4)) %>%
#       kable_styling(latex_options = c("repeat_header"))
#
# kable_as_image(kbt, filename = 'test', file_format = "png", keep_pdf = T)



#----------------------------------------------------------------------------
## Shared variance Venn: <physio> - repSTRESS - predSTRTESS
#----------------------------------------------------------------------------
# pubfig: made using EulerAPE and Inkscape based on R data
cat('\n\nShared variance: physi - repSTRESS - predSTRESS ... \n')

# Prepare data
fdv <- dplyr::select(fdsl, -timestamp)
fdv <- fdv[complete.cases(fdv$value),]

# Scale participant time series to set mean to zero and variances equal
wf <- function(daf){
  daf$value <- scale(daf$value)
  daf
}
fdv <- ddply(fdv, .(role, part, variable), wf)

# Spread for easy comparison
fdvw <- tidyr::spread(fdv, variable, value)

outdir.sharedvar <- file.path(outdir.figs, 'shared_variance')
dir.create(outdir.sharedvar)
outdir.eulerape <- file.path(outdir.sharedvar, 'eulerAPE')
dir.create(outdir.eulerape)

feat_df <- as.data.frame(OPTS$nppstress$factors$feat)
rownames(feat_df) <- feat_df$levels

for (cvar in aggvarArr){
  #db: cvar <- aggvarArr[[5]]

  # Compute shared variance
  sv <- shared.variance.decomp(fdvw[,cvar],
                               fdvw$self.stress_max,
                               fdvw$obj.stress)
  cat(sprintf('feature: %s, stress intersection (args 2 and 3): %f \n',
              cvar, sum(sv$areas[1,c(5,7)])))
  cur_areas <- as.numeric(sv$areas[1,]) #select version: x2 and x3 explaining x1
  cur_areas[cur_areas < 0] <- 0 #round negative elements to zero
  cur_areas <- floor(cur_areas * 100) #scale to full prc

  # Save shared variance values for EulerAPE
  file <- file.path(outdir.eulerape, sprintf('%s.els', cvar))
  x <- matrix(c(cur_areas[1:4],
                cur_areas[6],
                cur_areas[5],
                cur_areas[7]), nrow = 1)
  x[x == 0] <- 0.001 #eulerAPE does not like zeros...
  write.table(x, file,
              row.names = F,
              col.names =  c('//a','b','c','ab','ac','bc','abc'),
              #col.names = c(cvar, 'repSTRESS', 'predSTRESS', 'ab', 'ac', 'bc', 'abc'),
              sep = "|",
              quote = F)

  # Save non-proportional version of Venn
  saveNameBody <- sprintf('shared_variance_%s',
                          stringr::str_replace(cvar, '/', ''))
  png(file.path(outdir.sharedvar, paste0(saveNameBody, '.png')),
      width = 600, height = 600)

  plot.new()
  VennDiagram::draw.triple.venn(
    area1 = 100,
    area2 = 100,
    area3 = 100,
    n12   = cur_areas[4] + cur_areas[7],
    n23   = cur_areas[5] + cur_areas[7],
    n13   = cur_areas[6] + cur_areas[7],
    n123  = cur_areas[7],
    category = c(as.character(filter(feat_df, levels == cvar)$labels),
                 'repSTRESS','predSTRESS'),
    euler.d = T,
    scaled = F,
    print.mode = 'raw',
    cex = 2.5,
    cat.cex = 2.5,
    cat.pos = 0)

  dev.off()
}


#----------------------------------------------------------------------------
## Shared variance Venn: HR - ACC - stress
#----------------------------------------------------------------------------
# pubfig: made using EulerAPE and Inkscape based on R data
cat('\n\nShared variance: HR - ACC - stress ... \n')

varArrLst <- list( c('ibi_medianhr','Accelerometer_XYZ_mean','self.stress_max'),
                   c('ibi_medianhr','Accelerometer_XYZ_mean','obj.stress') )
#todo: maybe add STD and SDfrac?

# Note: used fdvw from previous block

for (i in 1:length(varArrLst) ){
  curID <- paste(varArrLst[[i]], collapse = '-')

  # Compute shared variance
  sv <- shared.variance.decomp(fdvw[ ,varArrLst[[i]][[1]] ],
                               fdvw[ ,varArrLst[[i]][[2]] ],
                               fdvw[ ,varArrLst[[i]][[3]] ])
  cur_areas <- as.numeric(sv$areas[1,]) #select version: x2 and x3 explaining x1
  cur_areas[cur_areas < 0] <- 0 #round negative elements to zero
  cur_areas <- floor(cur_areas * 100) #scale to full prc

  # Save shared variance values for EulerAPE
  file <- file.path(outdir.eulerape, sprintf('%s.els', curID))
  x <- matrix(c(cur_areas[1:4],
                cur_areas[6],
                cur_areas[5],
                cur_areas[7]), nrow = 1)
  x[x == 0] <- 0.001 #eulerAPE does not like zeros...
  write.table(x, file,
              row.names = F,
              col.names =  c('//a','b','c','ab','ac','bc','abc'),
              sep = "|",
              quote = F)

  # Save non-proportional version of Venn
  saveNameBody <- sprintf('shared_variance_%s',
                          stringr::str_replace(curID, '/', ''))
  png(file.path(outdir.sharedvar, paste0(saveNameBody, '.png')),
      width = 600, height = 600)

  plot.new()
  VennDiagram::draw.triple.venn(
    area1 = 100,
    area2 = 100,
    area3 = 100,
    n12   = cur_areas[4] + cur_areas[7],
    n23   = cur_areas[5] + cur_areas[7],
    n13   = cur_areas[6] + cur_areas[7],
    n123  = cur_areas[7],
    category = feat_df[varArrLst[[i]],'labels'],
    euler.d = T,
    scaled = F,
    print.mode = 'raw',
    cex = 2.5,
    cat.cex = 2.5,
    cat.pos = 0)

  dev.off()
}
