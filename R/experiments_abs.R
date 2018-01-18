# A hack to make some analyses using absolute difference from baseline
# The data used in this script differs from the rest of the nppstress data only
# in how the baseline correction is made. Here rfdf_absolute() is used instead
# of rfdf_relative().

# Note: Assumes that nppstress_batch.m has been run before this.
#source("init.R")
OPTS$nppstress <- load_settings_nppstress(240, 180, OPTS,
                                       block.type = 'blocks_event')

# todo: run header of nppstress_batch.R first to get things initiated
require(dplyr)


#--------------------------------------------------------------------------
## Load data
cat('\n\nLoading data: ... \n')

dir.imdata <- OPTS$nppstress$dir.imdata
fdla <- readRDS(file.path(dir.imdata, 'feature_data_long_abs-scaled.rds'))
bld <- attr(fdla, 'baseline')
fdla <- tibble::as_tibble(fdla)

# debug:
# tmp <- subset(fdla, (casename %in% 'CORE201505_P2_M1') &
#                     (task %in% c('normal-run','post-base')) &
#                     (variable %in% 'ibi_medianhr'))
# mean(tmp$value)


#--------------------------------------------------------------------------
## Exclude subjects
sbj.arr <- sprintf('CORE2015%02d',
                   setdiff(1:23, c(4,OPTS$nppstress$exclude.subjects)) )
# 4 is not measured at all!
fdla <- subset(fdla, subject %in% sbj.arr)

bld$subject <- sapply(bld$casename, function(x){strsplit(x,'_')[[1]][1]})
bld <- subset(bld, subject %in% sbj.arr)


#----------------------------------------------------------------------------
## Subset all variables to the "90th quantile of HR median"

trig.data.file <- OPTS$nppstress$trig.data.file
trig.data <- readRDS(trig.data.file)

fdlas <- subset_feature_data(fdla, trig.data, OPTS$nppstress$winlen)
fdlas <- as_tibble(fdlas)

# Run some checks:
fdst <- data.table::as.data.table(fdlas)

# check that there is only one variable per part-role-task
unique(fdst[, .N, by = .(part, role, task, variable)]$N)

# check that there is only one unique timestamp per part-role-task
unique(fdst[, length(unique(timestamp)),
            by = .(part, role, task, variable)]$V1)


#----------------------------------------------------------------------------
## Subset all variables to the "maximum value within task"
# Can be used to compare with halden abstracts.

# fdlas <-  fdla %>%
#           group_by(task, subject, variable) %>%
#           summarise(n=length(value), avg=mean(value), max=max(value))


#--------------------------------------------------------------------------
## Compute group averages: first group means then difference
cat('\n\nComputing group averages ... \n')

# average over subjects
rtb <-  fdlas %>%
        group_by(variable, task) %>%
        summarise(n=n(), mean_vchg=mean(value))

rtbb <- bld %>%
        group_by(variable) %>%
        summarise(n=length(mean), mean=mean(mean))

## Create simplified result table
cvar <- 'ibi_medianhr'
rtbs <- subset(rtb, variable==cvar)
rtbs$mean_base_value <- subset(rtbb, variable==cvar)$mean
rtbs$prc_of_base <- 100*(rtbs$mean_vchg / rtbs$mean_base_value)
rtbs$abs_value_mean <- rtbs$mean_base_value + rtbs$mean_vchg
print(as.data.frame(rtbs), digits = 4)
table2disk(rtbs, OPTS$nppstress$dir.restab,
           'avg-abs-change_HR-median_groupave-then-diff')


#--------------------------------------------------------------------------
## Compute group averages: first differences then group means
cat('\n\nComputing group averages - another way ... \n')

# Note: This approach and the one above produce the same results, apart from
# post-base where rtbb$mean contains too many values to average over.
fdlas <-  fdlas %>%
          select(casename, task, variable, value)

cvar <- 'ibi_medianhr'
test <- left_join(select(fdlas, casename, task, variable, value),
                  select(bld, casename, variable, mean),
                  by = c('casename','variable')) %>%
        mutate(chg_prc = (value / mean) * 100,
               abs_value = mean + value) %>%
        group_by(task, variable) %>%
        summarise(n = n(),
                  abs_value_mean = mean(abs_value),
                  chg_mean = mean(value),
                  chg_prc_mean = mean(chg_prc)) %>%
        filter(variable == cvar)

table2disk(test, OPTS$nppstress$dir.restab,
           'avg-abs-change_HR-median_diff-then-groupave')


#--------------------------------------------------------------------------
## Mean changes in reported stress without baseline correction
cat('\n\nMean changes in stress levels ... \n')

self.stress <- readRDS(file.path(dir.imdata, 'self_stress.rds'))
# this is a pre-saved dataset distributed as part of the repository

exclude.sbj.arr1 <- sprintf('CORE2015%02d', OPTS$nppstress$exclude.subjects) #have medication
exclude.sbj.arr2 <- c('CORE201504','CORE201507') #did not report stress
ss <- filter(self.stress, !(subject %in%
                                       union(exclude.sbj.arr1, exclude.sbj.arr2))) %>%
      as.tibble()


ss %>%  group_by(fulltask) %>%
  summarise(n = n(), mean = mean(value))

reptb <-  ss %>%
          group_by(subject, task) %>%
          summarise(n = n(), max = max(value)) %>%
          group_by(task) %>%
          summarise(n = n(), mean = mean(max))
table2disk(reptb, OPTS$nppstress$dir.restab,
           'avg-abs-values_repSTRESS')
