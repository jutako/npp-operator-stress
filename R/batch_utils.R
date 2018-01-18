
#----------------------------------------------------------------------------
## Helper functions
#----------------------------------------------------------------------------

# A function to add factor task to a subset of feature data
# 'fds' should contain only one level of '$part', events can contain
# all levels.
# Uses intervals -package to do the mathing of events to fds timestamps
add_task <- function(fds, events){

  # subset events
  evs <- filter(events, part == as.character(unique(fds$part)) )

  # create interval objects
  evm <- matrix(c(as.integer(evs$starttime), as.integer(evs$stoptime)),
                byrow = F, ncol = 2)
  evi <- intervals::Intervals(evm, closed = c(T,T), type = 'Z')

  fdm <- matrix(c(as.integer(fds$timestamp), as.integer(fds$timestamp)+1),
                byrow = F, ncol = 2)
  fdi <- intervals::Intervals(fdm, closed = c(T,F), type = 'Z')

  # find overlaps
  tmp <- intervals::interval_overlap(from = fdi, to = evi)

  # debug checks
  #if (! length(unlist(tmp)) == nrow(fdi)) {browser()}
  #em <- sapply(tmp, length) == 2 #for P3 several matches!

  # mark missing as NA or select first match and unlist
  tmp <- sapply(tmp, function(el){if (length(el)==0) {NA} else {el[1]}},
                simplify = T)

  # create task variable
  fds$task <- evs$task[tmp]
  fds
}


# Add "prcclass"
add.prcclass <- function(tmpd){
  tmpd$prcclass <- as.character(tmpd$variable)
  tmpd$prcclass <- gsub(".*_min", '0', tmpd$prcclass)
  tmpd$prcclass <- gsub(".*_prc10", '10', tmpd$prcclass)
  tmpd$prcclass <- gsub(".*_prc25", '25', tmpd$prcclass)
  tmpd$prcclass <- gsub(".*_prc50", '50', tmpd$prcclass)
  tmpd$prcclass <- gsub(".*_prc75", '25', tmpd$prcclass)
  tmpd$prcclass <- gsub(".*_prc90", '10', tmpd$prcclass)
  tmpd$prcclass <- gsub(".*_max", '0', tmpd$prcclass)
  tmpd$prcclass <- -as.numeric(tmpd$prcclass)
  tmpd
}


# In some cases also create_factors() might work.
set.plot.data.factors <- function(pd, factors){
  if ('task' %in% names(pd)){
    pd$task <- ordered(as.character(pd$task),
                       levels = factors$strev$levels,
                       labels = factors$strev$labels)
  }
  if ('variable' %in% names(pd)){
    pd$variable <- ordered(as.character(pd$variable),
                           levels = factors$feat$levels,
                           labels = factors$feat$labels)
  }
  pd
}

#' Maps values in data according to rules definec by from and to
map.values <- function(data, from, to){
  as.character(ordered(data, levels = from, labels = to))
}



# Function to compute custom y-tick positions
mybreaks <- function(vec){
  a <- seq(floor(min(vec)), ceiling(max(vec)), 1)
  b <- seq(floor(min(vec)), ceiling(max(vec)), 0.25)

  if (length(a) < 6){
    b
  } else {
    a
  }
}

# Make plot for one variable
make.plot.1var <- function(pd2, pd, cvar, ccol, clet){

  require(grid)

  pds <- subset(pd, variable == cvar)
  pd2s <- subset(pd2, variable == cvar)

  #   clim <- c( min(floor(min(pds$value, na.rm = T)), 0),
  #              ceiling(max(pds$value, na.rm = T))  )
  clim <- c( reshape::round_any(min(pds$value, na.rm = T), 0.25, floor),
             reshape::round_any(max(pds$value, na.rm = T), 0.25, ceiling) )

  # my_grob = grobTree(textGrob(clet, x=0.01,  y=0.88, hjust=0,
  #                             gp=gpar(col="black", fontsize=15)))
  # my_grob = grobTree(textGrob(clet, x=-0.03,  y=1, hjust=0,
  #                             gp=gpar(col="black", fontsize=15)))

  p <- ggplot(pd2s)
  # aes(colour = colset[cind]))
  p <- p + geom_point(aes(x = task, y = mean))
  p <- p + geom_linerange(aes(x = task, ymax = mean + sd, ymin = mean - sd ))
  p <- p + geom_line(aes(x = task, y = mean, group = variable), colour = ccol)
  #p <- p + geom_text(aes(x = 1, y = 1.5, label = cvar))

  p <- p + geom_line(aes(x = task, y = value, group = gvar),
                     data = pds,
                     colour = ccol, alpha = 0.2)
  #p <- p + annotation_custom(my_grob)
  p <- p + facet_grid(.~variable)
  #p <- p + scale_y_continuous(breaks = mybreaks, limits = clim)
  # If this is not done correctly some error bars may disappear
  p <- p + theme_light()
  #p <- p + scale_color_manual(values = rep(colors.three.qualitative[1],2))
  #p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
  p <- p + theme(legend.position="none",
                 strip.text.x = element_text(size = 14, color = 'black'),
                 axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0))
  p <- p + labs(y = NULL, x = NULL)
  p
}
# make.plot.1var(pd2, pd1, fvarset[1], colset[1], letset[1])

# Iterates over varset
make.plot.set <- function(varset, colset, letset, pd2, pd, ylims){
  plst <- list()
  for (i in seq_along(varset)){
    p <- make.plot.1var(pd2, pd, varset[i], colset[i], letset[i])

    #     if (i != 1){
    #       p <- p + theme(axis.text.x = element_blank())
    #     }

    #     if (i == length(varset)){
    #       p <- p + labs(x = 'task')
    #     }

    #     if (i > 2){
    #       p <- p + scale_y_continuous(limits = ylims)
    #       #p <- p + scale_y_log10()
    #     }
    plst <- c(plst, list(p))
  }
  plst
}

# Creates grobs from ggplots, searches for the widest and sets all widths equal
# Calls grid.arrange to make the plot
render_plot <- function(plst){
  gtlst <- vector(mode = 'list', length = length(plst))
  for (i in seq_along(plst)){
    gtlst[[i]] <- ggplot_gtable(ggplot_build(plst[[i]]))
  }

  maxWidth = unit.pmax(gtlst[[1]]$widths[2:3],
                       gtlst[[2]]$widths[2:3],
                       gtlst[[3]]$widths[2:3],
                       gtlst[[4]]$widths[2:3])

  for (i in seq_along(gtlst)){
    gtlst[[i]]$widths[2:3] <- maxWidth
  }
  do.call(grid.arrange, c(gtlst, list(ncol = 1)) )
}
# Option for the above:
#do.call(grid.arrange, c(plst1[1:2], list(ncol = 1)) )
#grid.draw(rbind(ggplotGrob(plst1[[1]]), ggplotGrob(plst1[[2]]), size = "last"))

adjust_gtables <- function(plst){
  gtlst <- vector(mode = 'list', length = length(plst))
  for (i in seq_along(plst)){
    gtlst[[i]] <- ggplot_gtable(ggplot_build(plst[[i]]))
    #gtlst[[i]]$layout$clip[gtlst[[i]]$layout$name == "panel"] <- "off"
  }

  maxWidth = unit.pmax(gtlst[[1]]$widths[2:3],
                       gtlst[[2]]$widths[2:3],
                       gtlst[[3]]$widths[2:3],
                       gtlst[[4]]$widths[2:3])

  for (i in seq_along(gtlst)){
    gtlst[[i]]$widths[2:3] <- maxWidth
  }
  gtlst
}


#' Function to use with ddply to compute similarities using simfun
#' daf = ddply subset of the input data frame
#' stressdf = stress data.frame
#' simfun = function to compute similarity with e.g. cor()
ddply.compute.similarity <- function(daf, stressdf, simfun){
  #daf <- dplyr::arrange(daf, part, role, task) #todo: why this was here?
  stresstmp <- dplyr::filter(stressdf, (role == unique(daf$role)) &
                               (part == unique(daf$part)) &
                               (task %in% unique(daf$task)) )
  daf <- inner_join(daf, stresstmp, by = 'task')

  # usually is.na(cor)==T if value.x does not change at all
  # if (is.na(cor(daf$value.x, daf$value.y))){
  #   browser()
  # }

  data.frame(n = nrow(daf),
             cor = simfun(daf$value.x, daf$value.y))
}


# Subset all variables to the specified range
# Half of seglen is subtracted from all timestamps to get segment start
# instead of segment midpoint. This is done to ensure that the shorter trigger
# segments will position to the beginning of the longer analysis segments.
subset.to.trigger.cseg <- function(daf, trgd, winlen, trgd.winlen = 60){
  #debug:
  #daf <- subset(hrvd, part == 'P4' & role == 'TO' & task == 'run1' & variable == "ibi_medianhr")
  # daf <- subset(hrvd, part == 'P1' & role == 'TO'
  #               & task == 'pre-base' & variable == "ibi_medianhr")
  # trgd <- filter(trig.data, which_extreme == 'high')

  if (nrow(daf) > 0){
    daf <- daf[order(daf[['timestamp']]),] #order by time

    cpart <- as.character(unique(daf$part))
    crole <- as.character(unique(daf$role))
    ctask <- as.character(unique(daf$task))
    trig.ts <- subset(trgd, (part == cpart) & (role == crole) & (task == ctask))$timestamp
    #find index equal or just smaller than trig.ts
    trig.ts.ind <- max( which( (daf$timestamp - winlen/2) <=
                                 (trig.ts - trgd.winlen/2), arr.ind = T) )
    # test <- difftime(daf$timestamp - winlen/2, trig.ts - trgd.winlen/2, units = 'secs')
    # test2 <- difftime(daf$timestamp, trig.ts, units = 'secs')
    # which.min(abs(test))
    # browser()
    if (is.infinite(trig.ts.ind)){
      browser()
    }

    daf[trig.ts.ind, ]

  } else {
    daf
  }
} # enf of function


# Apply subset.to.trigger.cseg differently for each set of tasks
subset_feature_data <- function(hrvd, trig.data, winlen, trgd.winlen = 60){

  ## Tasks for which high stress should be reported
  task_arr <- c('pre-base', 'run1', 'run2', 'run3')
  trgd1 <- filter(trig.data, which_extreme == 'high')
  hrvds1 <- plyr::ddply(filter(hrvd, task %in% task_arr),
                        .(part, role, task, variable),
                        function(x){subset.to.trigger.cseg(x, trgd1, winlen, trgd.winlen)})


  ## Tasks for which low stress should be reported
  task_arr <- c('normal-run','post-base')
  trgd2 <- filter(trig.data, which_extreme == 'low')
  hrvds2 <- plyr::ddply(filter(hrvd, task %in% task_arr),
                        .(part, role, task, variable),
                        function(x){subset.to.trigger.cseg(x, trgd2, winlen, trgd.winlen)})

  hrvds <- rbind(hrvds1, hrvds2)

  # Sanity checks:
  # check that there is only one variable per part-role-task
  testd <- data.table::as.data.table(hrvds)
  if ( max(unique(testd[, .N, by = .(part, role, task, variable)]$N)) != 1 ){
    browser()
  }

  hrvds
}


