# A script that sets up the analysis environment

# Assumes that:
# * R working directory is <repo>/R (getwd())
# * required packages are installed using packrat (see repository readme.md)


###################################################################
## Load packages - note!: order matters for plyr-dplyr!
##library(devtools)

#library(reshape2) #loads plyr, hence loaded before dplyr
#library(ggplot2) #loads plyr, hence loaded before dplyr

#library(plyr) #should be loaded before dplyr or not used at all

library(plyr)
# plyr needs to be loaded BEFORE dplyr (in tidyverse) to avoid conflict.
# Best option would be to use only dplyr, but due to historical reasons
# both are in use here.

library(tidyverse)

library(gridExtra)
#library(tidyr)
#library(stringr)
#library(lubridate)

#library(data.table)
#library(dplyr)
#library(dtplyr)
#library(plotrix) # draw.ellipse() needed - colibri?

#library(intervals) # for finding overlaps


###################################################################
## Load project code

source("opts.R") #settings
source("utils.R") #general utilities
source("batch_utils.R") #batch processing functions
#source("preprocess_features.R")


###################################################################
## Load project setup
OPTS <- loadSettings(basedir.project = dirname(getwd()))
