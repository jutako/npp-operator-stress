# npp-operator-stress

Open access analyses related to the Psychophysiology article about NPP operator stress. Link to the article will be added

```
here
```

when the manuscript has been published.

## Howto
Use
```
git pull https://github.com/bwrc/npp-operator-stress.git <target directory>
```
at some suitable location on your local machine to get a copy of this repo.

It is assumed that `<target directory>/R` is the current working directory in R when reproducing the analyses.

This project uses `packrat` to keep track of needed packages and their versions. Run
```
packrat::restore()
```
to install all available libraries.

You may reproduce statistical analyses and manuscript figures using
 ```
 source('batch.R')
 ```
 Note that especially the bootstrapping step takes a fairly long time depending on the value of variable `N_BOOT` in `experiments.R`.
 
The reported main results will appear under `<repo>/analysis/HRV_blocks_event/colibri_hrv_win240_ovrl180/npp-stress`. The path is long as it is a one-to-one copy of the actual branch used to store analysis results in the actual project that this manuscript comes from.


## Analysis overview
The manuscript analysis consists of the following steps:
1. Raw ECG HRV feature extraction using Colibri (https://github.com/bwrc/colibri)
2. HRV feature baseline correction and calculation segment selection
3. Merging HRV and stress data
4. Visualizations and statistical analysis

The current repository provides datasets and code to reproduce step 4 ('batch.R'). Reproduction of steps 2. and 3. is not set up, but the script that was used to do this is present (`preprocess_features.R`). Code for feature extraction (step 1) is not included as it requires too much effort to create dummy ECG data and as Colibri is already publicly available.

## Usage overview
'init.R' to loads packages and sets up the environment, once all packages listed in `packrat` are installed and operational.

The feature values in source data are completely random gaussian noise, since we cannot at this point make the original dataset available online. However, all factors in the data are exactly as reported in the manuscript.

We recommend using RStudio (https://www.rstudio.com/) to interact with R.

## Known issues
* "plotting time series" causes a graphics device to pop-up when using from command line outside RStudio.
* also calls to `View()` produce unexpected results outside RStudio

## Tested under
Debian Linux 9.3 and Ubuntu Linux 16.04 LTS with the following R setup:
```
> sessionInfo()
R version 3.3.3 (2017-03-06)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Debian GNU/Linux 9 (stretch)

locale:
 [1] LC_CTYPE=en_US.utf8          LC_NUMERIC=C                
 [3] LC_TIME=en_US.utf8           LC_COLLATE=en_US.utf8       
 [5] LC_MONETARY=en_US.utf8       LC_MESSAGES=en_US.utf8      
 [7] LC_PAPER=en_US.utf8          LC_NAME=en_US.utf8          
 [9] LC_ADDRESS=en_US.utf8        LC_TELEPHONE=en_US.utf8     
[11] LC_MEASUREMENT=en_US.utf8    LC_IDENTIFICATION=en_US.utf8

attached base packages:
[1] grid      stats     graphics  grDevices utils     datasets  methods  
[8] base     

other attached packages:
 [1] boot_1.3-20         bindrcpp_0.2        reshape2_1.4.3     
 [4] VennDiagram_1.6.18  futile.logger_1.4.3 xlsx_0.5.7         
 [7] xlsxjars_0.6.1      rJava_0.9-9         lubridate_1.7.1    
[10] gridExtra_2.3       forcats_0.2.0       stringr_1.2.0      
[13] dplyr_0.7.4         purrr_0.2.4         readr_1.1.1        
[16] tidyr_0.7.2         tibble_1.4.1        ggplot2_2.2.1      
[19] tidyverse_1.2.1     plyr_1.8.4         

loaded via a namespace (and not attached):
 [1] Rcpp_0.12.14         lattice_0.20-35      assertthat_0.2.0    
 [4] rprojroot_1.3-2      digest_0.6.14        packrat_0.4.8-53    
 [7] psych_1.7.8          R6_2.2.2             cellranger_1.1.0    
[10] futile.options_1.0.0 backports_1.1.2      evaluate_0.10.1     
[13] httr_1.3.1           pillar_1.1.0         rlang_0.1.6         
[16] lazyeval_0.2.1       curl_3.1             readxl_1.0.0        
[19] data.table_1.10.4-3  rstudioapi_0.7       rmarkdown_1.8       
[22] labeling_0.3         devtools_1.13.4      foreign_0.8-69      
[25] munsell_0.4.3        broom_0.4.3          modelr_0.1.1        
[28] pkgconfig_2.0.1      mnormt_1.5-5         htmltools_0.3.6     
[31] tidyselect_0.2.3     reshape_0.8.7        crayon_1.3.4        
[34] withr_2.1.1          MASS_7.3-48          nlme_3.1-131        
[37] jsonlite_1.5         gtable_0.2.0         magrittr_1.5        
[40] scales_0.5.0.9000    cli_1.0.0            stringi_1.1.6       
[43] xml2_1.1.1           cowplot_0.9.2        lambda.r_1.2        
[46] tools_3.3.3          glue_1.2.0           hms_0.4.0           
[49] parallel_3.3.3       colorspace_1.3-2     rvest_0.3.2         
[52] memoise_1.1.0        knitr_1.18           bindr_0.1           
[55] haven_1.1.1         
```
 
