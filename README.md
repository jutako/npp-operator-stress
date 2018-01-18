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


## Tested under
Debian Linux 9.3 and Ubuntu Linux 16.04 LTS with the following R setup:
```
todo
```
 
