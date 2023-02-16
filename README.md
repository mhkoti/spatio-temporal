# Spatio-temporal modelling of bat activity in Finland
---


## Description of the data and file structure

This repository includes a pre-processed dataset and all necessary code for replicating the analyses in 'Large-scale long-term passive-acoustic monitoring reveals spatio-temporal activity patterns of boreal bats' (DOI: 10.22541/au.166826490.04895229/v1) 

In the data file 'data.csv' there are following columns:

-period: ordinal referring to the 2 week subdivision of the study season 1.5.-15.10. 1 refers to the nights from 1.5. to 14.5. (starting date of a night) and so on.

-year: the year (2015-2021)

-taxa: taxon (Eptesicus nilssonii, Myotis sp., Pipistrellus nathusii)

-place: station code and number referring to the recording place within each field station

-station: name of the field station

-fail: number of 10 min units without an observation

-succ: number of 10 min units with an observation

-lat: latitude of the field station

-mic: number to specify whether an original (1) or new (2) mic was used in the specific device

During pre-processing, raw bat observations have been cleaned to remove misidentifications, observations outside the study season (1.5.-15.10.), and non-focal taxa. The remaining observations have been allocated into 10-minute-long observation units grouped by period, year, taxon and place. The number of successful and failed (taxon recorded or not) units were then summarized and only the counts are presented in the data provided here.


## Sharing/Access information

The data is also available at https://github.com/mhkoti/spatio-temporal
The published reseach article is available at 10.22541/au.166826490.04895229/v1


## Code/Software

The R script 'spatiotemporal_main.R' includes all necessary code for replicating the results presented in the article. The user only needs to store 'data.csv' in the working directory and run the script.
The script was developed and tested with R version 4.2.0, tidyverse 1.3.1, glmmTMB 1.1.4 and effects 4.2-2.

The additional script 'spatiotemporal_aic_comparison.R' includes the code for fitting all reduced versions of the main models to ensure that the complexity of the main models is justified. This can be run only after the script 'spatiotemporal_main.R'

