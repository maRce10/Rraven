# Version 1.0.12

* Update requested by CRAN

# Version 1.0.11

* Update requested by CRAN

# Version 1.0.10

* Update requested by CRAN

# Version 1.0.9

## Changes and additions

* Fix bug on raven_batch_detec() when reading output Raven files
* Added 'wave.object' argument to return a single wave object in 'exp_est()'

# Version 1.0.8

* Package resubmission requested by CRAN

# Version 1.0.7

## Changes and additions

* changes required by CRAN for saving files in examples
* imp_raven() error message when not all selections have a sound file name column

# Version 1.0.6

## Changes and additions

* unique names to temporal .txt output files to allow parallelization in 'raven_batch_detec()'
* new arguments in 'imp_raven()': 'warbler.format' for importing with warbleR selection table column names, 'only.spectro.view' for keeping measurements only for the spectrogram view and 'files' for importing specific selection files

# Version 1.0.5

## New functions

* exp_empty_sels: export a 'Raven' selection for all sound files in a folder

## Changes and additions

* bug fix in 'raven_batch_detec()' when using custom made detectors 
* bug fix in 'extract_ts()' when having 1 value or no values in Raven frequency contours 

# Version 1.0.4

## Changes and additions

* 'detector.name', 'detector.preset' and 'view.preset' arguments added to 'raven_batch_detec()' to allow the use of custom made detectors 

# Version 1.0.3

## New functions

* to_sound_selection: convert Raven's selection table files to sound selection table files

# Version 1.0.2 

## New functions

* to_sound_selection: convert Raven's selection table files to sound selection table files
* sort_colms: sort columns in a more intuitive order
* match_wav_case: corrects the case of the extension name of sound files

## Changes and additions

* 'rm_dup' argument to remove duplicated rows in 'imp_raven' function
* Small changes in vignette table appearance
* Parallel and progress bar available in 'imp_raven' function
* 'hz.to.khz' and 'khz.to.hz' arguments available in 'relabel_colms' function

# Version 1.0.1

## Changes and additions

* Small changes in vignette table appearance

# Version 1.0.0

* First release

