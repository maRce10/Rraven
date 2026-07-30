# Changelog

## Version 1.0.17

### Changes and additions

- [`exp_raven()`](https://marce10.github.io/Rraven/reference/exp_raven.md)
  now can export empty (nrow = 0) selection tables

## Version 1.0.16

CRAN release: 2025-10-23

### Changes and additions

- fix renaming of “Begin File” in
  [`imp_raven()`](https://marce10.github.io/Rraven/reference/imp_raven.md)
- More details about selection tables on documentation

## Version 1.0.15

CRAN release: 2025-07-17

### Changes and additions

- Message in functions that run raven from R to let users know it is
  only works on Raven Pro 1.4 or earlier versions
- Fix no relabeling of “Begin File” column in
  [`imp_raven()`](https://marce10.github.io/Rraven/reference/imp_raven.md)
- ix bug in
  [`imp_raven()`](https://marce10.github.io/Rraven/reference/imp_raven.md)
  when no empty files found

## Version 1.0.14

CRAN release: 2024-08-19

### Changes and additions

- Fix error when selection files have no “selections” column
- Fix error when selection files have no rows. Now they are omitted and
  an be shown if `unread = TRUE`

## Version 1.0.13

CRAN release: 2021-04-21

- Update requested by CRAN

## Version 1.0.12

CRAN release: 2021-03-09

- Update requested by CRAN

## Version 1.0.11

CRAN release: 2021-01-28

- Update requested by CRAN

## Version 1.0.10

CRAN release: 2020-06-08

- Update requested by CRAN

## Version 1.0.9

CRAN release: 2020-02-12

### Changes and additions

- Fix bug on
  [`raven_batch_detec()`](https://marce10.github.io/Rraven/reference/raven_batch_detec.md)
  when reading output Raven files
- Added ‘wave.object’ argument to return a single wave object in
  [`exp_est()`](https://marce10.github.io/Rraven/reference/exp_est.md)

## Version 1.0.8

CRAN release: 2019-11-02

- Package resubmission requested by CRAN

## Version 1.0.7

CRAN release: 2019-09-03

### Changes and additions

- changes required by CRAN for saving files in examples
- imp_raven() error message when not all selections have a sound file
  name column

## Version 1.0.6

CRAN release: 2019-05-23

### Changes and additions

- unique names to temporal .txt output files to allow parallelization in
  ‘raven_batch_detec()’
- new arguments in ‘imp_raven()’: ‘warbler.format’ for importing with
  warbleR selection table column names, ‘only.spectro.view’ for keeping
  measurements only for the spectrogram view and ‘files’ for importing
  specific selection files

## Version 1.0.5

CRAN release: 2019-03-05

### New functions

- exp_empty_sels: export a ‘Raven’ selection for all sound files in a
  folder

### Changes and additions

- bug fix in ‘raven_batch_detec()’ when using custom made detectors
- bug fix in ‘extract_ts()’ when having 1 value or no values in Raven
  frequency contours

## Version 1.0.4

CRAN release: 2018-09-29

### Changes and additions

- ‘detector.name’, ‘detector.preset’ and ‘view.preset’ arguments added
  to ‘raven_batch_detec()’ to allow the use of custom made detectors

## Version 1.0.3

CRAN release: 2018-05-22

### New functions

- to_sound_selection: convert Raven’s selection table files to sound
  selection table files

## Version 1.0.2

CRAN release: 2018-03-11

### New functions

- to_sound_selection: convert Raven’s selection table files to sound
  selection table files
- sort_colms: sort columns in a more intuitive order
- match_wav_case: corrects the case of the extension name of sound files

### Changes and additions

- ‘rm_dup’ argument to remove duplicated rows in ‘imp_raven’ function
- Small changes in vignette table appearance
- Parallel and progress bar available in ‘imp_raven’ function
- ‘hz.to.khz’ and ‘khz.to.hz’ arguments available in ‘relabel_colms’
  function

## Version 1.0.1

CRAN release: 2018-02-11

### Changes and additions

- Small changes in vignette table appearance

## Version 1.0.0

CRAN release: 2017-11-21

- First release
