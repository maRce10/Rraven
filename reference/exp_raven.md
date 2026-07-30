# Export 'Raven' selections

`exp_raven` exports selection tables as 'Raven' selection data in .txt
format.

## Usage

``` r
exp_raven(X, path = NULL, file.name = NULL, khz.to.hz = TRUE, 
sound.file.path = NULL, single.file = TRUE, parallel = 1, pb = TRUE)
```

## Arguments

- X:

  Object of class data frame or
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.html)
  containing columns for sound file (sound.files), selection (selec),
  start and end time of signals ('start' and 'end') and low and high
  frequency ('bottom.freq' and 'top.freq', optional). See example data
  'selec_table' in the
  [warbleR](https://cran.r-project.org/package=warbleR)) package.

- path:

  A character string indicating the path of the directory in which to
  save the selection files. If not provided (default) the function saves
  the file into the current working directory.

- file.name:

  Name of the output .txt file. If `NULL` then the sound file names are
  used instead. If multiple selection files are generated (see
  'single.file') then the sound files names are added to the provided
  'file.name'. Ignored if `single.file = FALSE`.

- khz.to.hz:

  Logical. Controls if frequency variables should be converted from kHz
  (the unit used by other bioacoustic analysis R packages like
  [warbleR](https://cran.r-project.org/package=warbleR)) to Hz (the unit
  used by Raven). Default is `TRUE`.

- sound.file.path:

  A character string indicating the path of the directory containing the
  sound file(s). Providing this information allows to open both sound
  file and selection table simultaneously. This can be done by using the
  'File \> Open selection table' option in 'Raven' (or drag/drop the
  selection file into Raven). Default is `NULL`. This argument is
  required when exporting selections from multiple sound files.

- single.file:

  Logical. Controls whether a single selection file (`TRUE`; default) or
  a selection file for each sound file (`FALSE`, hence, only applicable
  when several sound files are included in 'X') are generated. Note that
  'sound.file.path' must be provided when exporting several sound files
  into a single selection file as the duration of the sound files is
  required.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

## Value

The function saves a selection table in '.txt' format that can be
directly opened in Raven. If several sound files are available users can
either export them as a single selection file or as multiple selection
files (one for each sound file). No objects are returned in the R
environment.

## Details

The function exports selection tables (as the ones used in the R package
[warbleR](https://cran.r-project.org/package=warbleR)) into the 'Raven'
selection file format ('.txt'). This can be useful to obtain additional
Raven measurements on existing selections by adding new measurements to
the selection table once in Raven. Note that selection labels must be
numeric and non-duplicated when exporting them to Raven. If that is not
the case the function will relabeled the selections and the previous
selection labels will be retained in a new column ('old.selec').

## See also

[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md);
[`imp_syrinx`](https://marce10.github.io/Rraven/reference/imp_syrinx.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r

# Load data
library(warbleR)
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

# Select data for a single sound file
st1 <- lbh_selec_table[lbh_selec_table$sound.files == "Phae.long1.wav", ]

# Export data of a single sound file
exp_raven(st1, file.name = "Phaethornis 1", path = tempdir())

# Export a single selection table including multiple files
tuneR::writeWave(Phae.long1, file.path(tempdir(), 
"Phae.long1.wav"), extensible = FALSE) #save sound files 
tuneR::writeWave(Phae.long2, file.path(tempdir(), 
"Phae.long2.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long3, file.path(tempdir(), 
"Phae.long3.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long4, file.path(tempdir(), 
"Phae.long4.wav"), extensible = FALSE)

# export raven selection as single file
exp_raven(X = lbh_selec_table, file.name = "Phaethornis multiple sound files",
single.file = TRUE, sound.file.path = tempdir(), path = tempdir())
```
