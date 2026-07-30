# Export a 'Raven' selection for all sound files in a folder

`exp_empty_sels` exports a 'Raven' selection data in .txt format that
includes empty selections for all sound files in a folder.

## Usage

``` r
exp_empty_sels(path = NULL, file.name = NULL, sound.files = NULL, pb = TRUE)
```

## Arguments

- path:

  A character string indicating the path of the directory in which to
  look for sound files. If not provided (default) the function will use
  the current working directory.

- file.name:

  Name of the output .txt file. If `NULL` then the folder name is used
  instead.

- sound.files:

  character vector indicating the sound files that will be included. If
  `NULL` (default) then all sound files in the working directory (or
  'path') will be included.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

## Value

The function saves a selection table in '.txt' format that can be
directly opened in Raven. No objects are returned in the R environment.

## Details

The function saves a selection file in '.txt' format (that can be
directly opened in Raven) that will display all sound files in the
provided directory (argument 'path') by having a single point annotation
(i.e. annotation in which the start and end time are the same and bottom
and top frequency are also the same) at the beginning of each sound file
. Useful to simplify the making of selections from several sound files
that need to be displayed simultaneously (e.g. several recordings from
the same individual). The selection file is saved in the provided
directory ('path').

## See also

[`exp_raven`](https://marce10.github.io/Rraven/reference/exp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r

# Load data
library(NatureSounds)
#> Loading required package: knitr
library(warbleR)
#> Loading required package: tuneR
#> Loading required package: seewave
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

## Export a single selection table including multiple files
#save sound files 
tuneR::writeWave(Phae.long1, file.path(tempdir(), 
"Phae.long1.wav"), extensible = FALSE) 
tuneR::writeWave(Phae.long2, file.path(tempdir(), 
"Phae.long2.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long3, file.path(tempdir(), 
"Phae.long3.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long4, file.path(tempdir(), 
"Phae.long4.wav"), extensible = FALSE)

# export with no file name
exp_empty_sels(path = tempdir())
#> all selections are OK 
#> 
#> exporting annotations (step 0 of 0):

# export with file name
exp_empty_sels(file.name = "Phaethornis.longirostris", path = tempdir())
#> all selections are OK 
#> 
```
