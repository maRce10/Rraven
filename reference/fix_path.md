# Modify sound file path in Raven's selection tables

`fix_path` modifies the path column in selection tables and sound
selection tables

## Usage

``` r
fix_path(path = NULL, dest.path = NULL, recursive = FALSE, parallel = 1, pb = TRUE, 
new.begin.path, sound.file.col, files = NULL)
```

## Arguments

- path:

  A character string indicating the path of the directory in which to
  look for the 'Raven' selection (text) files. If not provided (default)
  the function searches into the current working directory.

- dest.path:

  A character string indicating the path of the directory in which sound
  selection tables will be saved. If not supplied selection files will
  be overwritten with the new begin path. If not provided (default)
  files will be save in the current directory.

- recursive:

  Logical. If `TRUE` the listing recurses into sub-directories.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- new.begin.path:

  A character string indicating the path of the directory where sound
  files would be located. This argument is required.

- sound.file.col:

  A character string with the name of the column containing the sound
  file names in the selection text files. Required.

- files:

  Character vector indicating the name of selection files (in .txt
  format) to be imported. Optional. Default is `NULL`.

## Value

Selection table file(s) saved in 'dest.path' or in the working directory
(by default, which overwrites existing files).

## Details

The function modifies the path field in Raven's selection tables or
sound selection tables. This is useful when sound files have been moved
to a different location (or computer). Note that the ability to open
selections and sound files simultaneously works as long as the
"begin.path" column is referring to the directory containing the sound
files.

## See also

[`to_sound_selection`](https://marce10.github.io/Rraven/reference/to_sound_selection.md);
[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{
# load warbleR for sound file examples
library(NatureSounds)

#load data
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selection_files"))

# save sound files
tuneR::writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"), extensible = FALSE)
# save 'Raven' selection tables in the temporary directory
out <- lapply(1:2, function(x)
writeLines(selection_files[[x]], con = file.path(tempdir(), names(selection_files)[x])))

# try drag and drop selection files into Raven (shouldn't work)

# now fix files
fix_path(path = tempdir(),
sound.file.col = "Begin File", new.begin.path = "YOUR NEW LOCATION HERE")

# try drag and drop into Raven again (should work now)
}
```
