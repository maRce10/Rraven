# Import 'Raven' selections

`imp_raven` imports several 'Raven' selection files simultaneously.
Files must be in '.txt' format.

## Usage

``` r
imp_raven(path = NULL, warbler.format = FALSE,  all.data = FALSE, files = NULL,
only.spectro.view = TRUE, recursive = FALSE, name.from.file = FALSE,
ext.case = NULL, freq.cols = TRUE, waveform = FALSE, parallel = 1, pb = TRUE,
unread = FALSE, rm.dup = FALSE,  sound.file.col = NULL)
```

## Arguments

- path:

  A character string indicating the path of the directory in which to
  look for the 'Raven' selection (text) files. If not provided (default)
  the function searches into the current working directory.

- warbler.format:

  Logical. If `TRUE` columns are renamed using the standard names for a
  selection table as in the package 'warbleR', frequency limit columns
  (high and low frequency) in 'Hz' are converted to 'kHz' (as in warbleR
  selection tables) and only the spectrogram view measurements are kept.
  Default is `FALSE`.

- all.data:

  Logical. If `TRUE` all columns in the selection files are returned,
  keeping the name columns as in the 'Raven' files. Default is `FALSE`.
  Columns absent in some selection files will be filled with NA's. This
  argument WILL BE DEPRECATED as it is being replaced by
  'warbler.format'.

- files:

  Character vector indicating the name of selection files (in .txt
  format) to be imported. Optional. Default is `NULL`.

- only.spectro.view:

  Logical. If `TRUE` (default) only the measurements in the Raven
  spectrogram view ('View' column) are returned. Ignored if
  `warbler.format == TRUE` (only spectrogram view measurements are
  kept).

- recursive:

  Logical. If `TRUE` the listing recurses into sub-directories.

- name.from.file:

  Logical. If `TRUE` the sound file names are extracted from the
  selection text file name. It assumes that selections files contained
  the suffix "Table.1.selections.txt", "selections.txt" or ".txt" (in
  that order). Note that by default it will assume that the extension
  file name is ".wav". This can be control using the argument
  'ext.case'. Default is `FALSE`). Ignored if sound.file.col' is
  provided and/or all.data is `TRUE`). Note that the time information
  for selection tables with multiple sound files won't be corrected if
  `name.from.file = TRUE`.

- ext.case:

  Character string of length 1 to specify whether sound file extensions
  are in upper or lower case. This should match the extension of the of
  the .wav files from which the selection were made. It must be either
  'upper' or 'lower'. Only needed when 'name.from.file' is `TRUE`.

- freq.cols:

  Logical. If `TRUE` 'Low Freq' and 'High Freq' columns are also
  imported. Ignored if all.data is `TRUE`.

- waveform:

  Logical to control if waveform view data should be included (this data
  is typically duplicated in spectrogram view data). Default is `FALSE`
  (not to include it). This argument WILL BE DEPRECATED as it is being
  replaced by 'only.spectro.view'.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- unread:

  DEPRECATED. Logical. If `TRUE` a list (instead of a data frame). The
  first element of the list contains the selections. This argument has
  been deprecated. Name of unread files are now kept in
  `.Options$Rraven`. whole data. The second and third elements are
  character vectors with the names of sound files that could not be read
  or that contain multiple sound files but no 'File Offset' column and
  could not be imported. Default is `FALSE`.

- rm.dup:

  Logical. If `TRUE` duplicated rows and columns are removed. Useful
  when selection files have been duplicated. Default is `FALSE`.

- sound.file.col:

  A character string with the name of the column containing the sound
  files in the selection text files. Default is `NULL`. Ignored if
  'name.from.file' is `TRUE` and/or all.data is `TRUE`. This argument
  WILL BE DEPRECATED as the function now searches for columns containing
  the sound file names.

## Value

A single data frame with information of the selection files. If some
selection files were not read they will be listed in `.Options$Rraven`.
If 'warbler.format' argument is set to `TRUE` the data frame contains
the following columns: sound.files, selec, channel,start, end, top.freq,
bottom.freq and selec.file. If all.data is set to `TRUE` then all
columns in the 'Raven' selection files are returned. If individual
selection files contain information about multiple sound files the
function will import the file and correct the time parameters (start and
end) only if 1) the 'File Offset (s)' is found in the selection table.

## Details

The function import 'Raven' selection data from many files
simultaneously. All selection files in the working directory or 'path'
supplied will be imported (unless 'files' argument is also supplied). It
has been created using Raven Pro 1.5 and tested on Raven 1.6. Selection
tables created with other versions might not be read properly. Files
must be in '.txt' format. Selection files including data from multiple
recordings can also be imported although they must contained a 'File
Offset (s)' column. Selections that span across multiple sound files are
not recommended as they will be assigned to the first sound file, which
would produce errors for downstream analyses as those from the 'warbleR'
package. Empty '.txt' files are ignored.

## See also

[`imp_syrinx`](https://marce10.github.io/Rraven/reference/imp_syrinx.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r

# load data
data(selection_files)

# save 'Raven' selection tables in the temporary directory
out <- lapply(1:2, function(x)
writeLines(selection_files[[x]], con = file.path(tempdir(), names(selection_files)[x])))

# \donttest{
# providing the name of the column with the sound file names
rvn.dat <- imp_raven(sound.file.col = "Begin.File", all.data = FALSE, path = tempdir())

# View(rvn.dat)
# }
```
