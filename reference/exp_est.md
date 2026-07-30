# Export wave objects of extended selection tables as sound files

`exp_est` exports wave objects of an extended selection table as sound
files

## Usage

``` r
exp_est(X, file.name = NULL, path = NULL, single.file = FALSE, 
selection.table = TRUE, pb = TRUE, normalize = TRUE, parallel = 1, wave.object = FALSE)
```

## Arguments

- X:

  object of class 'extended_selection_table' (objects produced by
  [`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.html)).
  More details about these objects can be found on [this
  link](https://marce10.github.io/warbleR/articles/annotation_data_format.html#extended-selection-tables).

- file.name:

  character string indicating the name of the sound file (if
  `single.file = TRUE`) and/or the selection table (if
  `selection.table = TRUE`). Default is `NULL`.

- path:

  A character string indicating the path of the directory where sound
  files and/or selection table will be saved. If not provided the
  function uses the current working directory. Default is `NULL`.

- single.file:

  Logical argument to control if all wave objects are pooled together in
  a single sound file (if `TRUE`) or each one as an individual sound
  file (if `FALSE`, default). If exporting a single sound file the files
  are concatenated in the same sequences as in the extended selection
  table. Note that to create a single sound file ALL WAVE OBJECTS IN 'X"
  MUST HAVE THE SAME SAMPLE RATE (check
  `attributes(X)$check.res$sample.rate`) and ideally the same bit depth
  (although not strictly required). If that is not the case, sample rate
  can be homogenize using the
  [`resample_est`](https://marce10.github.io/warbleR/reference/resample_est.html)
  from the warbleR package.

- selection.table:

  Logical argument to determine if a Raven sound selection table ('.txt'
  file) is also exported. Default is `TRUE`. If `FALSE` then selection
  table is return as an object in the R environment. If exporting
  multiple sound files (if `single.file = FALSE`) the function still
  exports a single selection table (in this case a multiple sound
  selection table).

- pb:

  Logical argument to control progress bar when exporting multiple sound
  files. Default is `TRUE`.

- normalize:

  Logical argument to control if wave objects are individually
  normalized before exporting (or before being concatenated if
  `single.file = TRUE`). Normalization rescales amplitude values to a 16
  bit dynamic range. Default is `FALSE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- wave.object:

  Logical argument to control if ONLY a single wave object is returned
  in the R environment (TRUE) instead of a wave file in the working
  directory (and a selection table if `selection.table = TRUE`). Default
  is `FALSE`.

## Value

Sound file(s) are saved in the provided path or current working
directory. If `selection.table = TRUE` a Raven sound selection table
with the data in 'X' will also be saved.

## Details

Extended selection tables are annotations that include both the acoustic
and annotation data. This is an specific object class,
`extended_selection_table` (from the package
[warbleR](https://cran.r-project.org/package=warbleR)), that includes a
list of 'wave' objects corresponding to each the annotations in the data
([see a full description
here](https://marce10.github.io/warbleR/articles/annotation_data_format.html#extended-selection-tables)).
The function `exp_est` takes the wave objects contained as attributes in
extended selection tables and saves them as sound files in '.wav'
format. A single sound file containing all the annotations or several
sound files (one for each wave object) can be produced (see
'single.file' argument). In addition, a Raven sound selection table can
be saved along with the sound files. The exported selection table can be
open in Raven for exploring/manipulating selections in 'X'. Note that
the background noise margin (if any) that is found on each wave object
before and/or after the annotated sound is also exported. Note that for
extended selection tables created 'by song' ([described
here](https://marce10.github.io/warbleR/articles/annotation_data_format.html#by-element-vs-by-song-extended-selection-tables);
see argument 'by.song' in the warbleR function
[`selection_table`](https://marce10.github.io/warbleR/reference/selection_table.html))
when using the argument `single.file = FALSE` the function will export a
single sound file for every 'song' in the selection table. In other
words exported sound files will contain all the annotations for a given
song, instead of a single annotation per sound file.

## See also

[`exp_raven`](https://marce10.github.io/Rraven/reference/exp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# load example data
data(list = "lbh.est", package = "NatureSounds")

# subset to 10 selections
X <- lbh.est[1:10, ]

# Export data to a single sound file
exp_est(X, file.name = "test", single.file = TRUE, path = tempdir())

# Export data to a single sound file and normalizing, no pb
exp_est(X, file.name = "test2", single.file = TRUE, normalize = TRUE, pb = FALSE, path = tempdir())

# several files
exp_est(X, single.file = FALSE, file.name = "test3", path = tempdir())
} # }
```
