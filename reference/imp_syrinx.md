# Import 'Syrinx' selections

`imp_syrinx` imports 'Syrinx' selection data from many files
simultaneously. All files must have the same columns.

## Usage

``` r
imp_syrinx(path = NULL, all.data = FALSE, recursive = FALSE, 
exclude = FALSE, hz.to.khz = TRUE, parallel = 1, pb = TRUE)
```

## Arguments

- path:

  A character string indicating the path of the directory in which to
  look for the text files. If not provided (default) the function
  searches into the current working directory. Default is `NULL`.

- all.data:

  Logical. If `TRUE` all columns in text files are returned. Default is
  `FALSE`. Note that all files should contain exactly the same columns
  in the same order.

- recursive:

  Logical. If `TRUE` the listing recurses into sub-directories.

- exclude:

  Logical. Controls whether files that cannot be read are ignored
  (`TRUE`). Default is `FALSE`.

- hz.to.khz:

  Logical. Controls if frequency variables should be converted from Hz
  (the unit used by Syrinx) to kHz (the unit used by warbleR and other
  bioacoustic analysis packages in R). Default if `TRUE`. Ignored if
  all.data is `TRUE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

## Value

A single data frame with information of the selection files. If all.data
argument is set to `FALSE` the data frame contains the following
columns: selec, start, end, and selec.file. If sound.file.col is
provided the data frame will also contain a 'sound.files' column. If
all.data is set to `TRUE` then all columns in selection files are
returned.

## See also

[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
#load data 
data(selection_files)

#save 'Raven' selection tables in the temporary directory 
writeLines(selection_files[[7]], con = file.path(tempdir(), names(selection_files)[7]))

syr.dat <- imp_syrinx(all.data = FALSE)

# View(syr.dat)

#getting all the data
syr.dat <- imp_syrinx(all.data = TRUE)

# View(syr.dat)
} # }
```
