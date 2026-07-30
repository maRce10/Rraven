# Convert Raven's selection files into sound selection files

`to_sound_selection` converts Raven's selection files into sound
selection files

## Usage

``` r
to_sound_selection(path = NULL, dest.path = NULL, recursive = FALSE,
 parallel = 1, pb = TRUE, sound.file.path, sound.file.col)
```

## Arguments

- path:

  A character string indicating the path of the directory in which to
  look for the 'Raven' selection (text) files. If not provided (default)
  the function searches into the current working directory.

- dest.path:

  A character string indicating the path of the directory in which sound
  selection tables will be saved. If not provided (default) files will
  be saved in directory where the original raven selections were found.

- recursive:

  Logical. If `TRUE` the listing recurses into sub-directories.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- sound.file.path:

  A character string indicating the path of the directory containing the
  sound file(s). This argument is required.

- sound.file.col:

  A character string with the name of the column containing the sound
  file names in the selection text files. Required.

## Value

Sound selection table file(s) saved in 'dest.path' or in the working
directory.

## Details

The function converts Raven's selection tables to sound selection
tables. Sound selection table is a more convenient format as it can be
open directly in Raven (or drag-and-drop) and will automatically open
the associated sound file. Multiple files can be simultaneously
converted. Files must be in '.txt' format. Selection files including
data from multiple recordings can be converted only if all the
correspondent sound files are found in the same directory. Note that no
data is imported into the R environment.

## See also

[`imp_syrinx`](https://marce10.github.io/Rraven/reference/imp_syrinx.md);
[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
{

#load data 
data(selection_files)

# save 'Raven' selection tables in the temporary directory
out <- lapply(1:2, function(x)
writeLines(selection_files[[x]], con = file.path(tempdir(), names(selection_files)[x])))

# try drag and drop selection files into Raven (shouldn't work)

# now convert files
to_sound_selection(sound.file.path = tempdir(), 
sound.file.col = "Begin Path", path = tempdir())

# try drag and drop into Raven again (should work now)
}
#> 'sound.file.col' not found in at least 1 selection file
```
