# Open sound files in 'Raven' sound analysis software

`run_raven` opens several sound files in 'Raven' sound analysis software

## Usage

``` r
run_raven(raven.path = NULL, sound.files = NULL, path = NULL, at.the.time = 10,
import = FALSE, redo = FALSE, view.preset = NULL, pb = TRUE, ...)
```

## Arguments

- raven.path:

  A character string indicating the path of the directory in which to
  look for the 'Raven' executable file (where 'Raven' was installed).

- sound.files:

  character vector indicating the files that will be analyzed. If `NULL`
  (default) then 'Raven' will be run without opening any file.

- path:

  A character string indicating the path of the directory in which to
  look for the sound files. If not provided (default) the function
  searches into the current working directory. Default is `NULL`.

- at.the.time:

  Numeric vector of length 1 controlling how many files will be open in
  'Raven' at the same time. Note that opening too many files at once
  could make 'Raven' run out of memory. You need to close 'Raven' every
  time the batch of files is analyzed, so the next batch is opened.
  Default is 10. Not available in OSX (mac).

- import:

  Logical. Controls if the selection tables generated should be returned
  as a data frame into the R environment. This only works if the
  selections are saved in the "Selections" folder in the 'Raven'
  directory. This argument calls the
  [`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)
  internally. Additional arguments can be passed to
  [`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)
  to control the way the data is imported.

- redo:

  Logical. Controls whether only the subset of files with no 'Raven'
  selections (.txt file) in the 'Raven' 'selections' folder are analyzed
  (if `FALSE`). Useful when resuming the analysis. Default is `FALSE`.

- view.preset:

  Character string defining the 'Raven' view preset to be used. It
  should match exactly the name of the present in the 'Raven' folder
  'Presets/Sound Window'. If not provided the default view preset is
  used.

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- ...:

  Additional arguments to be passed to
  [`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)
  for customizing how selections are imported (ignored if
  `import = FALSE`).

## Value

If `import = TRUE` a data frame with the selections produced during the
analysis will be return as an data frame. See
[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)
for more details on how selections are imported.

## Details

The function runs 'Raven' sound analysis software (Cornell Lab of
Ornithology), opening many files simultaneously. 'Raven' will still run
if no sound files are provided (i.e. `sound.files = NULL`). At the end
of the analysis the data can be automatically imported back into R using
the 'import' argument. 'Raven' Pro must be installed. Note that 'Raven'
can also take sound files in 'mp3', 'flac' and 'aif' format. THIS
FUNCTION ONLY WORKS WITH RAVEN PRO 1.5 OR EARLIER VERSIONS.

## See also

[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md);
[`imp_syrinx`](https://marce10.github.io/Rraven/reference/imp_syrinx.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# save sound files
library(NatureSounds) 
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
tuneR::writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"), extensible = FALSE)

# here replace with the path where 'Raven' is install in your computer
raven.path <- "PATH_TO_RAVEN_DIRECTORY_HERE" 

# run function 
run_raven(raven.path = raven.path, sound.files = c("Phae.long1.wav", "Phae.long2.wav"),
 at.the.time = 2, import = T, name.from.file = T, ext.case = "upper", 
 all.data = TRUE, path = tempdir())  
 
#getting all the data
rav.dat <- run_raven(all.data = TRUE, raven.path = raven.path)
# View(rav.dat)
} # }
```
