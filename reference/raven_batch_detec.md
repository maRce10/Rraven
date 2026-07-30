# Run 'Raven' batch detector

`raven_batch_detec` Runs 'Raven' batch detector on multiple sound files
sequentially

## Usage

``` r
raven_batch_detec(raven.path = NULL, sound.files, path = NULL, 
detector.type, detector.preset = "Default",
view.preset = "Default", relabel_colms = TRUE, pb = TRUE, parallel = 1)
```

## Arguments

- raven.path:

  A character string indicating the path of the directory in which to
  look for the 'Raven' executable file (where 'Raven' was installed).

- sound.files:

  character vector indicating the files that will be analyzed. In OSX
  (mac) only one file at the time can be run (use loops instead!). If
  `NULL` (default) then 'Raven' will be run without opening any file.

- path:

  A character string indicating the path of the directory in which to
  look for the sound files. If not provided (default) the function
  searches into the current working directory. Default is `NULL`.

- detector.type:

  Character string specifying the type of detector to be called. There
  are 3 options available in 'Raven': 'Amplitude Detector', 'Band
  Limited Energy Detector' and 'Band Limited Entropy Detector'. Must be
  provided.

- detector.preset:

  Character string specifying the name of the customized detector to be
  called. If `NULL` (default) then the 'Default' detector for the
  specific detector type is used (see 'detector.type' argument). Custom
  detectors must be found in one of the default 'Raven' detector
  directories (usually within "'raven.path'/Presets/Detector").

- view.preset:

  Character string specifying the name of the window preset to be used.
  Not require for 'Amplitude Detector' (see 'detector.type' argument).
  If `NULL` (default) then the 'Default' window preset is used.

- relabel_colms:

  Logical. If `TRUE` (default) columns are labeled to match the
  selection table format from the acoustic analysis package
  [warbleR](https://cran.r-project.org/package=warbleR)

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

## Value

A data frame with the selections produced during the detection. See
[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md)
for more details on how selections are imported.

## Details

The function runs 'Raven' sound analysis software (Cornell Lab of
Ornithology), detector on multiple sound files sequentially. 'Raven' Pro
must be installed. Note that batch detection in 'Raven' can also take
sound files in 'mp3', 'flac' and 'aif' format. THIS FUNCTION ONLY WORKS
WITH RAVEN PRO 1.5 OR EARLIER VERSIONS.

## See also

[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md);
[`imp_syrinx`](https://marce10.github.io/Rraven/reference/imp_syrinx.md);
[`run_raven`](https://marce10.github.io/Rraven/reference/run_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{

# here replace with path where 'Raven' is installed in your computer
raven.path <- "PATH_TO_RAVEN_DIRECTORY_HERE" 

# Run detector on raven example sound files

# single sound file using 'Amplitude Detector' 
detec.res <- raven_batch_detec(raven.path = raven.path, 
sound.files = "BlackCappedVireo.aif", path = file.path(raven.path, "Examples"), 
detector.type = "Amplitude Detector")

# on raven examples  2 files
detec.res <- raven_batch_detec(raven.path = raven.path, 
sound.files = c("BlackCappedVireo.aif", "CanyonWren.wav"), 
path = file.path(raven.path, "Examples"), detector.type = "Amplitude Detector")

# using 'Band Limited Energy Detector' 
detec.res <- raven_batch_detec(raven.path = raven.path, 
sound.files = c("BlackCappedVireo.aif", "CanyonWren.wav"), 
path = file.path(raven.path, "Examples"), detector = "Band Limited Energy Detector")
} # }
```
