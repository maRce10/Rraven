# Relabel columns to match the selection table format

` relabel_colms` relabels columns to match the selection table format
(as in the R package
[warbleR](https://cran.r-project.org/package=warbleR))

## Usage

``` r
relabel_colms(X, extra.cols.name = NULL, extra.cols.new.name = NULL, 
khz.to.hz = FALSE, hz.to.khz = FALSE, waveform = FALSE)
```

## Arguments

- X:

  Data frame imported from Raven.

- extra.cols.name:

  Character vector with the names of additional columns to be relabeled.
  Default is `NULL`. 'extra.cols.new.name' must also be provided.

- extra.cols.new.name:

  Character vector with the new names for the additional columns to be
  relabeled. Default is `NULL`. 'extra.cols.name' must also be provided.

- khz.to.hz:

  Logical. Controls if frequency variables ('top.freq' and
  'bottom.freq') should be converted from kHz (the unit used by other
  bioacoustic analysis R packages like
  [warbleR](https://cran.r-project.org/package=warbleR)) to Hz (the unit
  used by Raven). Default is `FALSE`.

- hz.to.khz:

  Logical. Controls if frequency variables ('top.freq' and
  'bottom.freq') should be converted from Hz (the unit used by other
  bioacoustic analysis R packages like Raven) to kHz (the unit used by
  [warbleR](https://cran.r-project.org/package=warbleR)). Default is
  `FALSE`. Ignored if 'kHz.to.hz' is `TRUE`.

- waveform:

  Logical to control if 'waveform' related data should be included (this
  data is typically duplicated in 'spectrogram' data). Default is
  `FALSE` (not to include it).

## Value

The function returns the input data frame with new column names for time
and frequency 'coordinates' and sound files and selections.

## Details

This function relabels columns to match the selection table format to
match then ones used by other bioacoustic analysis R packages like
[warbleR](https://cran.r-project.org/package=warbleR).

## See also

[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md);
[`exp_raven`](https://marce10.github.io/Rraven/reference/exp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r

# Load data
data(selection_files)

#save 'Raven' selection tables in the temporary directory 
writeLines(selection_files[[5]], con = file.path(tempdir(), names(selection_files)[5]))

# \donttest{ 
#'# import data to R
rvn.dat <- imp_raven(all.data = TRUE, path = tempdir()) 

names(rvn.dat)
#>  [1] "Selection"              "View"                   "Channel"               
#>  [4] "Begin Time (s)"         "End Time (s)"           "Low Freq (Hz)"         
#>  [7] "High Freq (Hz)"         "Begin File"             "channel"               
#> [10] "Begin Path"             "File Offset (s)"        "Peak Freq Contour (Hz)"
#> [13] "old.selec"              "Rraven.labels"          "selec.file"            

# Select data for a single sound file
rvn.dat2 <-  relabel_colms(rvn.dat)

names(rvn.dat2)
#>  [1] "selec"                  "View"                   "Channel"               
#>  [4] "start"                  "end"                    "bottom.freq"           
#>  [7] "top.freq"               "sound.files"            "channel"               
#> [10] "Begin Path"             "File Offset (s)"        "Peak Freq Contour (Hz)"
#> [13] "old.selec"              "Rraven.labels"          "selec.file"            

# plus 1 additional column
rvn.dat2 <-  relabel_colms(rvn.dat, extra.cols.name = "selec.file", "Raven selection file")

names(rvn.dat2)
#>  [1] "selec"                  "View"                   "Channel"               
#>  [4] "start"                  "end"                    "bottom.freq"           
#>  [7] "top.freq"               "sound.files"            "channel"               
#> [10] "Begin Path"             "File Offset (s)"        "Peak Freq Contour (Hz)"
#> [13] "old.selec"              "Rraven.labels"          "Raven selection file"  

# plus 2 additional column 
rvn.dat2 <- relabel_colms(rvn.dat, extra.cols.name = c("selec.file", "View"), 
c("Raven selection file", "Raven view"))
 
names(rvn.dat2)
#>  [1] "selec"                  "Raven view"             "Channel"               
#>  [4] "start"                  "end"                    "bottom.freq"           
#>  [7] "top.freq"               "sound.files"            "channel"               
#> [10] "Begin Path"             "File Offset (s)"        "Peak Freq Contour (Hz)"
#> [13] "old.selec"              "Rraven.labels"          "Raven selection file"  
# }
```
