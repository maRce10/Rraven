# Extract time series parameters from data imported from 'Raven'

`extract_ts` extracts time series parameters from data imported from
'Raven' bioacoustic software.

## Usage

``` r
extract_ts(X, ts.column, equal.length = FALSE, as.time.series = FALSE, 
length.out = 30, parallel = 1, pb = TRUE)
```

## Arguments

- X:

  Data frame imported from Raven. It should include at least columns
  for: sound file names, selection labels, a parameters encoded as a
  time series (e.g. several numbers separated by semicolon)

- ts.column:

  Name of the column with the time series data to be extracted. Default
  is `NULL`.

- equal.length:

  Logical. Controls whether time series are kept as in the original data
  (most of the time with unequal lengths) or numbers are interpolated to
  equalize series length (using the
  [`approx`](https://rdrr.io/r/stats/approxfun.html) function). All
  series will be interpolated to match the length of the longest series
  in the data. Default is `FALSE`.

- as.time.series:

  Logical. Controls if data is converted to the time series format
  (using the [`as.ts`](https://rdrr.io/r/stats/ts.html) function).
  Default is `FALSE`.

- length.out:

  A numeric vector of length 1 giving the number of measurements to be
  interpolated (the length of the time series). default is 30. Ignored
  if equal.length is `FALSE`.

- parallel:

  Numeric. Controls whether parallel computing is applied. It specifies
  the number of cores to be used. Default is 1 (i.e. no parallel
  computing).

- pb:

  Logical argument to control progress bar. Default is `TRUE`.

## Value

A data frame with columns for sound file name (sound.files), selection
label (selec) and the time series for each selection.

## Details

The function extracts parameters encoded as time series in 'Raven'
selection files. The resulting data frame can be directly input into
functions for time series analysis of acoustic signals as
[`freq_DTW`](https://marce10.github.io/warbleR/reference/freq_DTW.html).

## See also

[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md);
[`exp_raven`](https://marce10.github.io/Rraven/reference/exp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load data
data(selection_files)

#save 'Raven' selection tables in the temporary directory 
writeLines(selection_files[[5]], con = file.path(tempdir(), names(selection_files)[5]))

# import data to R
rvn.dat <- imp_raven(all.data = TRUE, path = tempdir())

# Peak freq dif length
extract_ts(X = rvn.dat, ts.column = "Peak Freq Contour (Hz)")

# Peak freq equal length
extract_ts(X = rvn.dat, ts.column = "Peak Freq Contour (Hz)", equal.length = T)
 
# Peak freq equal length 10 measurements
extract_ts(X = rvn.dat, ts.column = "Peak Freq Contour (Hz)", 
equal.length = TRUE, length.out = 10) 
} # } 
```
