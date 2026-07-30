# Import 'Raven' batch correlator output

`imp_corr_mat` imports the output of 'Raven' batch correlator.

## Usage

``` r
imp_corr_mat(file, path = NULL)
```

## Arguments

- file:

  A character string with the name of the output '.txt' file generated
  by Raven.

- path:

  A character string indicating the path of the directory in which to
  look for the text files. If not provided (default) the function
  searches into the current working directory.

## Value

A list with 2 matrices. The first one contains the correlation
coefficients and the second one the time lags of the peak correlations.

## Details

The function imports the output of a batch correlation routine in Raven.
Both the correlation and lag matrices contained in the output ' .txt'
file are read and both waveform and spectrogram (cross-correlation)
correlations can be imported.

## See also

[`imp_raven`](https://marce10.github.io/Rraven/reference/imp_raven.md);
[`exp_raven`](https://marce10.github.io/Rraven/reference/exp_raven.md)

## Author

Marcelo Araya-Salas (<marcelo.araya@ucr.ac.cr>)

## Examples

``` r
if (FALSE) { # \dontrun{ 
# Load data
library(NatureSounds)
data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))

tuneR::writeWave(Phae.long1, file.path(tempdir(), 
"Phae.long1.wav"), extensible = FALSE) #save sound files 
tuneR::writeWave(Phae.long2, file.path(tempdir(), 
"Phae.long2.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long3, file.path(tempdir(), 
"Phae.long3.wav"), extensible = FALSE)
tuneR::writeWave(Phae.long4, file.path(tempdir(), 
"Phae.long4.wav"), extensible = FALSE)

#create new folder to put cuts
dir.create(file.path(tempdir(), "cuts"))

# cut files
cut_sels(X = lbh_selec_table, mar = 0.05, path = tempdir(),
 dest.path = file.path(tempdir(), "cuts"))

#Now run 'Raven' batch correlator un the cuts and save the output in the same folder

# Import output (change the name of the file if you used a different one)
xcorr.rav <- imp_corr_mat(file = "BatchCorrOutput.txt", 
path = file.path(tempdir(), "cuts"))

# check results
  
## correlation matrix
xcorr.rav[[1]]

## time lag matrix
xcorr.rav[[2]]
} # }
```
