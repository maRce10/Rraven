# Package index

## Import annotations

Functions to import Raven selection tables and other annotation formats

- [`imp_corr_mat()`](https://marce10.github.io/Rraven/reference/imp_corr_mat.md)
  : Import 'Raven' batch correlator output
- [`imp_raven()`](https://marce10.github.io/Rraven/reference/imp_raven.md)
  : Import 'Raven' selections
- [`imp_syrinx()`](https://marce10.github.io/Rraven/reference/imp_syrinx.md)
  : Import 'Syrinx' selections

## Export annotations

Export annotations from R to Raven selection table file format

- [`exp_empty_sels()`](https://marce10.github.io/Rraven/reference/exp_empty_sels.md)
  : Export a 'Raven' selection for all sound files in a folder
- [`exp_est()`](https://marce10.github.io/Rraven/reference/exp_est.md) :
  Export wave objects of extended selection tables as sound files
- [`exp_raven()`](https://marce10.github.io/Rraven/reference/exp_raven.md)
  : Export 'Raven' selections

## Format and manipulate annotations

Functions to format and manipulate annotation data frames

- [`extract_ts()`](https://marce10.github.io/Rraven/reference/extract_ts.md)
  : Extract time series parameters from data imported from 'Raven'
- [`fix_path()`](https://marce10.github.io/Rraven/reference/fix_path.md)
  : Modify sound file path in Raven's selection tables
- [`match_wav_case()`](https://marce10.github.io/Rraven/reference/match_wav_case.md)
  : Fix the extension case of sound files
- [`relabel_colms()`](https://marce10.github.io/Rraven/reference/relabel_colms.md)
  : Relabel columns to match the selection table format
- [`to_sound_selection()`](https://marce10.github.io/Rraven/reference/to_sound_selection.md)
  : Convert Raven's selection files into sound selection files

## Run Raven from R

Functions to run Raven analyses from R (they only work with Raven Pro
1.5)

- [`raven_batch_detec()`](https://marce10.github.io/Rraven/reference/raven_batch_detec.md)
  : Run 'Raven' batch detector
- [`run_raven()`](https://marce10.github.io/Rraven/reference/run_raven.md)
  : Open sound files in 'Raven' sound analysis software

## Built in datasets

Datasets included in Rraven

- [`selection_files`](https://marce10.github.io/Rraven/reference/selection_files.md)
  : A list of 'Raven' selection tables.
