#' Rraven: Exchange data and open sound files in 'Raven' from R
#' 
#' @description Rraven is a package designed to facilitate the exchange of data
#' between R and 'Raven' sound analysis software (Cornell Lab of Ornithology)
#' @details License: GPL (>= 2)  
#' 
#'  @section Functions:
#'   
#'   \code{\link{exp_raven}}: Export R selection tables into 'Raven' selection file format
#'  
#'   \code{\link{exp_empty_sels}}: Export a 'Raven' selection for all sound files in a folder
#'
#'   \code{\link{exp_est}}: Export wave objects of extended selection tables as sound files
#'   
#'   \code{\link{extract_ts}}: Extract time series parameters from data imported from Raven
#'   
#'   \code{\link{fix_path}}: Modify sound file path in Raven's selection tables 
#'   
#'   \code{\link{imp_corr_mat}}: Import 'Raven' batch correlator output 
#'   
#'   \code{\link{imp_raven}}: Import 'Raven' selections
#'   
#'   \code{\link{imp_syrinx}}: Import 'Syrinx' selections
#'
#'   \code{\link{match_wav_case}}: Fix the extension case of sound files
#'
#'   \code{\link{raven_batch_detec}}: Run 'Raven' batch detector
#'   
#'   \code{\link{relabel_colms}}: Relabel columns to match the selection table format
#'   
#'   \code{\link{run_raven}}: Open sound files in Raven
#'   
#'   \code{\link{to_sound_selection}}: Convert Raven's selection files into sound selection files
#'      
#' @import warbleR 
#' @import NatureSounds 
#' @import pbapply
#' @importFrom seewave duration pastew
#' @importFrom tuneR normalize writeWave
#' @importFrom stats approx as.ts
#' @importFrom utils read.table write.table
#' @author Marcelo Araya-Salas
#'   
#' Maintainer: Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#'   
#' @docType package
#' @name Rraven
