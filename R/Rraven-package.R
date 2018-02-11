#' Rraven: Exchange data and open sound files in 'Raven' from R
#' 
#' @description Rraven is a package designed to facilitate the exchange of data
#' between R and 'Raven' sound analysis software (Cornell Lab of Ornithology)
#' @details License: GPL (>= 2)  
#' 
#'  @section Functions:
#'   
#'   \code{\link{extract_ts}}: Extract time series parameters from data imported from Raven
#'   
#'   \code{\link{exp_raven}}: Export R selection tables into 'Raven' selection file format
#'   
#'   \code{\link{imp_corr_mat}}: Import 'Raven' batch correlator output 
#'   
#'   \code{\link{imp_raven}}: Importing 'Raven' selections
#'   
#'   \code{\link{imp_syrinx}}: Importing 'Syrinx' selections
#'   
#'   \code{\link{relabel_colms}}: Relabel columns to match the selection table format
#'   
#'   \code{\link{run_raven}}: Open sound files in Raven
#'      
#' @import warbleR 
#' @import dplyr
#' @import kableExtra
#' @importFrom pbapply pblapply
#' @importFrom stats approx as.ts
#' @importFrom utils read.table write.table
#' @author Marcelo Araya-Salas
#'   
#'   Maintainer: Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#'   
#' @docType package
#' @name Rraven
NULL
#> NULL 
#'
