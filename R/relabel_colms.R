#' Relabel columns to match the selection table format 
#' 
#' \code{ relabel_colms} relabels columns to match the selection table format (as in the R package \code{\link{warbleR}})
#' @usage  relabel_colms(X, extra.cols.name = NULL, extra.cols.new.name = NULL, 
#' khz.to.hz = FALSE, hz.to.khz = FALSE, waveform = FALSE)
#' @param X Data frame imported from Raven.
#' @param extra.cols.name Character vector with the names of additional columns to be relabeled. Default is \code{NULL}.
#' 'extra.cols.new.name' must also be provided.
#' @param extra.cols.new.name Character vector with the new names for the additional columns to be relabeled. 
#' Default is \code{NULL}. 'extra.cols.name' must also be provided.
#' @param khz.to.hz Logical. Controls if frequency variables ('top.freq' and 'bottom.freq') should be converted from kHz 
#' (the unit used by other bioacoustic analysis R packages like \code{\link{warbleR}}) to Hz (the unit used by Raven). 
#' Default is \code{FALSE}.
#' @param hz.to.khz Logical. Controls if frequency variables ('top.freq' and 'bottom.freq') should be converted from Hz 
#' (the unit used by other bioacoustic analysis R packages like Raven) to kHz (the unit used by \code{\link{warbleR}}). 
#' Default is \code{FALSE}. Ignored if 'kHz.to.hz' is \code{TRUE}.
#' @param waveform Logical to control if 'waveform' related data should be included (this data is typically duplicated in 'spectrogram' data).  Default is \code{FALSE} (not to include it).
#' @return The function returns the input data frame with new column names for time and frequency 'coordinates' and sound files and selections.
#' @details This function relabels columns to match the selection table format to match then ones used by other bioacoustic analysis R packages like \code{\link{warbleR}}. 
#' @seealso \code{\link{imp_raven}}; \code{\link{exp_raven}} 
#' @export
#' @name  relabel_colms
#' @examples
#' 
#' # Load data
#' data(selection_files)
#' 
#' #save 'Raven' selection tables in the temporary directory 
#' writeLines(selection_files[[5]], con = names(selection_files)[5])
#'
#' \donttest{ 
#' #'# import data to R
#'rvn.dat <- imp_raven(all.data = TRUE) 
#'
#' names(rvn.dat)
#' 
#' # Select data for a single sound file
#' rvn.dat2 <-  relabel_colms(rvn.dat)
#' 
#' names(rvn.dat2)
#' 
#' # plus 1 additional column
#' rvn.dat2 <-  relabel_colms(rvn.dat, extra.cols.name = "selec.file", "Raven selection file")
#'
#' names(rvn.dat2)
#' 
#' # plus 2 additional column 
#' rvn.dat2 <- relabel_colms(rvn.dat, extra.cols.name = c("selec.file", "View"), 
#' c("Raven selection file", "Raven view"))
#'  
#'names(rvn.dat2)
#'}
#' 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on nov-7-2017

 relabel_colms <- function(X, extra.cols.name = NULL, extra.cols.new.name = NULL, khz.to.hz = FALSE, hz.to.khz = FALSE,
                           waveform = FALSE){
  
   op.dig <- options(digits = 6)
   
  #if X is not a data frame
  if (!class(X) == "data.frame") stop("X is not a data frame")
  
  # if not extra.cols.new.name and extra.cols.name are provided
  if (any(!is.null(extra.cols.name) & is.null(extra.cols.new.name), is.null(extra.cols.name) & !is.null(extra.cols.new.name))) stop("if either 'extra.cols.name' or 'extra.cols.new.name' are provided the other must be provided as well")

  # if not the same length
  if (length(extra.cols.new.name) != length(extra.cols.name)) 
    stop("'extra.cols.name' and 'extra.cols.new.name' must have the same length")
    
  # remove waveform rows
  if (!waveform & any(names(X) == "View"))
  X <- X[grep("Waveform", X$View, ignore.case = TRUE, invert = TRUE), ]
  
  # change column names try 1
  rvn.nms <- c("Selection", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)")
  wblr.nms <- c("selec", "start", "end", "bottom.freq", "top.freq")
  
  for(i in 1:length(rvn.nms))
    names(X)[names(X) == rvn.nms[i]] <- wblr.nms[i]

  # change column names try 1
  rvn.nms <- c("Selection", "Begin.Time..s.", "End.Time..s.", "Low.Freq..Hz.", "High.Freq..Hz.")
  wblr.nms <- c("selec", "start", "end", "bottom.freq", "top.freq")
  
  for(i in 1:length(rvn.nms))
    names(X)[names(X) == rvn.nms[i]] <- wblr.nms[i]
  
  if (!is.null(extra.cols.name))
  for(i in 1:length(extra.cols.name))
    names(X)[names(X) == extra.cols.name[i]] <- extra.cols.new.name[i]
  
  
  
  #convert sound file column name
  names(X)[grep("\\.File$", names(X))[1]] <- "sound.files"
  
  # convert to Hz
  if ("bottom.freq" %in% names(X) & khz.to.hz)
    X$bottom.freq <- X$bottom.freq * 1000
  
  # convert to Hz
  if ("top.freq" %in% names(X) & khz.to.hz)
    X$top.freq <- X$top.freq * 1000
  
  # convert to kHz
  if ("bottom.freq" %in% names(X) & !khz.to.hz & hz.to.khz)
    X$bottom.freq <- X$bottom.freq / 1000
  
  # convert to kHz
  if ("top.freq" %in% names(X) & !khz.to.hz & hz.to.khz)
    X$top.freq <- X$top.freq / 1000
  
  
return(X)  
  
}

