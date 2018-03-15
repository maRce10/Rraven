#' Extract time series parameters from data imported from 'Raven' 
#' 
#' \code{extract_ts} extracts time series parameters from data imported from 'Raven' bioacoustic software.
#' @usage extract_ts(X, ts.column, equal.length = FALSE, as.time.series = FALSE, 
#' length.out = 30, parallel = 1, pb = TRUE)
#' @param X Data frame imported from Raven. It should include at least columns for: sound file names, selection labels,
#' a parameters encoded as a time series (e.g. several numbers separated by semicolon) 
#' @param ts.column Name of the column with the time series data to be extracted. 
#' Default is \code{NULL}.
#' @param equal.length Logical. Controls whether time series are kept as in the original data (most of the 
#' time with unequal lengths) or numbers are interpolated to equalize series length (using the \code{\link[stats]{approx}} function). All series will be interpolated to match the length of the longest series in the data. Default is \code{FALSE}.
#' @param as.time.series Logical. Controls if data is converted to the time series format (using the \code{\link[stats]{as.ts}} function). Default is \code{FALSE}.
#' @param length.out A numeric vector of length 1 giving the number of measurements to be
#' interpolated (the length of the time series). default is 30. Ignored if equal.length is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame with columns for sound file name (sound.files), selection label (selec) and the time series for each selection.
#' @details The function extracts parameters enconded as time series in 'Raven' selection files. The resulting data frame can be directly input into functions for time series analysis of acoustic signals as \code{\link[warbleR]{dfDTW}}.
#' @seealso \code{\link{imp_raven}}; \code{\link{exp_raven}} 
#' @export
#' @name extract_ts
#' @examples
#' 
#' # Load data
#' data(selection_files)
#' 
#' #save 'Raven' selection tables in the temporary directory 
#' writeLines(selection_files[[5]], con = names(selection_files)[5])
#'
#' \donttest{
#' # import data to R
#'rvn.dat <- imp_raven(all.data = TRUE) 
#'
#'# Peak freq dif length
#'extract_ts(X = rvn.dat, ts.column = "Peak.Freq.Contour..Hz.")
#' 
#'# Peak freq equal length
#'extract_ts(X = rvn.dat, ts.column = "Peak.Freq.Contour..Hz.", equal.length = T)
#'  
#' # Peak freq equal length 10 measurements
#' extract_ts(X = rvn.dat, ts.column = "Peak.Freq.Contour..Hz.", 
#' equal.length = TRUE, length.out = 10) 
#'} 
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-7-2017
extract_ts <- function(X, ts.column, equal.length = FALSE, as.time.series = FALSE,
                       length.out = 30, parallel = 1, pb = TRUE){
  
  #if X is not a data frame
  if(!class(X) == "data.frame") stop("X is not a data frame")
  
  #check if ts.column exists
  if(!any(names(X) == ts.column)) stop("'ts.column' not found")
  
  #remove waveform rows
  if(any(names(X) == "View"))
  X <- X[grep("Waveform", X$View, ignore.case = TRUE, invert = TRUE), ]
  
  X <- X[, c(grep("sound.files|selec|Selection|.File$", names(X))[1:2], which(names(X) == ts.column))]
  
  #split ts
  out <- strsplit(as.character(X[, ncol(X)]), ";",fixed=T)
  out <- lapply(out, as.numeric)
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel
  
  if(equal.length)
  {
    out <-  pbapply::pblapply(out, cl = cl, function(x) stats::approx(x, 1:length(x), n = length.out)$x)
    
  } else out <-  pbapply::pblapply(out, cl = cl, function(x) c(x, rep(NA, max(sapply(out, length)) - length(x))))
    
  
  Y <- as.data.frame(do.call(rbind, out))
  names(Y) <- paste(abbreviate(ts.column), 1:ncol(Y))
  names(Y) <- gsub("\\. ", "\\.", names(Y))
    
  if(as.time.series) {
    Y <- stats::as.ts(Y)
    rownames(Y) <- paste(X[,grep("sound.files|.File$", names(X))[1]], X[,grep("sound.files|selec|Selection|.File$", names(X))[1]], sep = "-")
    return(Y)} else {
      Z <- data.frame(X[,grep("sound.files|selec|Selection|.File$", names(X))[1:2]], Y)
      Z <- Z[, c(grep("file", names(Z), ignore.case = TRUE), grep("^selec", names(Z), ignore.case = TRUE), 3:ncol(Z))]
      names(Z)[1:2] <- c("sound.files", "selec")
      
      return(Z)
    }  
}