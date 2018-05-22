#' Export 'Raven' selections
#' 
#' \code{exp_raven} exports selection tables as 'Raven' selection data in .txt format.
#' @usage exp_raven(X, path = NULL, file.name = NULL, khz.to.hz = TRUE, 
#' sound.file.path = NULL, single.file = TRUE, parallel = 1, pb = TRUE)
#' @param X Data frame containing columns for sound file (sound.files), selection (selec), start and end time of signals ('start' and 'end') and low and high frequency ('bottom.freq' and 'top.freq', optional). See example data 'selec.table' in the \code{\link{warbleR}}) package.
#' @param path A character string indicating the path of the directory in which to save the selection files.
#' If not provided (default) the function saves the file into the current working directory.
#' @param file.name Name of the output .txt file. If \code{NULL} then the sound file names are used instead. If multiple
#' selection files are generated (see 'single.file') then the sound files names are added to the provided 'file.name'.
#' @param khz.to.hz Logical. Controls if frequency variables should be converted from kHz (the unit used by other bioacoustic analysis R packages like \code{\link{warbleR}}) to Hz (the unit used by Raven). Default is \code{TRUE}.
#' @param sound.file.path A character string indicating the path of the 
#' directory containing the sound file(s). Providing this information allows
#'  to open both sound file and selection table simultaneously. This can be
#'  done by using the 'File > Open selection table' option in 'Raven' (or drag/drop the 
#' selection file into Raven). Default is \code{NULL}. This argument is required when
#' exporting selections from multiple sound files.
#' @param single.file Logical. Controls whether a single selection file (\code{TRUE}; default)
#' or multiple selection files for each sound files (\code{FALSE}, hence, only applicable
#' when several sound files are included in 'X') are generated. Note that
#' 'sound.file.path' must be provided when exporting several sound files into a single selection file as the
#' duration of the sound files is required.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return The function saves a selection table in '.txt' format that can be 
#' directly opened in Raven. If several sound files are available users can either 
#' export them as a single selection file or as multiple selection files (one for each sound file). 
#' No objects are returned in the R environment.
#' @details The function exports selection tables (as the ones used in the R 
#' package \code{\link{warbleR}}) into the 'Raven' selection file format ('.txt').
#'  This can be useful to obtain additional Raven
#' measurements on existing selections by adding new measurements to the 
#' selection table once in Raven. Note that selection labels must be numeric and unduplicated 
#' when exporting them to Raven. If that is not the case the function will
#' relabeled the selections and the previous selection labels will be retained in a new column('old.selec').
#' @seealso \code{\link{imp_raven}}; \code{\link{imp_syrinx}} 
#' @export
#' @name exp_raven
#' @examples
#' 
#' # Load data
#' library(warbleR)
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selec.table"))
#' 
#' # Select data for a single sound file
#' st1 <- selec.table[selec.table$sound.files == "Phae.long1.wav",]
#' 
#' # Export data of a single sound file
#' exp_raven(st1, file.name = "Phaethornis 1")
#' 
#' writeWave(Phae.long1, "Phae.long1.wav", extensible = FALSE) #save sound files 
#' writeWave(Phae.long2, "Phae.long2.wav", extensible = FALSE)
#' writeWave(Phae.long3, "Phae.long3.wav", extensible = FALSE)
#' writeWave(Phae.long4, "Phae.long4.wav", extensible = FALSE)
#' 
#' exp_raven(X = selec.table, file.name = "Phaethornis multiple sound files",
#'  single.file = TRUE, sound.file.path = getwd())
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-7-2017
exp_raven <- function(X, path = NULL, file.name = NULL, khz.to.hz = TRUE, sound.file.path = NULL, single.file = TRUE, parallel = 1, pb = TRUE){
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  #check path to working directory
  if(is.null(path)) path <- getwd() else {if(!file.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  #if X is not a data frame
  if(!class(X) == "data.frame") stop("X is not a data frame")
  
  if(!all(c("sound.files", "selec", 
            "start", "end") %in% colnames(X))) 
    stop(paste(paste(c("sound.files", "selec", "start", "end")[!(c("sound.files", "selec", 
                                                                   "start", "end") %in% colnames(X))], collapse=", "), "column(s) not found in data frame"))
  
  #stop if more than 1 sound file is found in X
  if(length(unique(X$sound.files)) > 1 & is.null(sound.file.path)) stop("'sound.file.path' must be provided when including selections from multiple sound files")
  
  if(length(unique(X$sound.files)) == 1) single.file <- TRUE
  
  if(!is.null(sound.file.path))
{    
    #count number of sound files in working directory and if 0 stop
    recs.wd <- list.files(path = sound.file.path, pattern = "\\.wav$", ignore.case = TRUE)
  if(!all(unique(X$sound.files) %in% recs.wd)) 
    stop("Some (or all) .wav files are not in the working directory")
  }
  
  if (any(is.na(X$start), is.na(X$end))) stop("'NAs' found in start and/or end column")
  
  if (any(!is.numeric(X$start), !is.numeric(X$end))) stop("start and/or end column(s) are not numeric")
  
  # convert to Hz
  if("bottom.freq" %in% names(X) & khz.to.hz)
  X$bottom.freq <- X$bottom.freq * 1000

  # convert to Hz
  if("top.freq" %in% names(X) & khz.to.hz)
    X$top.freq <- X$top.freq * 1000
  
  # change column names
  rvn.nms <- c("Begin File", "Selection", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)")
  wblr.nms <- c("sound.files", "selec", "start", "end", "bottom.freq", "top.freq")
  
  for(i in 1:length(rvn.nms))
    names(X)[names(X) == wblr.nms[i]] <- rvn.nms[i]
  
  # add View and channel column
  X$View <- "Spectrogram 1"  
  X$Channel <- 1  
  
  mtch <- match(c( "Selection", "View", "Channel", "Begin Time (s)", "End Time (s)", "Low Freq (Hz)", "High Freq (Hz)"), names(X))
  
  X <- X[,c(mtch[!is.na(mtch)], base::setdiff(1:ncol(X), mtch))]
  
  if(!is.null(sound.file.path))
  {
    X$'Begin Path' <- file.path(sound.file.path, X$'Begin File')
    
    X$'File Offset' <- X$'Begin Time (s)'
    
    if(length(unique(X$'Begin File')) > 1 & single.file)
    {
      durs <- warbleR::wavdur(path = sound.file.path)
    durs$cumdur <- cumsum(durs$duration)
    durs <- durs[durs$sound.files %in% X$'Begin File', ]
    
    # calculate file offset
    out <- lapply(1:nrow(durs), function(x) {
      
      Y <- X[X$`Begin File` == durs$sound.files[x], ]
      Y$'File Offset' <- Y$`Begin Time (s)` 
      
      if(x > 1) {
        Y$`Begin Time (s)` <-  Y$`Begin Time (s)` + durs$cumdur[x - 1]
        Y$`End Time (s)` <-  Y$`End Time (s)` + durs$cumdur[x - 1]}
      
    return(Y)
        })     
      
    X <- do.call(rbind, out)
    } 
  }
  
 if(!is.null(sound.file.path))
   if(!is.numeric(X$Selection) | any(duplicated(X$Selection)))
   {
     X$old.selec <- X$Selection 
     X$Selection <- seq_len(nrow(X))
   }
 
if(single.file | nrow(X) == 1)
  row.list <- matrix(c(1, nrow(X)), nrow = 1) else 
  {
    e <- which(!duplicated(X$`Begin File`))  
    e2 <- c(e[2:length(e)] - 1, nrow(X))
    row.list <- data.frame(e, e2, sound.files = X$`Begin File`[!duplicated(X$`Begin File`)])
    }

  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel  
  
  out <- pbapply::pblapply(seq_len(nrow(row.list)), cl = cl, function(x){
  
  if(is.null(file.name)) file.name2 <- "" else file.name2 <- file.name
  
  if(!is.null(path))
    file.name2 <- file.path(path, file.name2)
  
  if(nrow(row.list) > 1)
    file.name2 <- file.path(file.name2, row.list$sound.files[x]) else
      if(is.null(file.name)) file.name2 <- file.path(file.name2, X$`Begin File`[1])
  
  # if file name does not contain the extension
  if(substr(file.name2, start = nchar(file.name2)- 3, nchar(file.name2)) != ".txt")
    file.name2 <- paste0(file.name2, ".txt")
  
  utils::write.table(x = X[c(row.list[x, 1] : row.list[x, 2]),], sep = "\t", file = file.name2, row.names = FALSE, quote = FALSE)  
 })
  
}