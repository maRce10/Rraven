#' Import 'Raven' selections
#' 
#' \code{imp_raven} imports 'Raven' selection files simultaneously from many files. Files must be in '.txt' format.
#' @usage imp_raven(path = NULL, sound.file.col = NULL, all.data = FALSE, recursive = FALSE,
#'  name.from.file = FALSE, ext.case = NULL, freq.cols = TRUE, waveform = FALSE, 
#'  parallel = 1, pb = TRUE, unread = FALSE, rm.dup = FALSE)  
#' @param path A character string indicating the path of the directory in which to look for the 'Raven' selection (text) files. 
#' If not provided (default) the function searches into the current working directory.
#' @param sound.file.col A character string with the name of the column containing the sound files in 
#' the selection text files. Default is \code{NULL}. If provided, the output data
#' frame will contained all columns needed for subsequent analysis in the acoustic analysis package \code{\link{warbleR}}. 
#' Duplicated rows, as when "waveform" and "spectrogram" information are included for the same selection, will be removed.
#' All selection files must contain "Selection", "Begin.Time" and "End.Time" columns.
#' @param all.data Logical. If \code{TRUE} all columns in the selection files are returned, 
#' keeping the name columns as in the 'Raven' files. Default is \code{FALSE}. Columns absent in some selection files will 
#' be filled with NA's. Note that when Raven selection files contain data from multiple sound files the  "Begin.Time" and 
#' "End.Time" columns are relative to the position of the sound files in sequence of files riginally displayed by Raven.
#' This data is corrected to the absolute time for each sound file only if \code{all.data = FALSE}.
#' @param recursive Logical. If \code{TRUE} the listing recurse into sub-directories.
#' @param name.from.file Logical. If \code{TRUE} the sound file names are extracted from the selection text file name. 
#' It asssumes that selections files contained the suffix "Table.1.selections.txt" or "selections.txt". 
#' Note that by default it will assume that the extension file name is ".wav". This can be control using the
#' argumet 'ext.wav'. Default is \code{FALSE}). Ignored if sound.file.col' is provided and/or all.data is \code{TRUE}).
#' @param ext.case Character string of length 1 to specify whether sound file extensions are in upper or lower case. This should match the extension of the
#' of the .wav files from which the selection were made. It must be either 'upper' or 'lower'. Only needed when 'name.from.file' is \code{TRUE}. 
#' Ignored if 'sound.file.col' is provided and/or all.data is \code{TRUE}.
#' @param freq.cols Logical. If \code{TRUE} 'Low Freq' and 'High Freq' columns are also imported. Ignored if all.data is \code{TRUE}.
#' @param waveform Logical to control if waveform view data should be included (this data is typically duplicated in spectrogram view data).  Default is \code{FALSE} (not to include it).
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param unread Logical. If \code{TRUE} a list (instead of a data frame). The first element of the list contains the selections\
#' whole the second one is a character vector with the names of sound files that could not be read. Default is \code{FALSE}.
#' @param rm.dup Logical. If \code{TRUE} duplicated rows and columns are removed. Usefull when 
#' selection files have been duplicated. Default is \code{FALSE}. 
#' @return A single data frame with information of the selection files. If \code{unread = TRUE} the function returns a list of length 2 with
#'  the selection data frame and a vector with the names of files that could not be read (see 'unread' argument).  
#'  If 'all.data' argument is set to \code{FALSE} the data frame contains the following columns: selec, start, end, and selec.file. 
#'  If sound.file.col is provided the data frame  will also contain a 'sound.files' column. In addition, all rows with duplicated 
#'  data are removed. This is useful when both spectrogram and waveform views are included in the 'Raven' selection files. If all.data is set to \code{TRUE} then all columns in the 'Raven' selection files are returned. 
#'  If individual selection files contain information about multiple sound files the function will correct the time
#'  parameters (start and end) only if 1) the 'File Offset (s)' is also included in the selection file and 2) the sound file column has been provided (using 'sound.file.col' or 'name.from.file' arguments) and 3) 'all.data' is \code{FALSE}.
#' @details The function import 'Raven' selection data from many files simultaneously. Files must be in '.txt' format. Selection 
#' files including data from mulitple recordings can also be imported. 
#' @seealso \code{\link{imp_syrinx}} 
#' @export
#' @name imp_raven
#' @examples
#' 
#' #load data 
#' data(selection_files)
#' 
#' # set temporary directory
#' # setwd(tempdir())
#' 
#' #save 'Raven' selection tables in the temporary directory 
#' out <- lapply(1:2, function(x) 
#' writeLines(selection_files[[x]], con = names(selection_files)[x]))
#' 
#' \donttest{
#'#providing the name of the column with the sound file names
#'rvn.dat <- imp_raven(sound.file.col = "Begin.File", all.data = FALSE)
#' 
#' # View(rvn.dat)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-7-2017

imp_raven <- function(path = NULL, sound.file.col = NULL, all.data = FALSE, 
                    recursive = FALSE, name.from.file = FALSE, ext.case = NULL, 
                    freq.cols = TRUE, waveform = FALSE, parallel = 1, pb = TRUE, 
                    unread = FALSE, rm.dup = FALSE) 
{
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else {if (!dir.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  
  
  if (!is.null(ext.case)) 
    if (!ext.case %in% c("upper", "lower")) stop("'ext.case' should be either 'upper' or 'lower'") else
    ext <- if (ext.case == "upper") "WAV" else "wav"
  
  if (is.null(ext.case) & name.from.file) stop("'ext.case' must be provided when name.from.file is TRUE")
  
  sel.txt <- list.files(pattern = ".txt$", full.names = TRUE, recursive = recursive, ignore.case = TRUE)
  
  sel.txt2 <- list.files(pattern = ".txt$", full.names = FALSE, recursive = recursive, ignore.case = TRUE)
  
  if (length(sel.txt) == 0) stop("No selection .txt files in working directory/'path' provided")
  
  options(warn = -1)
  
  read_sels_FUN <- function(i, sel.txt, sel.txt2, all.data, freq.cols, sound.file.col, name.from.file)
  {  
    # read selection file
    a <- try(utils::read.delim(sel.txt[i], header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
    
    if (class(a) != "try-error")
    {   
      if (!all.data) { # read only file, time and freq columns
        
        if (name.from.file) # get sound file name from selection file
        {  
        sound.files <- gsub("Table\\.([0-9]+)\\.selections.txt$", ext, sel.txt2[i])
        sound.files <- gsub(".selections.txt$", ext, sound.files) 
        }     
  
        if (!is.null(sound.file.col)) # get sound file name from sound file column provided
          if (length(grep(sound.file.col, colnames(a))) == 0) stop(paste0("'",sound.file.col , "' column provided in 'sound.file.col' not found. Make sure all files contain that column and that no other '.txt' files are found in that directory")) else
          sound.files <- a[, grep(sound.file.col, colnames(a), ignore.case = TRUE)]

        # put data in data frame      
        d <- try(data.frame(channel = a[, grep("channel", colnames(a), ignore.case = TRUE)],
                            selec = a[,grep("Selection$",colnames(a), ignore.case = TRUE)],
                            start = a[,grep("Begin.Time",colnames(a), ignore.case = TRUE)],
                            end = a[, grep("End.Time",colnames(a), ignore.case = TRUE)], selec.file = sel.txt2[i], stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
        
        # add sound file column and offset info
        if (exists("sound.files")) {
          d <- data.frame(sound.files, d) 
    
          # fix start end if multiple files are found
         if (length(unique(sound.files)) > 1)
          { 
           if (!any(grepl("offset", names(a), ignore.case = TRUE))) stop(paste0("selections files from multiple sound files must contain an 'Offset' column (check ", sel.txt[i],")"))
          
          d$file.offset.DELETE <- as.numeric(a[, grep("Offset", colnames(a), ignore.case = TRUE)])
          d$end <- as.numeric(d$end) - as.numeric(d$start)
          d$start <- d$file.offset.DELETE
          d$end <- d$end + d$start
          }
          }
        
        # add frequency columns 
        if (freq.cols)
        {
        try(d$bottom.freq <- a[, grep("Low.Freq", colnames(a), ignore.case = TRUE)]/ 1000, silent = TRUE)
        try(d$top.freq <- a[, grep("High.Freq", colnames(a), ignore.case = TRUE)]/ 1000, silent = TRUE)
          }
        
        if (all(c("High.Freq", "Low.Freq") %in% names(d)))
          d <- d[c(1:(ncol(d) - 3), ncol(d):(ncol(d)-1), ncol(d) -2 )]
      
        } else { # if all data needed
          # read data
          d <- try(data.frame(a, selec.file = sel.txt2[i], stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE) 
  
          if (class(d) == "try-error") d <- NA
      }
      } else d <- NA
      return(d)
 }

  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel  
  
  clist <- pbapply::pblapply(seq_len(length(sel.txt)), cl = cl, function(i) {
    read_sels_FUN(i, sel.txt = sel.txt, sel.txt2 = sel.txt2, all.data = all.data, freq.cols = freq.cols, 
                  sound.file.col = sound.file.col, name.from.file = name.from.file)
  })
    
# determine files that could not be read
error.files <- sel.txt2[!sapply(clist, is.data.frame)]    
    
# remove NAs    
clist <- clist[sapply(clist, is.data.frame)]

if (length(clist) > 0){

  # relabel duplicated column names
  clist <- lapply(clist, function(x) {
      
    # get column names
      nms <- names(x)
    # rename columns with NAs  
      nms[is.na(nms)] <- paste0("unnamed_", seq(length(which(is.na(nms)))))  
      
      names(x) <- nms
      
          if (any(duplicated(nms)))
    {
      dp_nms <- nms[duplicated(nms)]
      
      for(i in dp_nms)
        names(x)[names(x) == i] <- paste0(names(x)[names(x) == i],  seq_len(length(which(names(x) == i))))
    }
    return(x)
  }) 
  
  # determine all column names in all selection tables    
  cnms <- unique(unlist(lapply(clist, names)))    
  
  # add columns that are missing to each selection table
  clist <- lapply(clist, function(X)
  {
nms <- names(X)
if (length(nms) != length(cnms))  
for(i in cnms[!cnms %in% nms]) {
  X <- data.frame(X,  NA, stringsAsFactors = FALSE, check.names = FALSE)
  names(X)[ncol(X)] <- i
  }

return(X)
})
    
b <- do.call("rbind", clist)

if (rm.dup)
  b <- b[!duplicated(b), ]

rownames(b) <- 1:nrow(b)

# order columns
clm <- match(names(b), c("sound.files", "selec", "start", "end", "bottom.freq", "top.freq"))
clm <- clm[!is.na(clm)]

if (length(clm) > 1)
b <- b[, c(clm, base::setdiff(1:ncol(b), clm))]

# remove file.offset column
if (!all.data)  b$file.offset.DELETE <- NULL

if (!waveform & all.data)
  b <- b[grep("Waveform", b$View, ignore.case = TRUE, invert = TRUE), ]

if (!all.data & !is.null(b$sound.files))
  b$sound.files <- basename(as.character(b$sound.files))
}  else b <- NULL

if (length(error.files) == length(sel.txt2)) cat("Not a single file could be read") else
if (length(error.files) > 0 & !unread) cat(paste(length(error.files), "file(s) could not be read:", paste(error.files, collapse = "/")))

if (unread) return(list(selections = b, unread_files = error.files)) else
return(b)

}
