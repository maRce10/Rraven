#' Import 'Raven' selections
#' 
#' \code{imp_raven} imports several 'Raven' selection files simultaneously. Files must be in '.txt' format.
#' @usage imp_raven(path = NULL, warbler.format = FALSE,  all.data = FALSE, files = NULL,
#' only.spectro.view = TRUE, recursive = FALSE, name.from.file = FALSE, 
#' ext.case = NULL, freq.cols = TRUE, waveform = FALSE, parallel = 1, pb = TRUE, 
#' unread = FALSE, rm.dup = FALSE,  sound.file.col = NULL)  
#' @param path A character string indicating the path of the directory in which to look for the 'Raven' selection (text) files. 
#' If not provided (default) the function searches into the current working directory.
#' @param warbler.format Logical. If \code{TRUE} columns are renamed using the standard names for a selection table as in the package 'warbleR', frequency limit columns (high and low frequency) in 'Hz' are converted to 'kHz' (as in warbleR selection tables) and only the spectrogram view measurements are kept. Default is \code{FALSE}.
#' @param all.data Logical. If \code{TRUE} all columns in the selection files are returned, 
#' keeping the name columns as in the 'Raven' files. Default is \code{FALSE}. Columns absent in some selection files will 
#' be filled with NA's. This argument WILL BE DEPRECATED as it is being replaced by 'warbler.format'.
#' @param files Character vector indicating the name of selection files (in .txt format) to be imported. Optional. Default is \code{NULL}.
#' @param only.spectro.view Logical. If \code{TRUE} (default) only the measurements in the Raven spectrogram view ('View' column) are returned. Ignored if \code{warbler.format == TRUE} (only spectrogram view measurements are kept). 
#' @param recursive Logical. If \code{TRUE} the listing recurses into sub-directories.
#' @param name.from.file Logical. If \code{TRUE} the sound file names are extracted from the selection text file name. 
#' It assumes that selections files contained the suffix "Table.1.selections.txt", "selections.txt" or ".txt" (in that order). 
#' Note that by default it will assume that the extension file name is ".wav". This can be control using the
#' argument 'ext.case'. Default is \code{FALSE}). Ignored if sound.file.col' is provided and/or all.data is \code{TRUE}). Note that
#' the time information for selection tables with multiple sound files won't be corrected if \code{name.from.file = TRUE}.
#' @param ext.case Character string of length 1 to specify whether sound file extensions are in upper or lower case. This should match the extension of the
#' of the .wav files from which the selection were made. It must be either 'upper' or 'lower'. Only needed when 'name.from.file' is \code{TRUE}.
#' @param freq.cols Logical. If \code{TRUE} 'Low Freq' and 'High Freq' columns are also imported. Ignored if all.data is \code{TRUE}.
#' @param waveform Logical to control if waveform view data should be included (this data is typically duplicated in spectrogram view data).  Default is \code{FALSE} (not to include it). This argument WILL BE DEPRECATED as it is being replaced by 'only.spectro.view'.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param unread Logical. If \code{TRUE} a list (instead of a data frame). The first element of the list contains the selections
#' whole data. The second and third elements are character vectors with the names of sound files that could not be read or that contain multiple sound files but no 'File Offset' column and could not be imported. Default is \code{FALSE}.
#' @param rm.dup Logical. If \code{TRUE} duplicated rows and columns are removed. Useful when 
#' selection files have been duplicated. Default is \code{FALSE}. 
#' @param sound.file.col A character string with the name of the column containing the sound files in 
#' the selection text files. Default is \code{NULL}. Ignored if 'name.from.file' is \code{TRUE} and/or all.data is \code{TRUE}. This argument WILL BE DEPRECATED as the function now searches for columns containing the sound file names. 
#' @return A single data frame with information of the selection files. If \code{unread = TRUE} the function returns a list of length 3 with
#'  the selection data frame and a vector with the names of files that could not be read (see 'unread' argument).  
#'  If 'warbler.format' argument is set to \code{TRUE} the data frame contains the following columns: sound.files, selec, channel,start, end, top.freq, bottom.freq and selec.file. If all.data is set to \code{TRUE} then all columns in the 'Raven' selection files are returned. 
#'  If individual selection files contain information about multiple sound files the function will import the file and correct the time
#'  parameters (start and end) only if 1) the 'File Offset (s)' is found in the selection table.
#' @details The function import 'Raven' selection data from many files simultaneously. All selection files in the working directory or 'path' supplied will be imported (unless 'files' argument is also supplied). It has been created using Raven Pro 1.5 so selection tables created with other versions might not be read properly. Files must be in '.txt' format. Selection 
#' files including data from multiple recordings can also be imported, although they must contained a 'File Offset (s)' column. Selections that span across multiple sound files are not recommended as they will be assigned to the first sound file, which would produce errors for downstream analyses as those from the 'warbleR' package. 
#' @seealso \code{\link{imp_syrinx}} 
#' @export
#' @name imp_raven
#' @examples
#' 
#' # load data 
#' data(selection_files)
#' 
#' # save 'Raven' selection tables in the temporary directory 
#' out <- lapply(1:2, function(x) 
#' writeLines(selection_files[[x]], con = file.path(tempdir(), names(selection_files)[x])))
#' 
#' \donttest{
#'# providing the name of the column with the sound file names
#'rvn.dat <- imp_raven(sound.file.col = "Begin.File", all.data = FALSE, path = tempdir())
#' 
#' # View(rvn.dat)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on nov-7-2017

imp_raven <- function(path = NULL, warbler.format = FALSE, all.data = FALSE, files = NULL,  
                      only.spectro.view = TRUE, recursive = FALSE, name.from.file = FALSE, 
                      ext.case = NULL, freq.cols = TRUE, waveform = FALSE, 
                      parallel = 1, pb = TRUE, unread = FALSE, 
                      rm.dup = FALSE, sound.file.col = NULL) 
{
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop("'path' provided does not exist") else
      path <- normalizePath(path)
  
  if (!is.null(ext.case)) 
    if (!ext.case %in% c("upper", "lower")) stop("'ext.case' should be either 'upper' or 'lower'") else
      ext <- if (ext.case == "upper") "WAV" else "wav"
  
  if (is.null(ext.case) & name.from.file) stop("'ext.case' must be provided when name.from.file is TRUE")
  
  # read name and path
  sel.txt <- list.files(pattern = ".txt$", full.names = TRUE, recursive = recursive, ignore.case = TRUE, path = path)
  
  # only read file names
  sel.txt2 <- list.files(pattern = ".txt$", full.names = FALSE, recursive = recursive, ignore.case = TRUE, path = path)
  
  if (length(sel.txt) == 0) stop("No selection .txt files in working directory/'path' provided")
  
  # subset 
  if (!is.null(files)){
    sel.txt2 <- sel.txt2[sel.txt2 %in% files]
    sel.txt <- sel.txt[basename(sel.txt) %in% files]
  
    if (length(sel.txt) == 0) stop("Files provided not found")
    }
  
# set pb options 
pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel  
  
  # apply reading function over .txt files
  sl.list <- pbapply::pblapply(seq_len(length(sel.txt)), cl = cl,  
                               function(i)  try(utils::read.delim(sel.txt[i], header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE))
  
  # add name of file to list names
  names(sl.list) <- sel.txt2
  
  # determine files that could not be read
  error.files <- names(sl.list)[sapply(sl.list, class) == "try-error"]  
  
  # remove errors    
  sl.list <- sl.list[sapply(sl.list, class) != "try-error"]
  
  ## check if basic Raven columns are found 
  sl.list <- lapply(sl.list, function(x) {
    if(sum(grepl(pattern = "^view$|^selection$|^channel$", x = names(x), ignore.case = TRUE)) >= 3) return(x) else
      return(NA)
              })
 
  # remove NAs
  error.files <- c(error.files, names(sl.list)[sapply(sl.list, class) != "data.frame"])
  sl.list <- sl.list[sapply(sl.list, class) == "data.frame"]
  
  # fix time and make common column names
  if (length(sl.list) > 0){
      
    # relabel duplicated column names
    sl.list <- lapply(sl.list, function(x) {
      
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
    cnms <- unique(unlist(lapply(sl.list, names)))    
    
    # add columns that are missing to each selection table
    sl.list2 <- lapply(sl.list, function(X)
    {
      nms <- names(X)
      if (length(nms) != length(cnms))  
        for(i in cnms[!cnms %in% nms]) {
          X <- data.frame(X,  NA, stringsAsFactors = FALSE, check.names = FALSE)
          names(X)[ncol(X)] <- i
        }
      return(X)
    })
    
    # add names again
    names(sl.list2) <- names(sl.list)
    sl.list <- sl.list2
    
    # temporary put together to check sound file name column
    temp.sls <- do.call("rbind", sl.list2)
  
    if (!name.from.file){
    # check if file containing sound file name columns and choose sound file column
    sfcls <- names(temp.sls)[names(temp.sls) %in% c("Begin File", "End File", "Begin Path",  "End Path")]
    
    if(length(sfcls) == 0) 
      stop("No column containing sound file names was found in any selection table file")
    
    # sound file column
    sfcl <- sfcls[!(sapply(temp.sls[, sfcls], anyNA))][1]
    
    if((length(sfcl) == 0 | is.na(sfcl)) & !name.from.file) 
      stop("No column containing sound file names was shared by all selection table files")
    }
    
    # fix time in multiple file selection table
    #### ERROR HERE #######
    sl.list2 <- lapply(seq_len(length(sl.list)), function(i)
    {
      X <- sl.list[[i]]
      
      if (!name.from.file)
        if (length(unique(X[, sfcl])) > 1)
      { 
        if (!any(grepl("offset", names(X), ignore.case = TRUE))) {     
          message(paste0("warning: selections files from multiple sound files must contain a 'File Offset' column (check '", names(sl.list)[i],"')"))
          return(NA)
           } else {
          # fix start and end on multiple selection tables
        X[, grepl("^end time", names(X), ignore.case = TRUE)] <- as.numeric(X[, grepl("^end time", names(X), ignore.case = TRUE)]) - as.numeric(X[, grepl("^begin time", names(X), ignore.case = TRUE)])

        # fix if more than one column matches offset
        of.st.cl <- grep("^file offset", names(X), ignore.case = TRUE)
        if (length(of.st.cl) > 1) {
          of.st.cl2 <- names(X)[of.st.cl][which(sapply(X[,of.st.cl], is.numeric))[1]]
        
        # fix if column not found and use first column    
        of.st.cl <- if (is.na(of.st.cl2)) of.st.cl[1] else of.st.cl2
        }
        
        X[, grepl("^begin time", names(X), ignore.case = TRUE)] <- as.numeric(X[, of.st.cl])
        X[, grepl("^end time", names(X), ignore.case = TRUE)] <- X[, grepl("^end time", names(X), ignore.case = TRUE)] + X[, grepl("^begin time", names(X), ignore.case = TRUE)]
            }
        }
      
      return(X)
    })
    
    # add names again
    names(sl.list2) <- names(sl.list)
    sl.list <- sl.list2
    
    # determine files that could not be read
    error.multiple.files <- names(sl.list)[!sapply(sl.list, is.data.frame)]    
    
    # remove NAs again    
    sl.list <- sl.list[sapply(sl.list, is.data.frame)]
    
    # add selec file column
    sl.list <- lapply(1:length(sl.list), function(x)
      data.frame(sl.list[[x]], selec.file = names(sl.list)[x], check.names = FALSE)
      )
    
    # pool in a single data frame
    sls <- do.call("rbind", sl.list)
    
    # if name from file
    if(name.from.file)
    {
      if (name.from.file)
        sfcl <- "sound.files"
      
      if (ext.case == "lower")
        sls$sound.files <- gsub(pattern = "\\.Table\\.[[:digit:]].selections.txt$|\\.selections.txt$|\\.txt$", replacement = ".wav", x = sls$selec.file) else
          sls$sound.files <- gsub(pattern = "\\.Table\\.[[:digit:]].selections.txt$|\\.selections.txt$|\\.txt$", replacement = ".WAV", x = sls$selec.file) 
    }
    
    # delete offset column
    if (!all.data)
      sls <- sls[, !grepl("^file offset", names(sls), ignore.case = TRUE)]      
    
    # use base name in case begin.path or end.path were used as sound file names
    sls[, sfcl] <- basename(as.character(sls[, sfcl]))
    
    # remove duplicates
    if (rm.dup)
      sls <- sls[!duplicated(sls), ]
    
    # if rows are left
    if(!is.null(nrow(sls))) {
      
      # rename rows
      rownames(sls) <- 1:nrow(sls)
    
    # remove the views other than spectrogram
    if (only.spectro.view)
      sls <- sls[grepl("spectrogram", sls[,grepl("^view$", names(sls), ignore.case = TRUE)], ignore.case = TRUE), ]
    
    # remove non required columns 
    if (!all.data)
      sls <- sls[, grepl(paste0("^end time|^begin time|^low Freq|^High Freq|^selection$|^channel$|^selec.file$|", sfcl), names(sls), ignore.case = TRUE)]
    
    # remove frequency columns
    if (!freq.cols)
      sls <- sls[, !grepl("^low Freq|^High Freq", names(sls), ignore.case = TRUE)]
    
    # use warbleR column name format and transform Hz to kHz
    if (warbler.format) 
      sls <- relabel_colms(X = sls, extra.cols.name = sfcl, extra.cols.new.name = "sound.files", hz.to.khz = TRUE)
    }
    
    }  else sls <- NULL
  
  # warning for files that could not be read
  if (length(error.files) == length(sel.txt2)) cat("Not a single file could be read") else
    if (length(error.files) > 0 & !unread) cat(paste(length(error.files), "file(s) could not be read: ", paste(error.files, collapse = "/")))

  # warning for multiple sound file selections that could not be imported
  if (length(error.multiple.files) == length(sel.txt2)) cat("All selection tables contained data from multiple sound files but couldn't be imported  (missing 'File offset' column)") else
    if (length(error.multiple.files) > 0 & !unread) cat(paste(length(error.multiple.files), "selection table(s) with multiple files could not be imported (missing 'File offset' column): ", paste(error.multiple.files, collapse = "/")))
  
  if (unread) return(list(selections = sls, unread_files = error.files, multiple.sound.file.unread = error.multiple.files)) else
    return(sls)
}
