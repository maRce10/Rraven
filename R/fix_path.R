#' Modify sound file path in Raven's selection tables 
#' 
#' \code{fix_path} modifies the path column in selection tables and sound selection tables 
#' @usage fix_path(path = NULL, dest.path = NULL, recursive = FALSE, parallel = 1, pb = TRUE, 
#' new.begin.path, sound.file.col)  
#' @param path A character string indicating the path of the directory in which to look for the 'Raven' selection (text) files. 
#' If not provided (default) the function searches into the current working directory.
#' @param dest.path A character string indicating the path of the directory in which
#' sound selection tables will be saved. 
#' If not provided (default) files will be save in the current directory.
#' @param recursive Logical. If \code{TRUE} the listing recurses into sub-directories.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param new.begin.path A character string indicating the path of the directory where sound files 
#' would be located. This argument is required.
#' @param sound.file.col A character string with the name of the column containing the sound file names in 
#' the selection text files. Required.
#' @return Selection table file(s) saved in 'dest.path' or in the working 
#' directory (by default, which overwrites existing files). 
#' @details The function modifies the path field in Raven's selection tables or 
#' sound selection tables. This is useful when sound files have been moved to a 
#' different location (or computer). Note the ability to open selections and sound files
#' simultaneously works as long as the "begin.path" column is referring to the directory
#' containing the sound files.  
#' @seealso \code{\link{to_sound_selection}}; \code{\link{imp_raven}} 
#' @export
#' @name fix_path
#' @examples{
#' # load warbleR for sound file examples
#' library(warbleR)
#' 
#' #load data 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "selection_files"))
#'
#' # save sound files
#' writeWave(Phae.long1, file.path(tempdir(), "Phae.long1.wav"), extensible = FALSE)
#' writeWave(Phae.long2, file.path(tempdir(), "Phae.long2.wav"), extensible = FALSE)
#' writeWave(Phae.long3, file.path(tempdir(), "Phae.long3.wav"), extensible = FALSE)
#' writeWave(Phae.long4, file.path(tempdir(), "Phae.long4.wav"), extensible = FALSE)   

#' # save 'Raven' selection tables in the temporary directory
#' out <- lapply(1:2, function(x)
#' writeLines(selection_files[[x]], con = file.path(tempdir(), names(selection_files)[x])))
#' 
#' # try drag and drop selection files into Raven (shouldn't work)
#' 
#' # now fix files
#' fix_path(path = tempdir(), 
#' sound.file.col = "Begin File", new.begin.path = "YOUR NEW LOCATION HERE")
#' 
#' # try drag and drop into Raven again (should work now)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on nov-7-2017

fix_path <- function(path = NULL, dest.path = NULL, recursive = FALSE, parallel = 1, pb = TRUE, new.begin.path, sound.file.col)
{
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop("'path' provided does not exist") 
  
  # check dest.path
  if (is.null(dest.path)) dest.path <- path else 
    if (!dir.exists(dest.path)) stop("'path' provided does not exist") 
  
  # read selection file names
  sel.txt <- list.files(pattern = ".txt$", full.names = TRUE, recursive = recursive, ignore.case = TRUE, path = path)
  
  if (length(sel.txt) == 0) stop("No selection .txt files in working directory/'path' provided")
  
  options(warn = -1)
  
  repl.colm <- function(x, index, rpl) {
    xs <- strsplit(as.character(x), "\t",fixed=TRUE)[[1]]
    xs[index]  <- rpl
    y <- paste0(xs, collapse = "\t")
    return(y)
  }
  
  read_sels2_FUN <- function(i, sel.txt)
  {  
    a <- try(utils::read.table(sel.txt[i], header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE, check.names = FALSE), silent = TRUE)
    if (class(a) != "try-error")
    {
      
      rf <- readLines(sel.txt[i])  
      if (!grepl(sound.file.col, fixed = TRUE, rf[1])) cat("'sound.file.col' not found in at least 1 selection file") else
      {    
      
        if (grepl("Begin Path", fixed = TRUE, rf[1])) 
          rf[-1] <- sapply(2:length(rf), function(y)
            repl.colm(x = rf[y], index =  which(strsplit(rf[1], "\t",fixed=TRUE)[[1]] == "Begin Path"), rpl = file.path(new.begin.path, basename(strsplit(rf[y], "\t",fixed=TRUE)[[1]][which(strsplit(rf[1], "\t",fixed=TRUE)[[1]] == sound.file.col)])))) 
        
        name <- file.path(dest.path, basename(sel.txt[i])) 
        
        writeLines(rf, con = name)
    }
  }
  }
  
  # set pb options 
  pbapply::pboptions(type = ifelse(pb, "timer", "none"))
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel  
  
  out <- pbapply::pblapply(seq_len(length(sel.txt)), cl = cl, function(i) {
    read_sels2_FUN(i, sel.txt = sel.txt)
  })
  
}
