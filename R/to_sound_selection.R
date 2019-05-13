#' Convert Raven's selection files into sound selection files
#' 
#' \code{to_sound_selection} converts Raven's selection files into sound selection files
#' @usage to_sound_selection(path = NULL, dest.path = NULL, recursive = FALSE,
#'  parallel = 1, pb = TRUE, sound.file.path, sound.file.col)  
#' @param path A character string indicating the path of the directory in which to look for the 'Raven' selection (text) files. 
#' If not provided (default) the function searches into the current working directory.
#' @param dest.path A character string indicating the path of the directory in which
#' sound selection tables will be saved. 
#' If not provided (default) files will be save in the current directory.
#' @param recursive Logical. If \code{TRUE} the listing recurse into sub-directories.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param sound.file.path A character string indicating the path of the 
#' directory containing the sound file(s). This argument is required.
#' @param sound.file.col A character string with the name of the column containing the sound file names in 
#' the selection text files. Required.
#' @return Sound selection table file(s) saved in 'dest.path' or in the working 
#' directory. 
#' @details The function converts Raven's selection tables to sound selection tables.
#'  Sound selection table is a more convenient format as it can be open directly in Raven (or drag-and-drop) and 
#'  will automatically open the associated sound file. Multiple files can be 
#'  simultaneously converted. Files must be in '.txt' format. Selection 
#' files including data from mulitple recordings can be converted only if all the 
#' correspondent sound files are found in the same directory. Note that no data is 
#' imported into the R environment.
#' @seealso \code{\link{imp_syrinx}}; \code{\link{imp_raven}} 
#' @export
#' @name to_sound_selection
#' @examples{
#' 
#' #load data 
#' data(selection_files)
#' 
#' # set temporary directory
#' # setwd(tempdir())
#' 
#' # save 'Raven' selection tables in the temporary directory
#' out <- lapply(1:2, function(x)
#' writeLines(selection_files[[x]], con = names(selection_files)[x]))
#' 
#' # try drag and drop selection files into Raven (shouldn't work)
#' 
#' # now convert files
#' to_sound_selection(sound.file.path = getwd(), 
#' sound.file.col = "Begin Path")
#' 
#' # try drag and drop into Raven again (should work now)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on nov-7-2017

to_sound_selection <- function(path = NULL, dest.path = NULL, recursive = FALSE, parallel = 1, pb = TRUE, sound.file.path, sound.file.col)
{
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd))
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else {if (!dir.exists(path)) stop("'path' provided does not exist") else
    setwd(path)
  }  

  if (is.null(dest.path)) dest.path <- path else 
    if (!dir.exists(dest.path)) stop("'path' provided does not exist") 

  sel.txt <- list.files(pattern = ".txt$", full.names = TRUE, recursive = recursive, ignore.case = TRUE)

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
        repl.colm(x = rf[y], index =  which(strsplit(rf[1], "\t",fixed=TRUE)[[1]] == "Begin Path"), rpl = file.path(sound.file.path, basename(strsplit(rf[y], "\t",fixed=TRUE)[[1]][which(strsplit(rf[1], "\t",fixed=TRUE)[[1]] == sound.file.col)])))) else
        {
          rf[1] <- paste0(rf[1], "\tBegin Path")
        
           rf[-1] <- paste0(rf[-1], "\t", file.path(sound.file.path, basename(strsplit(rf[-1], "\t",fixed=TRUE)[[1]][which(strsplit(rf[1], "\t",fixed=TRUE)[[1]] == sound.file.col)])))        
      }      
            
    if (!grepl("File Offset (s)", fixed = TRUE, rf[1]))  
    {
      rf[1] <- paste0(rf[1], "\tFile Offset (s)")

      rf[-1] <- paste0(rf[-1], "\t", as.numeric(sapply(rf[-1], function(x) strsplit(x, "\t")[[1]][which(strsplit(rf[1], "\t",fixed=TRUE)[[1]] == "Begin Time (s)")])))
    }

    
        
    newname <- gsub("selections.txt$", "sound.selections.txt$",sel.txt[i], fixed = TRUE)

    if (!grepl("sound.selections", newname)) newname <- gsub("\\.txt$", "sound.selections.txt",sel.txt[i])
    
    if (newname == sel.txt[i]) newname <- gsub("\\.txt$", "-2.txt",sel.txt[i])

    if (!is.null(dest.path)) newname <- file.path(dest.path, newname)
    
    writeLines(rf, con = newname)
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
