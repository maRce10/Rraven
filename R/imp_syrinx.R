#' Import 'Syrinx' selections
#' 
#' \code{imp_syrinx} imports 'Syrinx' selection data from many files simultaneously. 
#' All files must have the same columns.
#' @usage imp_syrinx(path = NULL, all.data = FALSE, recursive = FALSE, 
#' exclude = FALSE, hz.to.khz = TRUE, parallel = 1, pb = TRUE)  
#' @param path A character string indicating the path of the directory in which to look for the text files. 
#' If not provided (default) the function searches into the current working directory. Default is \code{NULL}.
#' @param all.data Logical. If \code{TRUE} all columns in text files are returned. Default is \code{FALSE}. Note 
#' that all files should contain exactly the same columns in the same order. 
#' @param recursive Logical. If \code{TRUE} the listing recurses into sub-directories.
#' @param exclude Logical. Controls whether files that cannot be read are ignored (\code{TRUE}). Default is \code{FALSE}.
#' @param hz.to.khz Logical. Controls if frequency variables should be converted from  Hz (the unit used by Syrinx) to kHz (the unit used by warbleR and other bioacoustic analysis packages in R). Default if \code{TRUE}. Ignored if all.data is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A single data frame with information of the selection files. If all.data argument is set to \code{FALSE} the data 
#' frame contains the following columns: selec, start, end, and selec.file. If sound.file.col is provided the data frame
#' will also contain a 'sound.files' column. If all.data is set to \code{TRUE} then all 
#' columns in selection files are returned.
#' @seealso \code{\link{imp_raven}}
#' @export
#' @name imp_syrinx
#' @examples
#' \dontrun{
#' #load data 
#' data(selection_files)
#' 
#' #save 'Raven' selection tables in the temporary directory 
#' writeLines(selection_files[[7]], con = file.path(tempdir(), names(selection_files)[7]))
#' 
#' syr.dat <- imp_syrinx(all.data = FALSE)
#' 
#' # View(syr.dat)
#' 
#' #getting all the data
#' syr.dat <- imp_syrinx(all.data = TRUE)
#' 
#' # View(syr.dat)
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on nov-7-2017

imp_syrinx <- function(path = NULL, all.data = FALSE, recursive = FALSE,
                       exclude = FALSE, hz.to.khz = TRUE, parallel = 1, pb = TRUE) 
{ 
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) 
      stop("'path' provided does not exist") else
        path <- normalizePath(path)

sel.txt <- list.files(full.names = TRUE, path = path)
sel.txt2 <- list.files(full.names = FALSE, path = path)

sel.txt <- sel.txt[grep(".log$|.txt$",ignore.case = TRUE, sel.txt)]
sel.txt2 <- sel.txt2[grep(".log$|.txt$",ignore.case = TRUE, sel.txt2)]

if (length(sel.txt) == 0) stop("No selection files in working directory/'path' provided")

b<-NULL
if (substring(text = readLines(sel.txt[1])[1], first = 0, last = 9) == "fieldkey:") field <- T else field <- F

# set pb options 
pbapply::pboptions(type = ifelse(pb, "timer", "none"))

# set clusters for windows OS
if (Sys.info()[1] == "Windows" & parallel > 1)
  cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel  

clist <- pbapply::pblapply(1:length(sel.txt), cl = cl, function(i)
  {    
  if (field)  {
    
    a <- try(utils::read.table(sel.txt[i], header = TRUE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE), silent = TRUE) 
    if (!exclude & class(a) == "try-error") stop(paste("The selection file",sel.txt[i], "cannot be read"))
    
  if (!class(a) == "try-error" & !all.data) { c <- data.frame(selec.file = sel.txt2[i], sound.files = a[, grep("soundfile",colnames(a))],
                                selec = 1,
                                start = a[, grep("lefttimesec",colnames(a))],
                                end = a[, grep("righttimesec",colnames(a))],
                                bottom.freq = a[, grep("bottomfreq",colnames(a))],
                                top.freq = a[, grep("topfreq",colnames(a))])
  for(i in 2:nrow(c)) if (c$selec.file[i] == c$selec.file[i-1]) c$selec[i]<-c$selec[i-1] + 1
  } else c<-a 
                                } else {
            a <- try(utils::read.table(sel.txt[i], header = FALSE, sep = "\t", fill = TRUE, stringsAsFactors = FALSE), silent = TRUE) 
            if (!exclude & class(a) == "try-error") stop(paste("The selection file",sel.txt[i], "cannot be read"))
            
            if (!class(a) == "try-error") 
              { 
              c <- a[, seq(2, ncol(a), by =2)]
           colnames(c) <- gsub(":", "", unlist(a[1, seq(1,ncol(a), by =2)]), fixed = TRUE)
           if (!all.data) {c<-data.frame(sound.files = c[, grep("selected",colnames(c), ignore.case = TRUE)],
                                       selec = 1,
                                       start = c[, grep("lefttime",colnames(c), ignore.case = TRUE)],
                                       end = c[, grep("righttime",colnames(c), ignore.case = TRUE)],
                                       bottom.freq = c[, grep("bottomfreq",colnames(c), ignore.case = TRUE)],
                                       top.freq = c[, grep("topfreq",colnames(c), ignore.case = TRUE)])
           for(i in 2:nrow(c)) if (c$sound.files[i] == c$sound.files[i-1]) c$selec[i] <- c$selec[i-1] + 1} 
           } else c <- a         
                                }
  return(c)
})

clist <- clist[sapply(clist, is.data.frame)]
b <- do.call("rbind", clist)
if (!all.data) if (any(is.na(b$start))) warning("NAs found (empty rows)")

b <- b[!duplicated(b), ]

options(warn = -1)
if (!all.data)
{
  b$start <- as.numeric(b$start)
  b$end <- as.numeric(b$end)

  #remove NA rows
  b <- b[!is.na(b$start), ]
  } else b <-b[b[,2] != names(b)[2],]

# convert to hz
if (hz.to.khz & !all.data & all(c("bottom.freq", "top.freq") %in% names(b)))
  {b$bottom.freq <- as.numeric(b$bottom.freq) / 1000 
  b$top.freq <- as.numeric(b$top.freq) / 1000 
}
  return(b[!duplicated(b), ])

}
