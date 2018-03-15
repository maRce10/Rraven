#' Run 'Raven' batch detector
#' 
#' \code{raven_batch_detec} Runs 'Raven' batch detector on multiple sound files sequentially
#' @usage raven_batch_detec(raven.path = NULL, sound.files, path = NULL, 
#' detector = "Amplitude detector", 
#' relabel_colms = TRUE, pb = TRUE)  
#' @param raven.path A character string indicating the path of the directory in which to look for the 'Raven' executable file (where 'Raven' was installed). 
#' @param sound.files character vector indicating the files that will be analyzed.
#' In OSX (mac) only one file at the time can be run (use loops instead!). If \code{NULL} (default) 
#' then 'Raven' will be run without opening any file.
#' @param path A character string indicating the path of the directory in which to look for
#' the sound files. If not provided (default) the function searches into the current working 
#' directory. Default is \code{NULL}.
#' @param detector Character string specifying the type of detector to be called.
#' There are 3 options available in Raven: 'Amplitude detector' (default), 'Band 
#' Limited Energy Detector' and 'Band Limited Entropy Detector'. Detector 
#' parameters must be set in 'Raven' before running the function.
#' @param relabel_colms Logical. If  \code{TRUE} (default) colums are labeled to 
#' match the selection table format from the acoustic analysis package \code{\link{warbleR}}
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return A data frame with the selections produced during the detection. See \code{\link{imp_raven}} for more details on how selections are imported.
#' @details The function runs 'Raven' sound analysis software (Cornell Lab of
#' Ornithology), detector on  multiple sound files seuentially. 'Raven' Pro must be 
#' installed. Note that batch detection in 'Raven' can also take 
#' sound files in 'mp3', 'flac' and 'aif' format. 
#' @seealso \code{\link{imp_raven}}; \code{\link{imp_syrinx}};  \code{\link{run_raven}}  
#' @export
#' @name raven_batch_detec
#' @examples
#' \dontrun{
#' 
#' # here replace with the path where 'Raven' is install in your computer
#' raven.path <- "PATH_TO_RAVEN_DIRECTORY_HERE" 
#' 
#' # Run detector on raven example sound files
#' 
#' # single sound file
#' detec.res <- raven_batch_detec(raven.path = raven.path, 
#' sound.files = "BlackCappedVireo.aif", path = file.path(raven.path, "Examples"))
#' 
#' # on raven examples  2 files
#' detec.res <- raven_batch_detec(raven.path = raven.path, 
#' sound.files = c("BlackCappedVireo.aif", "CanyonWren.wav"), 
#' path = file.path(raven.path, "Examples"))
#' 
#' # using 'Band Limited Energy Detector' 
#' detec.res <- raven_batch_detec(raven.path = raven.path, 
#' sound.files = c("BlackCappedVireo.aif", "CanyonWren.wav"), 
#' path = file.path(raven.path, "Examples"), detector = "Band Limited Energy Detector")
#' }
#' 
#' @author Marcelo Araya-Salas (\email{araya-salas@@cornell.edu})
#last modification on nov-8-2017

raven_batch_detec <- function(raven.path = NULL, sound.files, path = NULL, detector = "Amplitude detector", relabel_colms = TRUE, pb = TRUE)
{
  
  #check path to working directory
  if(is.null(path)) path <- getwd() else if(!file.exists(path)) stop("'path' provided does not exist") 
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  on.exit(file.remove(file.path(raven.path, "temp.bcv.txt")), add = TRUE)

    if(is.null(raven.path))
    stop("Path to 'Raven' folder must be provided")  else
      if(!file.exists(raven.path)) stop("'raven.path' provided does not exist")
  
  setwd(raven.path)
  
    sf <- sound.files <- as.character(sound.files)

    #return warning if not all sound files were found
    recs.wd <- list.files(path = path, pattern = "\\.wav$|\\.aif$|\\.flac$|\\.mp3$", ignore.case = TRUE)
    
    #count number of sound files in working directory and if 0 stop
    sound.files <- sound.files[sound.files %in% recs.wd]
    if(length(sound.files) == 0)
      stop("The sound files are not in the working directory")
    
    # remove sound files not found
    if(length(sound.files) != length(sf)) 
      cat(paste(length(sf) - length(sound.files), ".wav file(s) not found"))
    
    
    # check if sound file names contains directory and fix
    if(basename(sound.files[1]) == sound.files[1])
      sound.files <- file.path(path, sound.files)
    
    if(pb) lply <- pbapply::pblapply else lply <- lapply

    out <- lply(sound.files, function(x) {
      
      if(Sys.info()[1] == "Windows")
      {  
        comnd <- paste(shQuote(file.path(raven.path, "Raven.exe"), type = "cmd"),  paste0("-detType:", detector), shQuote(x), "-detTable:temp.bcv.txt -x")
      } else
      {
        if(Sys.info()[1] == "Linux")
        comnd <- paste(file.path(raven.path, "Raven"), paste0("-detType:", detector), x, "-detTable:temp.bcv.txt -x") else
        comnd <- paste("open Raven.app --args", x, paste0("-detType:", detector), "-detTable:temp.bcv.txt -x") # OSX
      }
        
        # run raven
        system(command = comnd, ignore.stderr = TRUE, intern = TRUE)
        
        output <- utils::read.table("temp.bcv.txt", sep = "\t",  header = TRUE)
        
        if(nrow(output) > 1)
        output$sound.files <- basename(x) else output <- vector(length = 0)
        
        return(output)
    })

    out <- out[sapply(out, is.data.frame)]
    
    if(length(out) > 0)
    {
      output <- do.call(rbind, out)
    
    if(relabel_colms)
      output <- relabel_colms(output, waveform = !any(grepl("Spectrogram", output$View)))
    
    output <- output[, c(ncol(output), 2:(ncol(output) - 1))]  
    
  return(output)} else return(NULL)
  
  
}
