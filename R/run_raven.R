#' Open sound files in 'Raven' sound analysis software 
#' 
#' \code{run_raven} opens several sound files in 'Raven' sound analysis software
#' @usage run_raven(raven.path = NULL, sound.files = NULL, path = NULL, at.the.time = 10,
#' import = FALSE, redo = FALSE, view.preset = NULL, pb = TRUE, ...)  
#' @param raven.path A character string indicating the path of the directory in which to look for the 'Raven' executable file (where 'Raven' was installed). 
#' @param sound.files character vector indicating the files that will be analyzed. If  \code{NULL} (default) then 'Raven' will be run without opening any file.
#' @param path A character string indicating the path of the directory in which to look for
#' the sound files. If not provided (default) the function searches into the current working 
#' directory. Default is \code{NULL}.
#' @param at.the.time Numeric vector of length 1 controling how many files will be open in
#'  'Raven' at the same time. Note that opening too many files at once could make 'Raven' run out
#'  of memory. You need to close 'Raven' every time the batch of files is analyzed, so the next
#'  batch is opened. Default is 10. Not available in OSX (mac).
#' @param import Logical. Controls if the selection tables generated should be returned as a 
#' data frame into the R environment. This only works if the selections are saved in the 
#' "Selections" folder in the 'Raven' directory. This argument calls the \code{\link{imp_raven}}
#' internally. Additional arguments can be passed to \code{\link{imp_raven}} to control the way the data is imported.
#' @param redo Logical. Controls whether only the subset of files with no 'Raven' selections (.txt file) in the 'Raven' 'selections' folder
#' are analyzed (if \code{FALSE}). Useful when resuming the analysis. Default is \code{FALSE}.
#' @param view.preset Character string defining the 'Raven' view preset to be used.
#' It should match exactly the name of the present in the 'Raven' folder 'Presets/Sound Window'. If not provided the default view preset is used.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param ... Additional arguments to be passed to \code{\link{imp_raven}} for customizing
#' how selections are imported (ignored if \code{import = FALSE}).
#' @return If \code{import = TRUE} a data frame with the selections produced during the analysis will be return as an data frame. See \code{\link{imp_raven}} for more details on how selections are imported.
#' @details The function runs 'Raven' sound analysis software (Cornell Lab of
#' Ornithology), opening many files simultaneously. 'Raven' will still run if no
#' sound files are provided (i.e. \code{sound.files = NULL}). At the end of the
#' analysis the data can be automatically imported back into R using the 'import'
#' argument. 'Raven' Pro must be installed. Note that 'Raven' can also take sound files in 'mp3', 'flac' and
#' 'aif' format.
#' @seealso \code{\link{imp_raven}}; \code{\link{imp_syrinx}}
#' @export
#' @name run_raven
#' @examples
#' \dontrun{
#' # First set temporary folder
#' setwd(tempdir())
#' 
#'# save sound files
#' library(warbleR) 
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4"))
#' writeWave(Phae.long1, "Phae.long1.wav", extensible = FALSE)
#' writeWave(Phae.long2, "Phae.long2.wav", extensible = FALSE)
#' 
#' # here replace with the path where 'Raven' is install in your computer
#' raven.path <- "PATH_TO_RAVEN_DIRECTORY_HERE" 
#' 
#' # run function 
#' run_raven(raven.path = raven.path, sound.files = c("Phae.long1.wav", "Phae.long2.wav"),
#'  at.the.time = 2, import = T, name.from.file = T, ext.case = "upper", 
#'  all.data = TRUE, path = tempdir())  
#'  
#' #getting all the data
#' rav.dat<-run_raven(all.data = TRUE, raven.path = raven.path)
#' # View(rav.dat)
#' 
#' writeWave(Phae.long3, "Phae.long3.wav", extensible = FALSE)
#' writeWave(Phae.long4, "Phae.long4.wav", extensible = FALSE)
#' 
#' # run function on all the wav files in the working directory 3 at the time
#' run_raven(raven.path = raven.path, sound.files = list.files(pattern = "\\.wav$", 
#' ignore.case = TRUE, path = tempdir()), at.the.time = 3, import = FALSE, 
#' path = tempdir())
#'   
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on nov-7-2017

run_raven <- function(raven.path = NULL, sound.files = NULL, path = NULL, at.the.time = 10,
                      import = FALSE, redo = FALSE, view.preset = NULL, 
                      pb = TRUE, ...)
  {
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else if (!dir.exists(path)) stop("'path' provided does not exist") 
  
  if (is.null(raven.path))
    stop("Path to 'Raven' folder must be provided")  else
      if (!dir.exists(raven.path)) stop("'raven.path' provided does not exist")

  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(raven.path)
    
  # set progress bar back to original
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), 
          add = TRUE)
    
  if (!is.null(view.preset))
   {
    if (!any(view.preset %in% list.files(path = file.path(raven.path, "Presets/Sound Window")))) stop("'view.preset' provided not found")
    
  def.view.p <- grep("^Default", value = TRUE,  list.files(path = file.path(raven.path, "Presets/Sound Window")))

  for(i in def.view.p)
{ 
  out <- file.copy(from = file.path(raven.path, "Presets/Sound Window", i), to = file.path(raven.path, paste0("Presets/Sound Window/temp.", i)), overwrite = TRUE)

  out <- file.copy(from =  file.path(raven.path, "Presets/Sound Window",view.preset), to =  file.path(raven.path, "Presets/Sound Window", i), overwrite = TRUE)
    
  on.exit(file.copy(from =  file.path(raven.path, paste0("Presets/Sound Window/temp.", i)), to =  file.path(raven.path, "Presets/Sound Window", i), overwrite = TRUE), add = TRUE)
  }
  
on.exit(unlink(file.path(raven.path, "Presets/Sound Window", grep("^temp.Default", value = TRUE,  list.files(path = file.path(raven.path, "Presets/Sound Window"))))), add = TRUE)
  }
  
  
  if (is.null(sound.files))
{
  if (Sys.info()[1] == "Windows")
    out <- system(shQuote(file.path(raven.path, "Raven"), type = "cmd"), ignore.stderr = TRUE, intern = TRUE) else
    { 
      if (Sys.info()[1] == "Linux")
           out <- system(file.path(raven.path, "Raven"), ignore.stderr = TRUE, intern = TRUE) else
             out <- system("open Raven.app", ignore.stderr = TRUE, intern = TRUE) # OSX
    }  
     
} else {
  sf <- sound.files <- as.character(sound.files)
  
  #return warning if not all sound files were found
  recs.wd <- list.files(path = path, pattern ="\\.wav$|\\.aif$|\\.flac$|\\.mp3$", ignore.case = TRUE)
  
  #count number of sound files in working directory and if 0 stop
  sound.files <- sound.files[sound.files %in% recs.wd]
  if (length(sound.files) == 0)
    stop("The .wav files are not in the working directory")
  
  # remove sound files not found
  if (length(sound.files) != length(sf)) 
   cat(paste(length(sf) - length(sound.files), ".wav file(s) not found"))
  
  if (!redo) {
    # get names of files from selections
    sls <- NULL
    try(sls <- imp_raven(pb = FALSE, path = file.path(raven.path, "Selections"), ...), silent = TRUE)
    
    # remove those that have a selection table
    if (!is.null(sls))
    sound.files <- sound.files[!gsub("\\.wav", "", basename(sound.files), ignore.case = TRUE) %in% sapply(strsplit(unique(sls[,names(sls) == "selec.file"]), ".Table"), "[", 1)]
  
    if (length(sound.files) == 0) 
    {
      cat("All sound files have a selection table in Raven's selection folder")
      stop("")}
    }
   
    # check if sound file names contains directory and fix
    if (basename(sound.files[1]) == sound.files[1])
    sound.files <- file.path(path, sound.files)

  # if in OSX at the time not available
      if (!Sys.info()[1] %in% c("Windows", "Linux")) at.the.time <- length(sound.files)

  # subset by groups of sound files according to at the time
  sq <- unique(c(seq(1, length(sound.files), by = at.the.time)))
  
  if (pb) pbapply::pboptions(type = "timer") else pbapply::pboptions(type = "none")
  
  # run loop over files
  out <- pbapply::pblapply(sq, function(x)
    {
 
    fls <- sound.files[x:(x + at.the.time - 1)]
    fls <- fls[!is.na(fls)]
    
    fls <- paste(fls, collapse = " ")
    
    if (Sys.info()[1] == "Windows")
      comnd <- paste(shQuote(file.path(raven.path, "Raven"), type = "cmd"), fls) else
        {
          if (Sys.info()[1] == "Linux")
        comnd <- paste(paste("cd", raven.path, ";"), paste(file.path(raven.path, "Raven"), fls)) else
          comnd <- paste("Open Raven.app --args", fls)
        }
    
    # run raven
    out <- system(command = comnd, ignore.stderr = TRUE, intern = TRUE)
    }
    )
}
  
  if (import){
    sels <- imp_raven(pb = FALSE, path = file.path(raven.path, "Selections"), ...)
    
    # extract recording names from selec.file column
    rec.nms <- sapply(1:nrow(sels), function(x){
      strsplit(sels$selec.file, split = ".Table")[[x]][1]
    })
    
    # find selection tables for only target recordings among 'Raven' selection tables in Selections directory
    if (!is.null(sf)){
    sf <- gsub("\\.wav", "", sf, ignore.case = TRUE)
    
     if (any(rec.nms %in% sf)) sels <- sels[grep(paste(sf, collapse = "|"), rec.nms), ]
     }
   
  return(sels)

  }
  
}
