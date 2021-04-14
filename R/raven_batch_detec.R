#' Run 'Raven' batch detector
#' 
#' \code{raven_batch_detec} Runs 'Raven' batch detector on multiple sound files sequentially
#' @usage raven_batch_detec(raven.path = NULL, sound.files, path = NULL, 
#' detector.type, detector.preset = "Default",
#' view.preset = "Default", relabel_colms = TRUE, pb = TRUE, parallel = 1)
#' @param raven.path A character string indicating the path of the directory in which to look for the 'Raven' executable file (where 'Raven' was installed). 
#' @param sound.files character vector indicating the files that will be analyzed.
#' In OSX (mac) only one file at the time can be run (use loops instead!). If \code{NULL} (default) 
#' then 'Raven' will be run without opening any file.
#' @param path A character string indicating the path of the directory in which to look for
#' the sound files. If not provided (default) the function searches into the current working 
#' directory. Default is \code{NULL}.
#' @param detector.type Character string specifying the type of detector to be called.
#' There are 3 options available in 'Raven': 'Amplitude Detector', 'Band 
#' Limited Energy Detector' and 'Band Limited Entropy Detector'. Must be provided.
#' @param detector.preset Character string specifying the name of the customized detector to be called.
#' If \code{NULL} (default) then the 'Default' detector for the specific detector type is used 
#' (see 'detector.type' argument). Custom detectors must be found in one of the default 'Raven' detector directories 
#' (usually within "'raven.path'/Presets/Detector"). 
#' @param view.preset Character string specifying the name of the window preset to be used. Not require for 'Amplitude Detector' (see 'detector.type' argument).
#' If \code{NULL} (default) then the 'Default' window preset is used. 
#' @param relabel_colms Logical. If  \code{TRUE} (default) columns are labeled to 
#' match the selection table format from the acoustic analysis package \code{\link{warbleR}}
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#' It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @return A data frame with the selections produced during the detection. See \code{\link{imp_raven}} for more details on how selections are imported.
#' @details The function runs 'Raven' sound analysis software (Cornell Lab of
#' Ornithology), detector on  multiple sound files sequentially. 'Raven' Pro must be 
#' installed. Note that batch detection in 'Raven' can also take 
#' sound files in 'mp3', 'flac' and 'aif' format. 
#' @seealso \code{\link{imp_raven}}; \code{\link{imp_syrinx}}; \code{\link{run_raven}}  
#' @export
#' @name raven_batch_detec
#' @examples 
#' \dontrun{
#' 
#' # here replace with path where 'Raven' is installed in your computer
#' raven.path <- "PATH_TO_RAVEN_DIRECTORY_HERE" 
#' 
#' # Run detector on raven example sound files
#' 
#' # single sound file using 'Amplitude Detector' 
#' detec.res <- raven_batch_detec(raven.path = raven.path, 
#' sound.files = "BlackCappedVireo.aif", path = file.path(raven.path, "Examples"), 
#' detector.type = "Amplitude Detector")
#' 
#' # on raven examples  2 files
#' detec.res <- raven_batch_detec(raven.path = raven.path, 
#' sound.files = c("BlackCappedVireo.aif", "CanyonWren.wav"), 
#' path = file.path(raven.path, "Examples"), detector.type = "Amplitude Detector")
#' 
#' # using 'Band Limited Energy Detector' 
#' detec.res <- raven_batch_detec(raven.path = raven.path, 
#' sound.files = c("BlackCappedVireo.aif", "CanyonWren.wav"), 
#' path = file.path(raven.path, "Examples"), detector = "Band Limited Energy Detector")
#' }
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
# last modification on jan-06-2020

raven_batch_detec <- function(raven.path = NULL, sound.files, path = NULL, detector.type,
                              detector.preset = "Default", view.preset = "Default", relabel_colms = TRUE, pb = TRUE, parallel = 1)
{
  
  # check path to working directory
  if (is.null(path)) path <- getwd() else if (!dir.exists(path)) stop("'path' provided does not exist") else
    path <- normalizePath(path)
  
  # set progress bar back to original
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), 
          add = TRUE)
  
  # reset working directory 
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  on.exit(suppressWarnings(file.remove(file.path(raven.path, "temp.bcv.txt"))), add = TRUE)
  
  # check path
  if (is.null(raven.path))
    stop("Path to 'Raven' folder must be provided")  else
      if (!dir.exists(raven.path)) stop("'raven.path' provided does not exist")
  
  setwd(raven.path)
  
  # check detector type
  if (!detector.type %in% c("Amplitude Detector", "Band Limited Energy Detector", "Band Limited Entropy Detector")) stop("'detector.type' not recognized")
  
  # check detector name
  if (!file.exists(file.path(raven.path, "Presets/Detector", detector.type, detector.preset))) stop("'detector.preset' file not found")
  
  # check view  preset name
  # if (!file.exists(file.path(raven.path, "Presets/Sound Window", view.preset))) stop("'view.preset' file not found")
  
  sf <- sound.files <- as.character(sound.files)
  
  #return warning if not all sound files were found
  recs.wd <- list.files(path = path, pattern = "\\.wav$|\\.aif$|\\.flac$|\\.mp3$", ignore.case = TRUE)
  
  #count number of sound files in working directory and if 0 stop
  sound.files <- sound.files[sound.files %in% recs.wd]
  if (length(sound.files) == 0)
    stop("The sound files are not in the working directory")
  
  # remove sound files not found
  if (length(sound.files) != length(sf)) 
    cat(paste(length(sf) - length(sound.files), "sound file(s) not found"))
  
  # check if sound file names contains directory and fix
  if (basename(sound.files[1]) == sound.files[1])
    sound.files <- file.path(path, sound.files)
  
  # check if raven executable is "Raven" or "RavenPro" (changed in Raven Pro 1.6)
  rav.exe <- list.files(path = raven.path, pattern =  "Raven$|Raven.app$|Raven.exe$|Raven\ Pro$|Raven\ Pro.app$|Raven\ Pro.exe$")
  
  
  if (pb) pbapply::pboptions(type = "timer") else pbapply::pboptions(type = "none")
  
  # set clusters for windows OS
  if (Sys.info()[1] == "Windows" & parallel > 1)
    cl <- parallel::makePSOCKcluster(getOption("cl.cores", parallel)) else cl <- parallel  
  
  out <- pbapply::pblapply(sound.files,  cl = cl, function(x) {
    
    # view and detector preset together to fix it when view preset not need it  
    view.detector <- if (detector.type == "Amplitude Detector")  paste0("-detPreset:", detector.preset) else
      paste(paste0("-viewPreset:", view.preset), paste0("-detPreset:", detector.preset))
    
    if (Sys.info()[1] == "Windows")
    {  
      comnd <- paste(shQuote(file.path(raven.path, rav.exe), type = "cmd"), view.detector, paste0("-detType:", detector.type), shQuote(x), "-detTable:temp.bcv.txt -x")
    } else
    {
      if (Sys.info()[1] == "Linux")
        comnd <- paste(file.path(raven.path, rav.exe), view.detector, paste0("-detType:", detector.type), x, "-detTable:temp.bcv.txt -x") else
          comnd <- paste("Open",  rav.exe, "--args", x, view.detector, paste0("-detType:", detector.type), "-detTable:temp.bcv.txt -x") # OSX
    }
    
    # run raven
    system(command = comnd, ignore.stderr = TRUE, intern = TRUE)
    
    output <- utils::read.table("temp.bcv.txt", sep = "\t",  header = TRUE)
    
    if (nrow(output) > 0)
      output$sound.files <- basename(x) else output <- vector(length = 0)
    
    return(output)
  })
  
  out <- out[sapply(out, is.data.frame)]
  
  if (length(out) > 0)
  {
    output <- do.call(rbind, out)
    
    if (relabel_colms)
      output <- relabel_colms(output, waveform = !any(grepl("Spectrogram", output$View)))
    
    output <- output[, c(ncol(output), 2:(ncol(output) - 1))]  
    
    return(output)} else return(NULL)
}
