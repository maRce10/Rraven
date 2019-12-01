#' Fix the extension case of sound files
#' 
#' \code{match_wav_case} fixes the extension case of sound files in a selection table.
#' @usage match_wav_case(X, path = NULL, output = "data.frame", verbose = TRUE)
#' @param X Data frame containing columns for sound file (sound.files) and selection
#'  (selec). See example data 'lbh_selec_table' in the \code{\link{warbleR}}) package.
#' @param path A character string indicating the path of the directory in which to look for sound files. 
#' If not provided (default) the function searches into the current working directory.
#' @param output Character string. Controls whether a complete data frame ('data.frame') 
#' or only the sound file names ("names") are returned. Default is 'data.frame'. 
#' @param verbose Logical to control if messages are printed (\code{TRUE}, default).
#' @return  The same data as in the input data frame but with the case of the extension file names in the 'sound.files' column matching those of the sound files themselves. 
#' @details The function returns the data from the input data frame with extension file
#' names in the 'sound.files' column matching those of the sound files (in case there was
#' any mismatch). The function needs the path to the sound files to compare extension names.
#' @seealso \code{\link{relabel_colms}} 
#' @export
#' @name match_wav_case
#' @examples{
#' library(warbleR)
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", 
#' "lbh_selec_table"))
#' 
#' tuneR::writeWave(Phae.long1, file.path(tempdir(), 
#' "Phae.long1.wav"), extensible = FALSE) #save sound files 
#' tuneR::writeWave(Phae.long2, file.path(tempdir(), 
#' "Phae.long2.wav"), extensible = FALSE)
#' tuneR::writeWave(Phae.long3, file.path(tempdir(), 
#' "Phae.long3.wav"), extensible = FALSE)
#' tuneR::writeWave(Phae.long4, file.path(tempdir(), 
#' "Phae.long4.wav"), extensible = FALSE)
#'
#' # change one extension
#' lbh_selec_table$sound.files <- as.character(lbh_selec_table$sound.files)
#' lbh_selec_table$sound.files[1] <- gsub(".wav$", ".WAV", lbh_selec_table$sound.files[1]) 
#'  
#'  # fixed extension an return data frame
#'  match_wav_case(X = lbh_selec_table, path = tempdir())
#'   
#'  # fixed extension an return sound file names
#'  match_wav_case(X = lbh_selec_table, output = "names", path = tempdir())
#'  }   
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on sep-02-2019
match_wav_case <- function(X, path = NULL, output = "data.frame", verbose = TRUE){

# get path if not provided    
if (is.null(path)) path <- getwd()

# list  wav files in path
wvs <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)

if (length(wvs) == 0) stop("no sound files are found in working directory")

# remove extension for both names in lbh_selec_table and wavs
sf.no.ext <- gsub(".wav", "", X$sound.files, ignore.case = TRUE)
wvs.no.ext <- gsub(".wav", "", wvs, ignore.case = TRUE)

# get right extension
fix.sf <- sapply(1:nrow(X), function(i) ifelse(sf.no.ext[i] %in% wvs.no.ext, wvs[wvs.no.ext == sf.no.ext[i]], sf.no.ext[i]))

# warning if not all find found
if (any(!sf.no.ext %in% wvs.no.ext) & verbose) cat(paste("the following files were not found:", paste(unique(sf.no.ext[!sf.no.ext %in% wvs.no.ext]), collapse = "/")))

# overwrite sound.files column with fix names
X$sound.files <- fix.sf

if (output == "data.frame")  return(X) else return(fix.sf)
}
