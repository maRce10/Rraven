#' Export a 'Raven' selection for all sound files in a folder
#' 
#' \code{exp_empty_sels} exports a 'Raven' selection data in .txt format that includes empty selections for all sound files in a folder.
#' @usage exp_empty_sels(path = NULL, file.name = NULL, pb = TRUE)
#' @param path A character string indicating the path of the directory in which to look for sound files.
#' If not provided (default) the function will use the current working directory.
#' @param file.name Name of the output .txt file. If \code{NULL} then the folder name is used instead.
#' @param pb Logical argument to control progress bar. Default is \code{TRUE}.
#' @return The function saves a selection table in '.txt' format that can be 
#' directly opened in Raven.
#' No objects are returned in the R environment.
#' @details The function saves a selection file in '.txt' format (that can be 
#' directly opened in Raven) that will display all sound files in the provided
#' directory (argument 'path'). Useful to simplify the making of selections from several sound files that need to be displayed simultaneously (e.g. several recordings from the same individual). The selection file is saved in the provided directory ('path').
#' @seealso \code{\link{exp_raven}} 
#' @export
#' @name exp_empty_sels
#' @examples
#' 
#' # Load data
#' library(NatureSounds)
#' data(list = c("Phae.long1", "Phae.long2", "Phae.long3", "Phae.long4", "lbh_selec_table"))
#' 
#' ## Export a single selection table including multiple files
#' #save sound files 
#' tuneR::writeWave(Phae.long1, file.path(tempdir(), 
#' "Phae.long1.wav"), extensible = FALSE) 
#' tuneR::writeWave(Phae.long2, file.path(tempdir(), 
#' "Phae.long2.wav"), extensible = FALSE)
#' tuneR::writeWave(Phae.long3, file.path(tempdir(), 
#' "Phae.long3.wav"), extensible = FALSE)
#' tuneR::writeWave(Phae.long4, file.path(tempdir(), 
#' "Phae.long4.wav"), extensible = FALSE)
#' 
#' # export with no file name
#' exp_empty_sels(path = tempdir())
#' 
#' # export with file name
#' exp_empty_sels(file.name = "Phaethornis.longirostris", path = tempdir())
#' 
#' @author Marcelo Araya-Salas (\email{marceloa27@@gmail.com})
#last modification on oct-12-2018

exp_empty_sels <- function(path = NULL, file.name = NULL, pb = TRUE){
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else 
    if (!dir.exists(path)) stop("'path' provided does not exist") 
  
  # create a selection table for each sound file
  st <- warbleR::selection_table(whole.recs = TRUE, pb = FALSE, path = path)
  
  # set start end at 0 and top bottom at 1 kHz
  st$end <- 0
  st$top.freq <- 1
  st$bottom.freq <- 1
  st$selec <- 1:nrow(st)
  
  # add an extra column with sound file names
  st$Rraven.labels <- st$sound.files
  
  # set file name as folder name if not provided
  if (is.null(file.name)) file.name <- paste0(basename(path), ".selection.table")
  
  # export selection
  exp_raven(X = st, sound.file.path = path, file.name = file.name, path = path)
  
}
