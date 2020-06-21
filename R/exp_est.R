#' Export wave objects of extended selection tables as sound files  
#' 
#' \code{exp_est} exports wave objects of an extended selection table as sound files
#' @usage exp_est(X, file.name = NULL, path = NULL, single.file = FALSE, 
#' selection.table = TRUE, pb = TRUE, normalize = TRUE, parallel = 1, wave.object = FALSE)  
#' @param  X object of class 'extended_selection_table' (objects produced by \code{\link[warbleR]{selection_table}}). More details about these objects can be found on \href{https://marce10.github.io/2018/05/15/Extended_selection_tables.html}{this link}.
#' @param file.name character string indicating the name of the sound file (if \code{single.file = TRUE}) 
#' and/or the selection table (if \code{selection.table = TRUE}). Default is \code{NULL}.
#' @param path A character string indicating the path of the directory where sound files and/or selection table will be saved. If not provided the
#' function uses the current working directory. Default is \code{NULL}.
#' @param single.file Logical argument to control if all wave objects are pooled together in a 
#' single sound file (if \code{TRUE}) or each one as an individual sound file (if \code{FALSE}, default). If 
#' exporting a single sound file the files are pasted in the same sequences as in the extended selection table. Note that to create a single sound file ALL WAVE OBJECTS IN 'X" MUST HAVE THE SAME SAMPLE RATE (check \code{attributes(X)$check.res$sample.rate}) and ideally the same bit depth (although not strictly required). If that is not the case, sample rate can be homogenize using the \code{\link[warbleR]{resample_est}} from the warbleR package.
#' @param selection.table Logical argument to determine if a Raven sound selection table ('.txt' file) is also exported. 
#' Default is \code{TRUE}. If \code{FALSE} then selection table is return as an object in the R environment. If exporting multiple sound files (if \code{single.file = FALSE}) the function still exports a single selection table (in this case a multiple sound selection table).
#' @param pb Logical argument to control progress bar when exporting multiple sound files. Default is \code{TRUE}.
#' @param normalize Logical argument to control if wave objects are individually normalized before exporting (or before being pasted together if \code{single.file = TRUE}). Normalization rescales amplitude values to a 16 bit dynamic range. Default is \code{FALSE}.
#' @param parallel Numeric. Controls whether parallel computing is applied.
#'  It specifies the number of cores to be used. Default is 1 (i.e. no parallel computing).
#' @param wave.object Logical argument to control if ONLY a single wave object is returned in the R environment (TRUE) instead of a wave file in the working directory (and a selection table if \code{selection.table = TRUE}). Default is \code{FALSE}.
#' @return Sound file(s) are saved in the provided path or current working directory. If \code{selection.table = TRUE} a Raven sound selection table with the data in 'X' will also be saved.
#' @details The function takes wave objects contained as attributes in extended selection 
#' tables and saves them as sound files in '.wav' format. A single or several sound files can be produced (see 'single.file' argument).  In addition, a Raven sound selection table can be saved along with the sound files. The exported selection table can be open in Raven for exploring/manipulating selections in 'X'. 
#' @seealso \code{\link{exp_raven}}
#' @export
#' @examples \dontrun{
#'# load example data
#'data(list = "lbh.est", package = "NatureSounds")
#' 
#' # subset to 10 selections
#' X <- lbh.est[1:10, ]
#' 
#' # Export data to a single sound file
#' exp_est(X, file.name = "test", single.file = TRUE, path = tempdir())
#' 
#' # Export data to a single sound file and normalizing, no pb
#' exp_est(X, file.name = "test2", single.file = TRUE, normalize = TRUE, pb = FALSE, path = tempdir())
#' 
#' # several files
#' exp_est(X, single.file = FALSE, file.name = "test3", path = tempdir())
#' }
#' @name exp_est
#' 
#' @author Marcelo Araya-Salas (\email{marcelo.araya@@ucr.ac.cr})
#last modification on mar-11-2019

exp_est <- function(X, file.name = NULL, path = NULL, single.file = FALSE,  
                    selection.table = TRUE, pb = TRUE, normalize = TRUE, 
                    parallel = 1, wave.object = FALSE)
{
  
  #check path to working directory
  if (is.null(path)) path <- getwd() else if (!dir.exists(path)) stop("'path' provided does not exist") else
    path <- normalizePath(path)
 
  # if wave object then single file and no selection table
  if (wave.object) {
    
    # single file
    single.file <- TRUE
   
    # no selection table
    selection.table <- FALSE
  }
  
  
  # if file.name 
  if (is.null(file.name) & selection.table) stop("'file.name' must be provided when 'selection.table' is TRUE")
  # set progress bar back to original
  on.exit(pbapply::pboptions(type = .Options$pboptions$type), 
          add = TRUE)
  
  # check X is data.frame
  if (!warbleR::is_extended_selection_table(X))
      stop("X must be of class 'extended_selection_table'")
    
  # set progress bar
  if (pb) pbapply::pboptions(type = "timer") else 
    pbapply::pboptions(type = "none")
    
  # all wave objects in a list
  wvs <- attributes(X)$wave.objects
  
  # if exporting 1 sound file
  if (single.file)
  {
    # check if all wavs have the same sample rate
    if (length(unique(attributes(X)$check.res$sample.rate)) > 1)
      stop("to create a single file all wave objets in the extended selection table must have the same sample rate (check 'attributes(X)$check.res$sample.rate', use warbleR's resample_est() function to homogenize sample rate)")
      
    # normalize
    if (normalize)
      wvs <- lapply(wvs, tuneR::normalize, unit = "16", rescale = TRUE)
    
      # add .wav at the end of file.name if not included
    if (!wave.object)
      if(!grepl("\\.wav$", file.name, ignore.case = TRUE)) file.name <- paste0(file.name, ".wav")
    
    # get first wave object  
    sngl.wv <- wvs[[1]]
    
    # loop to put together waves in a single wave
    if(length(wvs) > 1)  
    for(i in 2:length(wvs))
      sngl.wv <- seewave::pastew(wave1 = wvs[[i]], wave2 = sngl.wv, output = "Wave")
  
    # save single wave 
    if(!wave.object)
    suppressWarnings(tuneR::writeWave(object = sngl.wv, filename = file.path(path, file.name), extensible = FALSE))
    } else

  # if no single file
  out <- pbapply::pbsapply(1:length(wvs), function(x)
  {
    file.name <- names(wvs)[[x]]
    
    if(!grepl("\\.wav$", file.name, ignore.case = TRUE)) file.name <- paste0(file.name, ".wav")
    
    if(normalize)
      wv <- tuneR::normalize(wvs[[x]], unit = "16", rescale = TRUE) else wv <- wvs[[x]]
    
    suppressWarnings(tuneR::writeWave(object = wv, filename = file.path(path, file.name), extensible = FALSE))
    }
    )
  
    st <- as.data.frame(X)

    if (selection.table)    
     { 
      if (single.file)
      {
        # add extension
        if(!grepl("\\.wav$", file.name, ignore.case = TRUE)) file.name <- paste0(file.name, ".wav")
        
        # use single name 
        st$sound.files <- file.name
        
        # number each selection
        st$selec <- 1:nrow(st)
        
        # get durations of individual waves
        durs <- sapply(wvs, seewave::duration)
        
        # get cummulative duration
        cumdur <- c(0, cumsum(durs)[-length(durs)])
        
        # if est was created by song
        if (attributes(X)$by.song$by.song){
          sls.x.rec <- tapply(X$sound.files, X$sound.files, length)
          
          # order as in wvs order
          sls.x.rec <- sls.x.rec[match(names(sls.x.rec), names(wvs))]
          
          cumdur <- as.vector(rep(cumdur, sls.x.rec))
            }  
          
            
        # add cummulative time
        st$start <- st$start + cumdur
        st$end <- st$end + cumdur
        
        # export data
        if (selection.table)
        exp_raven(X = st, path = path, file.name = gsub("\\.wav$", ".txt", file.name, ignore.case = TRUE), sound.file.path = path, pb = pb, parallel = parallel)
        } else
        {
        #fix sound file names in st
        st$sound.files <- ifelse(grepl("\\.wav$", st$sound.files, ignore.case = TRUE), st$sound.files, paste0( st$sound.files, ".wav"))
        
        # export selection table
        if (selection.table)
          exp_raven(X = st, path = path, file.name = gsub("\\.wav$", ".txt", file.name, ignore.case = TRUE), sound.file.path = path, pb = FALSE, parallel = 1, single.file = TRUE)
        }
    } else
    {
      # return something if no selection table
      #fix sound file names in st
      st$sound.files <- ifelse(grepl("\\.wav$", st$sound.files, ignore.case = TRUE), st$sound.files, paste0( st$sound.files, ".wav"))
    
      if(wave.object)
         return(sngl.wv) else
               return(st)
      }
}
