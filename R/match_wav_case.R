match_wav_case <- function(X, path = NULL, output = "data.frame"){

# get path if not provided    
if(is.null(path)) path <- getwd()

# list  wav files in path
wvs <- list.files(path = path, pattern = "\\.wav$", ignore.case = TRUE)

# remove extension for both names in selec.table and wavs
sf.no.ext <- gsub(".wav", "", X$sound.files, ignore.case = TRUE)
wvs.no.ext <- gsub(".wav", "", wvs, ignore.case = TRUE)

# get right extension
fix.sf <- sapply(1:nrow(X), function(i) ifelse(sf.no.ext[i] %in% wvs.no.ext, wvs[wvs.no.ext == sf.no.ext[i]], sf.no.ext[i]))

# warning if not all find found
if(length(which(!sf.no.ext %in% wvs.no.ext)) > 0) cat(paste("the following files were not found:", paste(sf.no.ext[which(!sf.no.ext %in% wvs.no.ext)], collapse = "/")))

# overwrite sound.files column with fix names
X$sound.files <- fix.sf

if(output == "data.frame")  return(X) else return(fix.sf)
}



