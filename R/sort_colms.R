#' sort column names in a more intuitive order

sort_colms <- function(X){
  
if(any(duplicated(names(X)))) stop(" Duplicated column names must be fixed first")  
  
mtch <- match(c("sound.files", "channel", "selec", "start", "end", "top.freq", "bottom.freq"), names(X))
  
mtch <- mtch[!is.na(mtch)]

X <- X[,c(mtch, setdiff(1:ncol(X), mtch))]

return(X)
}