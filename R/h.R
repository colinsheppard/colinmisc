h <- function(x,...){
  my.cat(pp('Dim: ',pp(dim(x),collapse=" x ")))
  head(x,...)
}
