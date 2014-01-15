h <- function(x,...){
  my.cat(pp('Dim: ',pp(dim(x),collapse=" x ")))
  if(is.data.table(x))my.cat(pp('Key: ',pp(key(x),collapse=", ")))
  head(x,...)
}
