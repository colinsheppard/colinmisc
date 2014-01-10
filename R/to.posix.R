to.posix <- function(x,fmt="%Y-%m-%d"){
  as.POSIXct(strptime(x,fmt))
}
