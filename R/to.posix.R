to.posix <- function(x,fmt="%Y-%m-%d",tz=""){
  as.POSIXct(strptime(x,fmt,tz))
}
