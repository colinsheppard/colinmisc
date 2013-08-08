intdatefstr <- function(strdate,format='%Y-%m-%d %H:%M:%S'){
	unclass(as.POSIXct(strptime(strdate,format=format)))[1]
}
