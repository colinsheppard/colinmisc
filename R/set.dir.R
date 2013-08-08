set.dir <- function(dir='',base.path=''){
  dir.path <- paste(base.path,dir,sep='')
  if(file.exists(dir.path)){
    setwd(dir.path)
  }else{
    stop(paste("Error:",dir.path," does not exist or cannot be found"))
  }
}
