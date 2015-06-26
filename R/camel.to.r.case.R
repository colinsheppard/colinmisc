camel.to.r.case <- function(camel.str){
  sapply(camel.str,function(cam.str){ 
    inds <- str_locate_all(cam.str,"[A-Z]")[[1]][,1]
    cam.str <- tolower(cam.str)
    r.str <- ''
    while(length(inds)>0){
      r.str <- pp(r.str,substr(cam.str,0,inds[1]-1),'.')
      cam.str <- substr(cam.str,inds[1],nchar(cam.str))
      inds <- tail(inds,-1) - (inds[1] - 1)
    }
    r.str <- pp(ifelse(substr(r.str,1,1)==".",substr(r.str,2,nchar(r.str)),r.str),cam.str)
    r.str
  },USE.NAMES=F)
}
