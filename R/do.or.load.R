do.or.load <- function(file.path,fun,args.to.save=NA,...){
  if(!is.na(args.to.save)){
    suppressWarnings(streval(pp('rm("',pp(args.to.save,collapse='","'),'",inherits=T)')))
  }
  if(!file.exists(file.path)){
    # do function call here so that vars are added to the frame of this function call....
    res <- fun(...)
    my.cat(res)
    attach(res,warn.conflicts=F)
    streval(pp('save(',pp(names(res),collapse=','),',file="',file.path,'")'))
    my.cat(pp('Variables: ',pp(names(res),collapse=','),' created and saved to ',file.path))
    detach(res)
  }else{
    my.cat(pp('Loading: ',file.path))
  }
  load(file.path,envir=parent.frame(2),verbose = T)
}

