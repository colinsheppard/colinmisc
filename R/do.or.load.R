do.or.load <- function(file.path,fun,...){
  if(!file.exists(file.path)){
    # do function call here so that vars are added to the frame of this function call....
    res <- fun(...)
    attach(res,warn.conflicts=F)
    streval(pp('save(',pp(names(res),collapse=','),',file="',file.path,'")'))
    my.cat(pp('Variables: ',pp(names(res),collapse=','),' created and saved to ',file.path))
    detach(res)
  }else{
    my.cat(pp('Loading: ',file.path))
  }
  load(file.path,parent.frame(2),verbose = T)
}

