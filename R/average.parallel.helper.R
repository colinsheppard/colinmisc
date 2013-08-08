average.parallel.helper <-
function(break.pair,inttimes,data,start.time,interval){
  ll <- break.pair[1]
  ul <- break.pair[2]
  avg.new <- as.data.frame(array(NA,dim=c(ul-ll+1,ncol(data)),dimnames=list(as.character(ll:ul),names(data))))
  for(i in ll:ul){
    begin <- start.time + (i-1)*interval
    end <- start.time + i*interval
    rows <- which(inttimes>=begin & inttimes<end)

    avg.new[as.character(i),1] <- as.POSIXct(begin,origin="1970-01-01",tz="PST")
    avg.new[as.character(i),2:ncol(data)] <- apply(data[rows,2:(ncol(data))],MARGIN=2,FUN=mean,na.rm=T)
  }
  return(avg.new)
}
