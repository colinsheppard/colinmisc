
average.columns<-function(data,interval=3600,interval.type="sec",start.time=NA,end.time=NA,use.snow=F,num.cpu=1,weighting=FALSE){
  # using apply on data means that we must have more than one column to average
  if(ncol(data)==2){
    # append a column of 0's
    data <- cbind(data,array(0,nrow(data)))
    dummy.column.added=T
  }else{
    dummy.column.added=F
  }
  # Lists of valid interval.types with corresponding POSIXlt part ID #	

  ###
	#time.part.list<-list("sec", "min", "hour", "day", "month", "year", "DSTday")
	#time.part.index<-list(1,      2,     3,       4,    5,       6,      4     )
  ###
	time.part.list<-list(sec=1, min=2, hour=3, day=4, month=5, year=6, DSTday=4)
	
  # use.snow is only valid for interval.type="sec"  
  if(use.snow){
    if(interval.type!="sec"){
      #logger("Error: use.snow only compatible with interval.type='sec', stopping....",scenario.tag)
      stop()
    }
    library('snow')
    if(exists('cl'))stopCluster(cl)
    rm('cl')
    cl <- makeSOCKcluster(rep("localhost",num.cpu))
    breaks <- seq(1,numrows,by=ceiling(numrows/length(cl)))
    break.pairs <- list()
    for(i in 1:(length(breaks)-1)){
      break.pairs[[i]] <- c(breaks[i],breaks[i+1]-1)
    }
    inttimes <- as.numeric(data[,1])
    break.pairs[[i+1]] <- c(breaks[i+1],numrows)
    clusterEvalQ(cl,rm(inttimes,data,start.time,interval,average.parallel.helper))
    clusterExport(cl,list("average.parallel.helper"))
    results<-clusterApply(cl,break.pairs,fun='average.parallel.helper',inttimes,data,start.time,interval)
    for(i in 1:length(results)){
      avg[break.pairs[[i]][1]:break.pairs[[i]][2],] <- results[[i]]
    }
    stopCluster(cl)
  }else{
    # Single Core processing:  	
        

    # Make matrix of "reset times" that are used to define the null value of a POSIXlt field

    ###
    #null.time<-matrix(0,nrow=10)
    #null.time[4]<-1 #1st day of month
    ###
    ### if null.time is one dimensional, specify an array for simplicity 
    null.time<-c(rep(0,3),1,rep(0,6))

    ###
    #data.times<-as.POSIXct(data[,1])
    ###
    data.times<-as.POSIXct(data[,1],origin="1970-01-01",tz="PST")
    data.times.secs<-unclass(data[,1])
    data.times.weights<-matrix(1,nrow=length(data.times.secs))

    # If Active, weight datapoints by time interval (1/2 of each forward/backward looking interval)
    if(weighting){
      #Special cases - first datapoint and last datapoint
      time.int<-(data.times.secs[2]-data.times.secs[1])/2  #Special cases - first datapoint and last datapoint
      data.times.weights[1]<-time.int

      time.int<-(data.times.secs[length(data.times.secs)]-data.times.secs[length(data.times.secs)-1])/2
      data.times.weights[length(data.times.secs)]<-time.int
      
      #The rest of the datapoints	
      for(i in 2:(length(data.times.secs)-1)){
        time.int<-(data.times.secs[i]-data.times.secs[i-1])/2+(data.times.secs[i+1]-data.times.secs[i])/2
        data.times.weights[i]<-time.int
      }
    }
    # Intuit the start and end times------------------------------
    ###
    #if(is.na(start.time)){start.time<-data[,1][1]} 
    #if(is.na(end.time)){end.time<-data[,1][length(data[,1])]} 
    ###
    if(is.na(start.time)){start.time<-data[1,1]} 
    if(is.na(end.time)){end.time<-data[nrow(data),1]}

    # Convert to POSIXlt for rounding
    ###
    #start.time <- as.POSIXlt(start.time)
    #end.time   <- as.POSIXlt(end.time)
    ###
    ### confusing for the same variable name to be used differently (ct vs. lt) in the same functon 
    start.avg <- as.POSIXlt(start.time,origin="1970-01-01",tz="PST")
    end.avg <- as.POSIXlt(end.time,origin="1970-01-01",tz="PST")

    if(interval.type!="sec"){ # Find the operating part of the POSIXlt list to work with if not seconds
      ###
      #time.part<-as.numeric(time.part.index[match(interval.type, time.part.list)])
      ###
      time.part<-time.part.list[[interval.type]]

      # Create the avg dataframe, populate the time column with POSIXct values
      for(i in 1:(time.part-1)){
        start.avg[[i]]<-null.time[i]  #Make "evenly" spaced - i.e. reset to beginning of time.part
      }
    }
      
    # Create the avg dataframe, populate the time column with POSIXct values
    avg.times<-seq.POSIXt(from=start.avg, to=end.avg, by=paste(interval, interval.type, sep=" "))
    numrows<-length(avg.times)
    avg <- as.data.frame(array(NA,dim=c(numrows,ncol(data)),dimnames=list(as.character(1:numrows),names(data))))
    ###
    #avg[[1]]<-as.POSIXct(avg.times)
    ###
    ### already class POSIXct
    avg[,1]<-avg.times

    # Sort the indices of avg.times for which the dates in data[,1] fall
    found.indices <- findInterval(data[,1],avg.times)

    if(!weighting){
      cat('col:')
      numcols <- ncol(data)
      if(dummy.column.added)numcols <- ncol(data)-1
      for(col in 2:numcols){
        cat(paste(col,',',sep=''))
        tmp <- mapply(data[,col],found.indices,FUN=mean,na.rm=T)
        missing.i <- which(!1:numrows %in% found.indices)
        non.missing.begin <- 1
        if(length(missing.i)>1){
          for(j in 2:(length(missing.i)-1)){
            if(non.missing.begin<missing.i[j]){    
              avg[non.missing.begin:(missing.i[j]-1),col] <- tmp[non.missing.begin:(missing.i[j]-1)]
            }
            avg[missing.i[j],col] <- NA
            non.missing.begin <- missing.i[j]+1
          }
        }
        if(non.missing.begin<=numrows){
          avg[non.missing.begin:numrows,col] <- tmp[non.missing.begin:numrows]
        }
      }
    }else{
      # Work through the rows, averaging the data that fall within the time intervals
      for(i in 1:numrows){
        ###
        #begin<-avg[[1]][i] # ith row of the time column
        ###
        begin<-avg.times[i]
        ###
        end<-seq.POSIXt(begin, by=paste(interval, interval.type, sep=" "), length.out=2)[2] # one step in the sequence

        active.rows<-which(data.times>=begin & data.times<end)
        avg[i,2:ncol(data)]<-apply(data[active.rows,2:(ncol(data))],MARGIN=2,FUN=weighted.mean,na.rm=T, w=data.times.weights[active.rows])
      }
    }
  }
    
  # End of Function, return values
  if(dummy.column.added){
    return(avg[,1:(ncol(avg)-1)])
  }else{
    return(avg)
  }
}

