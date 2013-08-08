\name{average.columns}
\alias{average.columns}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average Columns by Time Interval
%%  ~~function to do ... ~~
}
\description{
This function averages the columns of a matrix or a data frame by interval (e.g. seconds, minutes, days) beginning at start.time (must be in POSIXct format).  The first column should contain the dates (in POSIXct format).  If the data are not more finely discretized than the interval, some NaN values will emerge.  NaN will also result if the start.time or end.time are outside the range of the data.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
average.columns(data, interval = 3600, interval.type = "sec", start.time = NA, end.time = NA, use.snow = F, num.cpu = 1, weighting = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
A matrix or data frame where the first column contains data in POSIXct.
%%     ~~Describe \code{data} here~~
}
  \item{interval}{
A scalar number for the quantify of units of interval.type to average over.  E.g. if interval.type is "sec" and interval is 3600, then hourly averages will be computed (1 hour == 3600 seconds).
%%     ~~Describe \code{interval} here~~
}
  \item{interval.type}{
A character string specifying the units for interval.  E.g. whether inteval in seconds or minutes or hours, etc.  Valid values for interval.type are "sec", "min", "hour", "day", "month", "year", "DSTday".  Use "DSTday" instead of day to avoid daylight savings issues (it will assume all data are in standard time).  
%%     ~~Describe \code{interval.type} here~~
}
  \item{start.time}{
The beginning timestamp desired in the resulting matrix or data.frame.
%%     ~~Describe \code{start.time} here~~
}
  \item{end.time}{
The ending timestamp desired in the resulting matrix or data.frame.
%%     ~~Describe \code{end.time} here~~
}
  \item{use.snow}{
Logical specifying whether the R Package "snow" be employed to parallelize the computations.
%%     ~~Describe \code{use.snow} here~~
}
  \item{num.cpu}{
If use.snow is TRUE, this argument is used to define the number of nodes in the snow cluster to create, it is most efficient if this number is no larger than the number of (virtual) CPUS on your machine. 
%%     ~~Describe \code{num.cpu} here~~
}
  \item{weighting}{
Logical, if weighting is TRUE, the data will be weighted by the interval of time they represent (1/2 of the sum in the forward/backward looking directions with special cases for the end points).  This is only relevant for irregularly spaced data.  For example if the average interval is 1 hour and there are 30 regularly spaced samples from the first 30 mintutes of the hour and 1 sample in the second half of the hour (at minute 45), then the first sample would recieve a relative weighting of 1.5, the next 28 samples would each receive a weighting of 1, the 30th sample would receive a weighting of 8, and the last sample would receive a weighting of 22.5.
%%     ~~Describe \code{weighting} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A matrix or data frame containing the same columns as data, but with rows averaged over the specified time interval.
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Colin Sheppard, Schatz Energy Research Center, colin@humboldt.edu
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (data, interval = 3600, interval.type = "sec", start.time = NA, 
    end.time = NA, use.snow = F, num.cpu = 1, weighting = FALSE) 
{
    if (ncol(data) == 2) {
        data <- cbind(data, array(0, nrow(data)))
        dummy.column.added = T
    }
    else {
        dummy.column.added = F
    }
    time.part.list <- list(sec = 1, min = 2, hour = 3, day = 4, 
        month = 5, year = 6, DSTday = 4)
    if (use.snow) {
        if (interval.type != "sec") {
            logger("Error: use.snow only compatible with interval.type='sec', stopping....", 
                scenario.tag)
            stop()
        }
        library("snow")
        if (exists("cl")) 
            stopCluster(cl)
        rm("cl")
        cl <- makeSOCKcluster(rep("localhost", num.cpu))
        breaks <- seq(1, numrows, by = ceiling(numrows/length(cl)))
        break.pairs <- list()
        for (i in 1:(length(breaks) - 1)) {
            break.pairs[[i]] <- c(breaks[i], breaks[i + 1] - 
                1)
        }
        break.pairs[[i + 1]] <- c(breaks[i + 1], numrows)
        clusterEvalQ(cl, rm(inttimes, data, start.time, interval, 
            average.parallel.helper))
        clusterExport(cl, list("average.parallel.helper"))
        results <- clusterApply(cl, break.pairs, fun = "average.parallel.helper", 
            inttimes, data, start.time, interval)
        for (i in 1:length(results)) {
            avg[break.pairs[[i]][1]:break.pairs[[i]][2], ] <- results[[i]]
        }
        stopCluster(cl)
    }
    else {
        null.time <- c(rep(0, 3), 1, rep(0, 6))
        data.times <- as.POSIXct(data[, 1], origin = "1970-01-01", 
            tz = "PST")
        data.times.secs <- unclass(data[, 1])
        data.times.weights <- matrix(1, nrow = length(data.times.secs))
        if (weighting) {
            time.int <- (data.times.secs[2] - data.times.secs[1])/2
            data.times.weights[1] <- time.int
            time.int <- (data.times.secs[length(data.times.secs)] - 
                data.times.secs[length(data.times.secs) - 1])/2
            data.times.weights[length(data.times.secs)] <- time.int
            for (i in 2:(length(data.times.secs) - 1)) {
                time.int <- (data.times.secs[i] - data.times.secs[i - 
                  1])/2 + (data.times.secs[i + 1] - data.times.secs[i])/2
                data.times.weights[i] <- time.int
            }
        }
        if (is.na(start.time)) {
            start.time <- data[1, 1]
        }
        if (is.na(end.time)) {
            end.time <- data[nrow(data), 1]
        }
        start.avg <- as.POSIXlt(start.time, origin = "1970-01-01", 
            tz = "PST")
        end.avg <- as.POSIXlt(end.time, origin = "1970-01-01", 
            tz = "PST")
        if (interval.type != "sec") {
            time.part <- time.part.list[[interval.type]]
            for (i in 1:(time.part - 1)) {
                start.avg[[i]] <- null.time[i]
            }
        }
        avg.times <- seq.POSIXt(from = start.avg, to = end.avg, 
            by = paste(interval, interval.type, sep = " "))
        numrows <- length(avg.times)
        avg <- as.data.frame(array(NA, dim = c(numrows, ncol(data)), 
            dimnames = list(as.character(1:numrows), names(data))))
        avg[, 1] <- avg.times
        found.indices <- findInterval(data[, 1], avg.times)
        if (!weighting) {
            cat("col:")
            numcols <- ncol(data)
            if (dummy.column.added) 
                numcols <- ncol(data) - 1
            for (col in 2:numcols) {
                cat(paste(col, ",", sep = ""))
                tmp <- mapply(data[, col], found.indices, FUN = mean, 
                  na.rm = T)
                missing.i <- which(!1:numrows \%in\% found.indices)
                non.missing.begin <- 1
                if (length(missing.i) > 1) {
                  for (j in 2:(length(missing.i) - 1)) {
                    if (non.missing.begin < missing.i[j]) {
                      avg[non.missing.begin:(missing.i[j] - 1), 
                        col] <- tmp[non.missing.begin:(missing.i[j] - 
                        1)]
                    }
                    avg[missing.i[j], col] <- NA
                    non.missing.begin <- missing.i[j] + 1
                  }
                }
                if (non.missing.begin <= numrows) {
                  avg[non.missing.begin:numrows, col] <- tmp[non.missing.begin:numrows]
                }
            }
        }
        else {
            for (i in 1:numrows) {
                begin <- avg.times[i]
                end <- seq.POSIXt(begin, by = paste(interval, 
                  interval.type, sep = " "), length.out = 2)[2]
                active.rows <- which(data.times >= begin & data.times < 
                  end)
                avg[i, 2:ncol(data)] <- apply(data[active.rows, 
                  2:(ncol(data))], MARGIN = 2, FUN = weighted.mean, 
                  na.rm = T, w = data.times.weights[active.rows])
            }
        }
    }
    if (dummy.column.added) {
        return(avg[, 1:(ncol(avg) - 1)])
    }
    else {
        return(avg)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
