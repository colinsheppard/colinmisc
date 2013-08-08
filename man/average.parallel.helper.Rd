\name{average.parallel.helper}
\alias{average.parallel.helper}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Average Parallel Helper
%%  ~~function to do ... ~~
}
\description{
Average Parallel Helper
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
average.parallel.helper(break.pair, inttimes, data, start.time, interval)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{break.pair}{
%%     ~~Describe \code{break.pair} here~~
}
  \item{inttimes}{
%%     ~~Describe \code{inttimes} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{start.time}{
%%     ~~Describe \code{start.time} here~~
}
  \item{interval}{
%%     ~~Describe \code{interval} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
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
function (break.pair, inttimes, data, start.time, interval) 
{
    ll <- break.pair[1]
    ul <- break.pair[2]
    avg.new <- as.data.frame(array(NA, dim = c(ul - ll + 1, ncol(data)), 
        dimnames = list(as.character(ll:ul), names(data))))
    for (i in ll:ul) {
        begin <- start.time + (i - 1) * interval
        end <- start.time + i * interval
        rows <- which(inttimes >= begin & inttimes < end)
        avg.new[as.character(i), 1] <- as.POSIXct(begin, origin = "1970-01-01", 
            tz = "PST")
        avg.new[as.character(i), 2:ncol(data)] <- apply(data[rows, 
            2:(ncol(data))], MARGIN = 2, FUN = mean, na.rm = T)
    }
    return(avg.new)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
