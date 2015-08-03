\name{to.posix}
\alias{to.posix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alias for as.POSIXct(strptime(...))
%%  ~~function to do ... ~~
}
\description{
Alias to the command command converting a string to a posix ct object.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
to.posix (x,fmt="%Y-%m-%d")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
An array of string dates.
}
  \item{fmt}{
Datetime format as described in strftime.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
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
to.posix <- function(x,fmt="%Y-%m-%d"){
  as.POSIXct(strptime(x,fmt))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
