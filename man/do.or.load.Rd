\name{do.or.load}
\alias{do.or.load}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function that either loads an Rdata file, or saves the result of a function call to the Rdata file.
%%  ~~function to do ... ~~
}
\description{
Function that either loads an Rdata file, or saves the result of a function call to the Rdata file.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
# example usage
do.or.load (file.path,fun,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
%%     ~~Describe \code{dir} here~~
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
do.or.load('test.Rdata',function(x){ 
  x<-1
  y<-2
  list('x.var'=x,'y.var'=y)
})
}
