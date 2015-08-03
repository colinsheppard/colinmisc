\name{my.cat}
\alias{my.cat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Alias for cat with a newline added to end.
%%  ~~function to do ... ~~
}
\description{
Alias to the cat command with a newline added to the end of the message string.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
my.cat (message, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
%%     ~~Describe \code{dir} here~~
}
  \item{sep}{
%%     ~~Describe \code{dir} here~~
}
  \item{collapse}{
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
my.cat <- function(message,...){
  cat(paste(message,"\n",sep=''),...)
}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
