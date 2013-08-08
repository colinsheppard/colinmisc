\name{set.dir}
\alias{set.dir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Set Working Directory
%%  ~~function to do ... ~~
}
\description{
Set Working Directory
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
set.dir(dir = "",base.path = "")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dir}{
%%     ~~Describe \code{dir} here~~
}
  \item{base.path}{
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
function (dir = "") 
{
    dir.path <- paste(model.dir, dir, sep = "")
    if (file.exists(dir.path)) {
        setwd(dir.path)
    }
    else {
        logger(paste("Error:", dir.path, " does not exist or cannot be found"), 
            scenario.tag)
        stop()
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
