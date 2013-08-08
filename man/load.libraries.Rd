\name{load.libraries}
\alias{load.libraries}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load (and Install) Libraries
%%  ~~function to do ... ~~
}
\description{
Load (and Install) Libraries
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
load.libraries(needed.packs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{needed.packs}{
%%     ~~Describe \code{needed.packs} here~~
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
function (needed.packs) 
{
    installed.packs <- installed.packages()
    for (pack in needed.packs) {
        if (length(grep(pack, installed.packs)) == 0) {
            install.packages(pack)
        }
        library(pack, character.only = T)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
