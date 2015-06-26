\name{join.on}
\alias{join.on}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function that encapsulates common pre- and post- join operations with data tables.
%%  ~~function to do ... ~~
}
\description{
Function that encapsulates common pre- and post- join operations with data tables. Data table 2 is joined to data table 1 on keys that do not need to have the exact same names. The user can optionally specify which columns from data table 2 are joined to data table 1 and those columns can be optionally prefixed in the result.
}
\usage{
# example usage
do.or.load (data.table.A,data.table.B,c('first.key.A','second.key.A'),c('first.key.B','second.key.B'),c('cols','from','B','to','include'),'prefix.on.B.columns.in.result')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{...}{
%%     ~~Describe \code{dir} here~~
}
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

A <- data.table(a=1:10,b=LETTERS[1:10],key='a')
B <- data.table(c=1:20,d=letters[1:20],e=101:120,f=131:150,key='c')

join.on(A,B,key1='a',keys2='c',include.from.dt2=c('d','f'),included.prefix='from.b.') # note that we join


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
