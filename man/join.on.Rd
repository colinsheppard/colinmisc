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
join.on(dt1,dt2,keys1,keys2=NULL,include.from.dt2=NULL,included.prefix='')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dt1}{The first data table in the join, all columns and rows from dt1 will be present in the result with columns from dt2 added where keys match.}
  \item{dt2}{The second data table in the join, dt2 should be seens as contributing data to the rows of dt1.}
  \item{keys1}{The columns form dt1 to use as keys in the join.}
  \item{keys2}{The columns form dt2 to use as keys in the join. Leave as NULL if the keys from dt2 are identical to keys1.}
  \item{include.from.dt2}{The columns from dt2 to add to dt1. Leave as NULL to include all columns.}
  \item{included.prefix}{Optional prefix to append to the columns from dt2 that are added to dt1.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
A data table is the result of joining specified columns from dt2 to dt1. 
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
A <- data.table(a=1:10,b=LETTERS[1:10],key='a')
B <- data.table(c=1:20,d=letters[1:20],e=101:120,f=131:150,key='c')

join.on(A,B,key1='a',keys2='c',include.from.dt2=c('d','f'),included.prefix='from.b.') # note that we join

