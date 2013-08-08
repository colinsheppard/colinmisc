\name{stackpoly.posneg}
\alias{stackpoly.posneg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stack Polygons Positive and Negative 
%%  ~~function to do ... ~~
}
\description{
Stack Polygons Positive and Negative 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
stackpoly.posneg(x, y = NULL, main = "", xlab = "", ylab = "", xat = NA, xaxlab = NA, xlim = NA, ylim = NA, lty = 1, lwd = 1, border = NA, col = NULL, staxx = FALSE, stack = FALSE, axis2 = TRUE, axis4 = TRUE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{main}{
%%     ~~Describe \code{main} here~~
}
  \item{xlab}{
%%     ~~Describe \code{xlab} here~~
}
  \item{ylab}{
%%     ~~Describe \code{ylab} here~~
}
  \item{xat}{
%%     ~~Describe \code{xat} here~~
}
  \item{xaxlab}{
%%     ~~Describe \code{xaxlab} here~~
}
  \item{xlim}{
%%     ~~Describe \code{xlim} here~~
}
  \item{ylim}{
%%     ~~Describe \code{ylim} here~~
}
  \item{lty}{
%%     ~~Describe \code{lty} here~~
}
  \item{lwd}{
%%     ~~Describe \code{lwd} here~~
}
  \item{border}{
%%     ~~Describe \code{border} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{staxx}{
%%     ~~Describe \code{staxx} here~~
}
  \item{stack}{
%%     ~~Describe \code{stack} here~~
}
  \item{axis2}{
%%     ~~Describe \code{axis2} here~~
}
  \item{axis4}{
%%     ~~Describe \code{axis4} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (x, y = NULL, main = "", xlab = "", ylab = "", xat = NA, 
    xaxlab = NA, xlim = NA, ylim = NA, lty = 1, lwd = 1, border = NA, 
    col = NULL, staxx = FALSE, stack = FALSE, axis2 = TRUE, axis4 = TRUE, 
    ...) 
{
    ydim <- dim(y)
    if (is.null(y[1])) {
        y <- x
        ydim <- dim(y)
        if (is.null(ydim)) {
            x <- 1:length(y)
        }
        else {
            x <- matrix(rep(1:ydim[1], ydim[2]), ncol = ydim[2])
        }
    }
    if (stack) 
        y.neg <- t(apply(as.matrix(y), 1, function(r) {
            r[r > 0] <- 0
            cumsum(r)
        }))
    y.pos <- t(apply(as.matrix(y), 1, function(r) {
        r[r < 0] <- 0
        cumsum(r)
    }))
    if (is.na(xlim[1])) 
        xlim <- range(x)
    if (is.na(ylim[1])) 
        ylim <- range(c(y.pos, y.neg))
    plot(0, main = main, xlab = xlab, ylab = ylab, xlim = xlim, 
        ylim = ylim, type = "n", xaxs = "i", yaxs = "i", axes = FALSE, 
        ...)
    box()
    if (is.matrix(y.pos) || is.list(y.pos)) {
        plotlim <- par("usr")
        if (is.na(xat[1])) 
            xat <- x[, 1]
        if (is.na(xaxlab[1])) 
            xaxlab <- xat
        if (staxx) 
            staxlab(at = xat, labels = xaxlab)
        else axis(1, at = xat, labels = xaxlab)
        if (axis2) 
            axis(2)
        if (axis4) 
            axis(4)
        if (is.null(col[1])) 
            col = rainbow(ydim[2])
        else if (length(col) < ydim[2]) 
            col <- rep(col, length.out = ydim[2])
        if (length(border) < ydim[2]) 
            border <- rep(border, length.out = ydim[2])
        if (length(lty) < ydim[2]) 
            lty <- rep(lty, length.out = ydim[2])
        if (length(lwd) < ydim[2]) 
            lwd <- rep(lwd, length.out = ydim[2])
        for (pline in seq(ydim[2], 1, by = -1)) {
            if (pline == 1) {
                polygon(c(x[1], x[, pline], x[ydim[1]]), c(0, 
                  y.pos[, pline], 0), border = border[pline], 
                  col = col[pline], lty = lty[pline], lwd = lwd[pline])
            }
            else polygon(c(x[, pline], rev(x[, pline - 1])), 
                c(y.pos[, pline], rev(y.pos[, pline - 1])), border = border[pline], 
                col = col[pline], lty = lty[pline], lwd = lwd[pline])
        }
        for (pline in seq(ydim[2], 1, by = -1)) {
            if (pline == 1) {
                polygon(c(x[1], x[, pline], x[ydim[1]]), c(0, 
                  y.neg[, pline], 0), border = border[pline], 
                  col = col[pline], lty = lty[pline], lwd = lwd[pline])
            }
            else polygon(c(x[, pline], rev(x[, pline - 1])), 
                c(y.neg[, pline], rev(y.neg[, pline - 1])), border = border[pline], 
                col = col[pline], lty = lty[pline], lwd = lwd[pline])
        }
    }
    else {
        polygon(c(min(x), x, max(x), 0), c(0, y, 0, 0), border = border, 
            col = col, lty = lty, lwd = lwd)
        if (is.na(xat[1])) 
            xat <- x
        if (is.na(xaxlab[1])) 
            xaxlab <- xat
        if (staxx) 
            staxlab(at = xat, labels = xaxlab)
        else axis(1, at = xat, labels = xaxlab)
        if (axis2) 
            axis(2)
        if (axis4) 
            axis(4)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
