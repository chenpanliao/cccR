\name{intergral}
\alias{intergral}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
intergral(x, y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
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
function (x, y) 
{
    temp <- na.omit(data.frame(x, y))
    x <- temp$x
    y <- temp$y
    if (length(x) != length(y)) {
        stop("Length of x and y must be equal.")
    }
    if (length(x) < 2 | length(y) < 2) {
        stop("Length of x and y must larger than 1")
    }
    x.index <- order(x)
    xx <- x[x.index]
    yy <- y[x.index]
    n <- length(xx)
    xx.d <- xx[2] - xx[1]
    xx.cal <- numeric(n)
    for (k in 1:length(xx.cal)) {
        xx.cal[k] <- xx[1] + (k - 1) * xx.d
    }
    if (!all(xx == xx.cal)) {
        is.uniform <- F
        warning("x is not an arithmetic sequence.")
    }
    else {
        is.uniform <- T
    }
    if (is.uniform) {
        area <- ((xx[n] - xx[1])/2/(n - 1)) * (sum(yy) + sum(yy[2:(n - 
            1)]))
    }
    else {
        area.part <- numeric(n - 1)
        for (k in 1:(n - 1)) {
            area.part[k] <- (yy[k] + yy[k + 1]) * (xx[k + 1] - 
                xx[k])/2
        }
        area <- sum(area.part)
    }
    return(area)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
