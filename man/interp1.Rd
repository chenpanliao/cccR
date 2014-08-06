\name{interp1}
\alias{interp1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
interp1(x, y, xout, xmin = min(x), xmax = max(x), ymin = min(x), ymax = max(x), method = "linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{xout}{
%%     ~~Describe \code{xout} here~~
}
  \item{xmin}{
%%     ~~Describe \code{xmin} here~~
}
  \item{xmax}{
%%     ~~Describe \code{xmax} here~~
}
  \item{ymin}{
%%     ~~Describe \code{ymin} here~~
}
  \item{ymax}{
%%     ~~Describe \code{ymax} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
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
function (x, y, xout, xmin = min(x), xmax = max(x), ymin = min(x), 
    ymax = max(x), method = "linear") 
{
    if (method == "linear") {
        yout <- approx(x, y, xout = xout)$y
    }
    if (any(method == c("fmm", "natural")) && length(method) == 
        1) {
        yout <- spline(x, y, xout = xout, method = method)$y
    }
    return(yout)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
