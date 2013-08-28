\name{read.spectrum.file}
\alias{read.spectrum.file}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
read.spectrum.file(fileToRead, encoding = "UTF-8", cols = 2, pattern.blank = "[[:blank:],;]", pattern.num = "([-]?[0-9]+[.]?[0-9]*[Ee]*[+-]*[0-9]{0,5})")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fileToRead}{
%%     ~~Describe \code{fileToRead} here~~
}
  \item{encoding}{
%%     ~~Describe \code{encoding} here~~
}
  \item{cols}{
%%     ~~Describe \code{cols} here~~
}
  \item{pattern.blank}{
%%     ~~Describe \code{pattern.blank} here~~
}
  \item{pattern.num}{
%%     ~~Describe \code{pattern.num} here~~
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
function (fileToRead, encoding = "UTF-8", cols = 2, pattern.blank = "[[:blank:],;]", 
    pattern.num = "([-]?[0-9]+[.]?[0-9]*[Ee]*[+-]*[0-9]{0,5})") 
{
    con <- file(fileToRead, "r", blocking = FALSE, encoding = encoding)
    strdata <- readLines(con)
    close(con)
    pattern.row <- paste(sprintf("^\%s*", pattern.blank), paste(rep(pattern.num, 
        cols), sep = "", collapse = sprintf("\%s+", pattern.blank)), 
        sprintf("\%s*$", pattern.blank), sep = "", collapse = "")
    is.number.row <- regexpr(pattern.row, strdata) == T
    strdata.onlydata <- strdata[is.number.row]
    data.length <- length(strdata.onlydata)
    outdata <- matrix(nrow = data.length, ncol = cols)
    for (j in 1:cols) {
        outdata[, j] <- as.numeric(gsub(pattern.row, sprintf("\\\%1.0f", 
            j), strdata.onlydata, ))
    }
    return(list(outdata = outdata, filename = fileToRead, origin.file.content = strdata, 
        row.not.used = which(is.number.row == F), encoding = encoding, 
        grep.pattern = pattern.row, cols = cols, data.length = data.length))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
