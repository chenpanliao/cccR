# spectra.R is an R function to read spectrum file
# Copyright (C) 2013-2014 Chen-Pan Liao
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see {http://www.gnu.org/licenses/}.



## interpolation
interp1 <- function(
	x,
	y,
	xout,
	method = c("linear", "fmm", "natural"),
	smoother = 0L
){
	if (method[1] == "linear"){
		yout <- approx (x, y, xout)$y
	} else if (any(method[1] == c ("fmm" , "natural")) && length (method) == 1){
		yout <- spline(x, y, xout, method = method)$y
	} else {
    		warning("method must be \"linear\", \"fmm\" or \"natural\"")
	}
	return (cbind (xout, yout))
}



## spectra file phraser
read.spectra.file <- function (
	fileToRead,
	encoding = "UTF-8",
	cols = 2,
	pattern.blank = "[[:blank:],;]",
	pattern.num = "([-]?[0-9]+[.]?[0-9]*[Ee]*[+-]*[0-9]{0,5})"
) {

	# input file as a string
	con <- file(fileToRead, "r", blocking = F, encoding = encoding)
	readLines(con) -> strdata
	close(con)

	# create pattern according to pattern.blank and pattern.num
	pattern.row <- paste(
		sprintf("^%s*", pattern.blank),
		paste(
			rep(pattern.num, cols),
			sep = "",
			collapse=sprintf("%s+", pattern.blank)
		),
		sprintf("%s*$", pattern.blank),
		sep = "",
		collapse = ""
	)

	# remove non-data row
	is.number.row <- regexpr(pattern.row, strdata) == T
	strdata.onlydata <- strdata[is.number.row]

	# fetch data
	data.length <- length(strdata.onlydata)
	outdata <- matrix(nrow = data.length, ncol = cols)
	for (j in 1:cols) {
		outdata[,j] <- as.numeric(
			gsub(
				pattern.row,
				sprintf("\\%1.0f", j),
				strdata.onlydata
			)
		)
	}

	list(
		outdata = outdata,
		filename = fileToRead,
		origin.file.content = strdata,
		row.not.used = which(is.number.row == F),
		encoding = encoding,
		grep.pattern = pattern.row,
		cols = cols,
		data.length = data.length
	)
}


## main method
read.spectra <- function(
  filename,
	encoding = "UTF-8",
	cols = 2,
	pattern.blank = "[[:blank:],;]",
	pattern.num = "([-]?[0-9]+[.]?[0-9]*[Ee]*[+-]*[0-9]{0,5})",
  interp = c(300, 700, 0.1),
	interp.method = c("linear", "fmm", "natural"),
  smooth.length = 0L
) {

	if(length(filename) != 1L) {
		stop("the number of filename must be 1.")
	}else if(!file.exists(filename)) {
		stop("the file does not exist.")
	}else if(file.info(filename)$isdir) {
		stop("the file must be a plain text file rather than a folder.")
	}

	cat("Reading ", filename, "...", "\n", sep = "")
	obj <- list()

	## 檔案名稱
	obj$baseName <- basename(filename)
	obj$dirName <- dirname(filename)
	obj$fullName <- paste0(getwd(), "/", obj$dirName, "/", obj$baseName)

	## 檔案內容
	tmp <- read.spectra.file(filename)
	obj$rawString <- tmp$origin.file.content
	obj$outData <- tmp$outdata
	obj$rowNotUsed <- tmp$row.not.used

	## 內插法
	cat("Analyzing ", filename, "...", "\n", sep = "")
	obj$interpIndex <- interp
	interpIndex <- seq(obj$interpIndex[1], obj$interpIndex[2], obj$interpIndex[3])
	outDataInterp <- interp1(obj$outData[,1], obj$outData[,2], interpIndex, method = interp.method)
	dimnames(outDataInterp) <- NULL

  ## smoother
  smooth.mat <- smoother(outDataInterp[, 1], outDataInterp[, 2], smooth.length)
  obj$spectra <- smooth.mat[, 2]
	obj$wavelength <- outDataInterp[, 1]
  obj$smooth.length <- smooth.length

  class(obj) <- "spectra"
	return(obj)
}

## method: print
print.spectra <- function(object){
	cat("File:", object$fullName, "\n", sep = "")
	cat(
		"Interped index: from ", object$interpIndex[1],
		" to ", object$interpIndex[2],
		" by ", object$interpIndex[3], "\n", sep = ""
	)
  cat("Smooth length:", object$smooth.length, "\n", sep = "")
	cat("Summary:\n")
	print(summary(object$spectra))
}

## method: plot
plot.spectra <- function(x, y, ...){
	par(mar = c(3,3,0,0) + 0.1, cex = 10/12)
	matplot(
		x$wavelength, x$spectra,
		type = "l", xlab = "", ylab = ""
	)
}



# obj <- read.spectra("../test-data/LM-black-01-1.txt", smooth.length = 10L)
# plot(obj)
# obj



