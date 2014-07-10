# read.spectrum.file.R is a R function to read spectrum file
# Copyright (C) 2013-2014 Chen-Pan Liao
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see {http://www.gnu.org/licenses/}.


read.spectrum.file <- function (
	fileToRead,
	encoding = "UTF-8",
	cols = 2,
	pattern.blank = "[[:blank:],;]",
	pattern.num = "([-]?[0-9]+[.]?[0-9]*[Ee]*[+-]*[0-9]{0,5})"
) {

	# input file as a string
	con <- file(fileToRead, "r", blocking = FALSE, encoding = encoding)
	readLines(con) -> strdata
	close(con)

	# create pattern according to pattern.blank and pattern.num
	pattern.row <- paste(
		sprintf("^%s*", pattern.blank),
		paste(
			rep(pattern.num, cols),
			sep="",
			collapse=sprintf("%s+", pattern.blank)
		),
		sprintf("%s*$", pattern.blank),
		sep="",
		collapse=""
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
		
	r <- list(
		outdata = outdata,
		filename = fileToRead,
		origin.file.content = strdata,
		row.not.used = which(is.number.row == F),
		encoding = encoding,
		grep.pattern = pattern.row,
		cols = cols,
		data.length = data.length
	)
	return(r)
}
