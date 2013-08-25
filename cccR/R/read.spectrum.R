read.spectrum <-
function (

	fileToRead,
	encoding = "UTF-8",
	cols = 2,
	pattern.blank = "[[:blank:],;]",
	pattern.num = "([-]?[0-9]+[.]?[0-9]*[Ee]*[+-]*[0-9]{0,5})"

) {

	# input
	con <- file(fileToRead, "r", blocking = FALSE, encoding = encoding)
	readLines(con) -> strdata
	close(con)

	# remove non-data row m
	pattern.row <- paste(
		sprintf("^%s*", pattern.blank),
		paste(
			rep(pattern.num, cols) ,
			sep="", 
			collapse=sprintf("%s+", pattern.blank)
		),
		sprintf("%s*$", pattern.blank),
		sep="", collapse=""
	)
	is.number.row <- regexpr(pattern.row, strdata) == T
	strdata.onlydata <- strdata[is.number.row]
	
	# fetch data by gsub()
	data.length <- length(strdata.onlydata)
	outdata <- matrix(nrow = data.length, ncol = cols)
	for (j in 1:cols) {
		outdata[,j] <- as.numeric(
			gsub(
				pattern.row,
				sprintf("\\%1.0f", j),
				strdata.onlydata,
			)
		)
	}
		
	return(list(
		outdata = outdata,
		filename = fileToRead,
		origin.file.content = strdata,
		row.not.used = which(is.number.row == F),
		encoding = encoding,
		grep.pattern = pattern.row,
		cols = cols,
		data.length = data.length
	))

	
	

}
