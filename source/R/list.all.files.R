list.all.files <-
function(

	path = "."

) {
	
	allFile <- list.files(path)
	allFile.notDict <- file.info(allFile)$isdir == F
	return(allFile[allFile.notDict])
	
}
