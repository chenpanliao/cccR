setClass(
	"spectrum",
	representation(
		baseName = "character",
		dirName = "character",
		fullName = "character",
		rawString = "character",
		rowNotUsed = "numeric",
		outData = "matrix",
		interpIndex = "numeric",
		outDataInterped = "matrix"
	)
)

## constructors
setGeneric(
	"newSpectrum",
	function(filename, ...) {
		standardGeneric("newSpectrum")
	}
)
# for filename input
setMethod(
	"newSpectrum",
	signature(filename = "character"),
	function(filename, interp=c(300, 700, 0.1)) {
		
		if(length(filename) != 1L) {
			stop("the number of baseName must be 1.")
		}else if(!file.exists(filename)) {
			stop("the file does not exist.")
		}else if(file.info(filename)$isdir) {
			stop("the file must not be a folder.")
		}
		
		cat("Reading ", filename, "...", "\n", sep="")
		tmp <- read.spectrum.file(filename)
		obj <- new("spectrum")
		obj@baseName <- basename(filename)
		obj@dirName <- dirname(filename)
		obj@fullName <- paste0(getwd(), "/", obj@dirName, "/", obj@baseName)
		obj@rawString <- tmp$origin.file.content
		obj@outData <- tmp$outdata
		obj@rowNotUsed <- tmp$row.not.used

		cat("Analyzing ", filename, "...", "\n", sep="")
		obj@interpIndex <- c(interp[1], interp[2], interp[3])
		interpIndex <- seq(obj@interpIndex[1], obj@interpIndex[2], obj@interpIndex[3])
		obj@outDataInterped <- cbind(
			interpIndex,
			interp1(obj@outData[,1], obj@outData[,2], interpIndex)
		)
		dimnames(obj@outDataInterped) <- NULL

		return(obj)
	}
)


## method: show
setMethod(
	"show",
	signature(object = "spectrum"),
	function(object){
		cat("File:", object@fullName, "\n", sep="")
		cat(
			"Interped index: from ", object@interpIndex[1], 
			" to ", object@interpIndex[2], 
			" by ", object@interpIndex[3], "\n", sep=""
		)
		cat("Summary:\n")
		print(summary(object@outDataInterped[,2]))
	}
)


## method: plot
setMethod(
	"plot",
	"spectrum",
	definition = function(x, y, ...){
		matplot(
			x@outDataInterped[,1], x@outDataInterped[,2],
			type = "l", cex.axis = 0.8, xlab = "", ylab = ""
		)
	}
)

x <- newSpectrum("/Users/apan/Documents/workshop/octave/cc/cc-20101114/results/20140103T230337/core_data/diurnalIllumination.txt", c(300,700,0.1))
x
plot(x)




# for matrix input
#setMethod(
#	"newSpectrum",
#	signature("matrix"),
#	function(.) {
#		new("spectrum", outData = .)
#	}
#)



# method: initialize
#setMethod(
#	"initialize",
#	"spectrum",
#	function(.Object, baseName) {
#		.Object@baseName <- "aaaaaaaaaa"
#		return(.Object)
#	}
#)
