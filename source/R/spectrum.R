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
#	,
#	prototype(
#		interpIndex = c(300, 700, 0.2)
#	)
#	,
#	validity = function(object){
#		if(length(object@baseName) != 1L) {
#			stop("the number of baseName must be 1.")
#		}else if(!file.exists(object@baseName)) {
#			stop("the file baseName does not exist.")
#		}else if(file.info(object@baseName)$isdir) {
#			stop("the file baseName must not be a folder.")
#		}else{
#			return(T)
#		}
#	}
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
		
		tmp <- read.spectrum.file(filename)
		obj <- new("spectrum")
		obj@baseName <- basename(filename)
		obj@dirName <- dirname(filename)
		obj@fullName <- paste0(getwd(), "/", obj@dirName, "/", obj@baseName)
		obj@rawString <- tmp$origin.file.content
		obj@outData <- tmp$outdata
		obj@rowNotUsed <- tmp$row.not.used
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



####### newSpectrum("sdata.txt")





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



## method: getRawString
#setGeneric("getRawString", function(.) standardGeneric("getRawString"))
#setMethod(
#	"getRawString",
#	"spectrum",
#	function(object) {
#		object@baseName
#	}
#)



## method: plot
#setMethod(
#	"plot",
#	"spectrum",
#	function(object){
#		plot(object@outData[,1], object@outData[,2])
#	}
#)


## method: show
#setMethod(
#	"show",
#	"spectrum",
#	function(object){
#		cat("baseName:", object@baseName)
#	}
#)

  

#x <- new("spectrum", baseName = "asdfg.asdgas.sd")