interp1 <- function(

	x,
	y,
	xout,
	xmin = min(x),
	xmax = max(x),
	ymin = min(x),
	ymax = max(x),	
	method = "linear"

){
	
	if (method == "linear") {
		yout <- approx(x, y, xout = xout)$y
	}
	
	if (
		any(method == c("fmm" , "natural"))  && length(method) == 1
	) {
		yout <- spline(x, y, xout = xout, method = method)$y
	}

	return(yout)
	
}