interp1 <- function(
	x,
	y,
	xout,
	method = c("linear", "fmm", "natural")
){
	if (method[1] == "linear"){
		yout <- approx(x, y, xout = xout)$y
	}
	if (any(method[1] == c("fmm" , "natural")) && length(method) == 1){
		yout <- spline(x, y, xout = xout, method = method)$y
	}

	return(yout)
}
