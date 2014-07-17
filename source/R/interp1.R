# interp1.R is a R function to do 1-D data interpolation
# Copyright (C) 2013  Chen-Pan Liao
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

