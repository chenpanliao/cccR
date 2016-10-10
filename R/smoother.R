# smooth.length.R is a R function to smooth a line
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

smoother <- function (
	x, y, smooth.length = 0L
) {

	# NA omit
	temp <- na.omit(data.frame(x, y))
	x <- temp$x
	y <- temp$y

	# check length
	if (length(x) != length(y)) {
		stop("Length of x and y must be equal.")
	}
	if (length(x) < 2 | length(y) < 2) {
		stop("Length of x and y must larger than 1")
	}
	if (! is.integer(smooth.length)) {
		stop("smooth.length must be a ingerger (e.g., 3L)")
	}
	# sort
	x.index <- order(x)
	xx <- x[x.index]
	yy <- y[x.index]
	n <- length(xx)

	# checking uniform grid ###################
	xx.d <- xx[2:n] - xx[1:(n-1)]
	if (sd (xx.d) > 10^(-10)) {
		warning ("x might not be an arithmetic sequence.")
	}

	# smooth
	if (smooth.length > 0) {
		yy.ave <- numeric (n)
		yy.tmp <- c (rep (yy[1], smooth.length), yy, rep (yy[n], smooth.length) )
		for (i in 1:n) {
			yy.ave[i] <- mean (yy.tmp[i:(smooth.length * 2 + i)])
		}
	} else {
		yy.ave <- yy
	}
	return ( cbind (xx, yy.ave))

}
