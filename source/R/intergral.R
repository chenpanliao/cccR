# intergral.R is a R function to do intergral
# Copyright (C) 2013  Chen-Pan Liao
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

intergral <-
function (
  x, y
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

  # sort
  x.index <- order(x)
  xx <- x[x.index]
  yy <- y[x.index]
  n <- length(xx)

  # checking uniform grid ###################
  xx.d <- xx[2] - xx[1]
  xx.cal <- numeric(n)
  for (k in 1:length(xx.cal)) {
    xx.cal[k] <- xx[1] + (k-1) * xx.d
  }
  if ( !all(xx == xx.cal) ) {
    is.uniform <- F
    message("x is not an arithmetic sequence.")
  } else {
    is.uniform <- T
  }
  
  # area
  if (is.uniform) {
    area <- ( (xx[n] - xx[1]) / 2 / (n-1) ) *
            ( sum(yy) + sum( yy[2:(n-1)] ) )
  } else {
    area.part <- numeric(n - 1)
    for (k in 1:(n - 1)) {
      area.part[k] <- (yy[k] + yy[k+1]) * (xx[k+1] - xx[k]) /2
    }
    area <- sum(area.part)
  }
  
  return (area)
  
}
