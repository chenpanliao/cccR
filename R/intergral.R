# intergral.R is an R function to do intergral
# Copyright (C) 2013-2014  Chen-Pan Liao
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# any later version.
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

	# check class and mode
	if (class (temp) != "matrix" | mode (temp) != "numeric" )

	# check length
	if (nrow (temp) < 2) {
		stop ("Length of x and y must larger than 1")
	}

	# sort
 	temp <- temp[order(temp$x), ]

	# area
	area <- integrate (
		approxfun(temp$x, temp$y, rule = 2) ,
		min (temp$x),
		max (temp$x)
	)$value
	
	return (area)
	
}
