# list.all.files.R is a R function to get file names under a path
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


list.all.files <-
function(

	path = "."

) {
	
	allFile <- list.files(path)
	allFile.notDict <- file.info(allFile)$isdir == F
	return(allFile[allFile.notDict])
	
}
