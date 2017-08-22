# intergral.R is a R function to do intergral
# Copyright (C) 2013-2014  Chen-Pan Liao
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


## 梯形法

intergral <-
  function (x, y) {
    # check length
    if (length (x) < 2 | length (y) < 2 | length(x) != length(y)) {
      stop ("Length of x and y must be equal and larger than 1.")
    }
    
    n <- length(y)
    y.up <- y[-1]
    y.low <- y[-n]
    x.up <- x[-1]
    x.low <- x[-n]
    delta <- x.up - x.low
    area <- sum((y.up + y.low) * delta / 2)
    return(area)
  }

# ## 缺點：切太多份會沒辦法算
# intergral <-
#   function (x, y) {
#     # NA omit
#     temp <- na.omit(data.frame(x, y))
#     
#     # check class and mode
#     if (class (temp) != "matrix" | mode (temp) != "numeric")
#       
#       # check length
#       if (nrow (temp) < 2) {
#         stop ("Length of x and y must be larger than 1")
#       }
#     
#     # sort
#     temp <- temp[order(temp$x),]
#     
#     # area
#     area <- integrate (approxfun(temp$x, temp$y, rule = 2) ,
#                        min (temp$x),
#                        max (temp$x))$value
#     
#     return (area)
#     
#   }
