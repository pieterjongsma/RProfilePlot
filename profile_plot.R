#
# Transparent profile plots
#
# Copyright 2014 Pieter Jongsma
# But please, use it!
#

# plot.polygons
# x: vector
# ys: list of vectors, each list will be plotted separately
plot.polygons <- function(x, ys) {
  # Calculating the min and max value for y is somewhat complicated because ys is a list. The expressions below simply get the minimum values in y for all the vectors in the list
  y.min <- min(sapply(ys, min))
  y.max <- max(sapply(ys, max))
  
  plot(0, 0, xlim=c(min(x), max(x)), ylim=c(y.min, y.max), type='n',
       xlab="", ylab="")
  # The fill color transparency should really be some function of the number of polygons being drawn (=length(ys))...
  fill <- rgb(1, 0, 0, 0.15)
  
  for (y in ys) {
    # Add two extra data points to create a nicely filled plot
    x.adj <- c(min(x), x, max(x))
    y.adj <- c(0, y, 0)
    
    polygon(x.adj, y.adj, col=fill, border=rgb(0, 0, 0, 0))
  }
}


# This is merely an example of how to call the plot.polygons function
# data.csv:
# - 1st row: headers
# - 2nd row: x values
# - other rows: y values 
d <- read.csv('./data.csv')
x <- d[1, ]
ys <- d[-1, ]
# plot.polygons requires a list, so convert the ys data frame
ys <- split(ys, rownames(ys))
# DRAW!
plot.polygons(x, ys) 
