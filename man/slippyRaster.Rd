\name{slippyRaster}
\alias{slippyRaster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Raster Layer Slippy Map Generation
}
\description{
Generates tiles and a tile-based slippy map based on the Leaflet Javascript library, from a Raster class object.
}
\usage{
slippyRaster(rst, zoom = c(3, 9), output, col = rev(terrain.colors(255)), browse = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{rst}{
The Raster dataset - should be in WGS84 projection.
}
  \item{zoom}{
A vector containing the minimum and maximum zoom levels (1-16).
}
  \item{output}{
Location for tile output.
}
  \item{col}{
Colour palette definition for object. By default, the same as for standard plot.raster output.
}
  \item{browse}{
Launch a browser to display the map when complete.
}
}
\details{
Currently renders a raster to tiles, produces the HTML/Javascript code to display it.  No projection of raster into the correct coordinate system currently takes place, so geographic WGS84 rasters are required.
}
\value{
Launches a browser window displaying the map, and returns the location of the index.html file.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
Grant Williamson
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
slippySpatial
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
library(raster)
rst=raster("ann_npp.tif")
output="tilemap"
render_tiles(rst,c(3,9),output,browse=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
