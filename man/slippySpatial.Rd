\name{slippySpatial}
\alias{slippySpatial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Spatial Layer Slippy Map Generation
}
\description{
Generates tiles and a tile-based slippy map based on the Leaflet Javascript library, from a Spatial class object. At the moment it's only tested on SpatialPolygons, especially for the plotting options
}
\usage{
slippySpatial(sp, zoom = c(3, 9), output, col = "white", border = "black", browse = TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{
A Spatial* class object.
}
  \item{zoom}{
A vector with the minimum and maximum zoom levels to be rendered.
}
  \item{output}{
Map output location.
}
  \item{col}{
Fill colour of polygons, can be a vector.
}
  \item{border}{
Border colour of polygons, can be a vector.
}
  \item{browse}{
Launch a browser to display the map when complete.
}
}
\details{
Currently renders a Spatial object to tiles, produces the HTML/Javascript code to display it.  No projection of raster into the correct coordinate system currently takes place, so geographic WGS84 rasters are required.  Note that there is a web Mercator error at small scales that I'm working on fixing.
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
slippyRaster
}
\examples{
tes=readShapePoly("clusters.shp")
slippySpatial(tes,zoom=c(2,4),output="alps3",col="blue",border="black",browse=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
