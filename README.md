Rslippy
=======

Slippy map tile generation for R

This package provides simple generation of tile-based slippy maps from spatial R objects, including Raster and Spatial* class objects.  It will output tiles for selected zoom levels, as well as the HTML to display the tiles as a map using the Leaflet Javscript mapping library.

The package is currently very basic - there is minimal checking of projections.  Options are also limited, but it works. 

To Do
-----
* Convert to S4 classes for different object types.
* More rendering/display options.
* Integration into shiny?
* Support for DoParallel library when rendering tiles.
