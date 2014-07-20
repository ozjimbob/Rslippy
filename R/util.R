## Utility Functions

# Converts latitude, longitude and zoome to a tile coordinate
deg2num<-function(lat_deg, lon_deg, zoom){
  lat_rad <- lat_deg * pi /180
  n <- 2.0 ^ zoom
  xtile <- floor((lon_deg + 180.0) / 360.0 * n)
  ytile = floor((1.0 - log(tan(lat_rad) + (1 / cos(lat_rad))) / pi) / 2.0 * n)
  return( c(xtile, ytile))
}

# Converts a tile coordinate and zoom to the corner latitude and longitude
num2deg<-function(x,y,z){
  n <- 2.0 ^ z
  lon_deg <- x / n * 360.0 - 180
  lat_rad <- atan(sinh(pi * (1-2*y/n)))
  lat_deg <- lat_rad * 180.0 / pi
  return(c(lat_deg,lon_deg))
}

# For a given bounding box and zoom range, generate a complete list of tiles
bounding_tiles=function(xmin,xmax,ymin,ymax,zm){
  t_list=data.frame(x=0,y=0,z=0)
  for(zms in zm[1]:zm[2]){
    top_left=deg2num(ymax,xmax,zms)
    bottom_right=deg2num(ymin,xmin,zms)
    all_x=top_left[1]:bottom_right[1]
    all_y=top_left[2]:bottom_right[2]
    all=expand.grid(all_x,all_y)
    all$z=zms
    names(all)=c("x","y","z")
    t_list=rbind(t_list,all)
  }
  t_list=subset(t_list,z>0)
  t_list
}

# For a given tile coordinate and zoom, return the lat/long bounding box
tile_square=function(x,y,z){
  c1=num2deg(x,y,z)
  c2=num2deg(x+1,y+1,z)
  return(c(min(c(c1[1],c2[1])),max(c(c1[1],c2[1])),min(c(c1[2],c2[2])),max(c(c1[2],c2[2]))))
}

