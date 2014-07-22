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
  tl1=project(cbind(xmax,ymax),proj_fos,inv=TRUE)
  br1=project(cbind(xmin,ymin),proj_fos,inv=TRUE)
  xmax=tl1[1]
  ymax=tl1[2]
  xmin=br1[1]
  ymin=br1[2]
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
  t_list=subset(t_list,z>0 & x>0 & y > 0)
  t_list
}

# For a given tile coordinate and zoom, return the lat/long bounding box
tile_square=function(x,y,z){
  c1=num2deg(x,y,z)
  c2=num2deg(x+1,y+1,z)
  xmin=min(c(c1[2],c2[2]))
  xmax=max(c(c1[2],c2[2]))
  ymin=min(c(c1[1],c2[1]))
  ymax=max(c(c1[1],c2[1]))
  
  tl=data.frame(x=xmax,y=ymax)
  coordinates(tl)=c("x","y")
  proj4string(tl) = crs.geo
  tl_p=spTransform(tl,CRS(proj_fos))
  
  br=data.frame(x=xmin,y=ymin)
  coordinates(br)=c("x","y")
  proj4string(br) = crs.geo
  br_p=spTransform(br,CRS(proj_fos))
  
  xmax=coordinates(tl_p)[1]
  ymax=coordinates(tl_p)[2]
  xmin=coordinates(br_p)[1]
  ymin=coordinates(br_p)[2]
  
  
  return(c(ymin,ymax,xmin,xmax))
}

proj_os = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
proj_wm = "+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=latlong +no_defs"
proj_fos = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"
crs.geo <- "+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0" 
