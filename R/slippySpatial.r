## Generate Spatial Tiles


slippySpatial=function(sp,zoom=c(3,9),output,col="white",border="black",browse=TRUE){
  if(is.na(proj4string(sp))){
    proj4string(sp)=crs.geo
  }
  sp=spTransform(sp,CRS(proj_fos))
  
  null_rast=raster(nrows=180,ncols=360,xmn=-180,xmx=180,ymn=-90,ymx=90)
  values(null_rast)=NA
  null_rast=projectRaster(from=null_rast,crs=CRS(proj_fos))
  ee=extent(sp)
  
  xmin=ee@xmin
  xmax=ee@xmax
  ymin=ee@ymin
  ymax=ee@ymax
  tile_list=bounding_tiles(xmin,xmax,ymin,ymax,zoom)
  zs=unique(tile_list$z)
  
  if(!file.exists(output)){
    dir.create(output)
  }
  
  if(!file.exists(paste0(output,"/tiles"))){
    dir.create(paste0(output,"/tiles"))
  }
  
  for(idx in zoom[1]:zoom[2]){
    this_root=paste0(output,"/tiles/",idx)
    if(!file.exists(this_root)){
      dir.create(this_root)
    }
    ux=unique(subset(tile_list,tile_list$z==idx)$x)
    for(udx in ux){
      this_node=paste0(this_root,"/",udx)
      if(!file.exists(this_node)){
        dir.create(this_node)
      }
    }
  }
  
  
  for(idx in seq_along(tile_list$x)){
    tlen=length(tile_list$x)
    pb=txtProgressBar(min=1,max=tlen,initial=1,style=3)
    setTxtProgressBar(pb,idx)
    eo=tile_square(tile_list$x[idx],tile_list$y[idx],tile_list$z[idx])
    png(paste0(output,"/tiles/",tile_list$z[idx],"/",tile_list$x[idx],"/",tile_list$y[idx],".png"),width=256,height=256,bg="#FFFFFF00")
    par(mai=c(0,0,0,0),mar=c(0,0,0,0),bty="n",xaxt="n",yaxt="n")
    cre=extent(c(eo[3],eo[4],eo[1],eo[2]))
    c_null_rast=crop(null_rast,cre)
    image(c_null_rast,xlim=c(eo[3],eo[4]),ylim=c(eo[1],eo[2]),bg="#FFFFFF88")
    plot(sp,add=TRUE,xlim=c(eo[3],eo[4]),ylim=c(eo[1],eo[2]),bg="#FFFFFF00",col=col,border=border)
    dev.off()
  }    
    h_tem=file("data/template.html")
    h_lines=readLines(h_tem)
    midpt_x=mean(xmin,xmax)
    midpt_y=mean(ymin,ymax)
    minzm=min(zoom)
    maxzm=max(zoom)
    c_line=paste0("      var mytile =L.tileLayer('file:tiles/{z}/{x}/{y}.png',{")
    h_lines[63]=c_line
    m_line=paste0("        maxZoom: ",maxzm,",")
    h_lines[64]=m_line
    p_line=paste0("      var map = L.map('map').setView([",midpt_y,",",midpt_x,"], ",minzm,");")
    h_lines[57]=p_line
    o_file=paste0(output,"/index.html")
    o_file=file(o_file)
    writeLines(h_lines,o_file)
    close(o_file)
    close(h_tem)
  
  if(browse==TRUE){
    browseURL(paste0(output,"/index.html"))
  }
  paste0(output,"/index.html")
}
