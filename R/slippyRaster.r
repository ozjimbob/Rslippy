## Generate Raster Riles


slippyRaster=function(rst,zoom=c(3,9),output,col=rev(terrain.colors(255)),browse=TRUE){
  ee=extent(rst)
  xmin=ee@xmin
  xmax=ee@xmax
  ymin=ee@ymin
  ymax=ee@ymax
  tile_list=bounding_tiles(xmin,xmax,ymin,ymax,zm)
  zs=unique(tile_list$z)
  
  if(!file.exists(output)){
    dir.create(output)
  }
  
  if(!file.exists(paste0(output,"/tiles"))){
    dir.create(paste0(output,"/tiles"))
  }
  
  for(idx in zm[1]:zm[2]){
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
    eo=tile_square(tile_list$x[idx],tile_list$y[idx],tile_list$z[idx])
    print(eo)
    png(paste0(output,"/tiles/",tile_list$z[idx],"/",tile_list$x[idx],"/",tile_list$y[idx],".png"),width=256,height=256,bg="#FFFFFF00")
    par(mai=c(0,0,0,0),mar=c(0,0,0,0),bty="n",xaxt="n",yaxt="n")
    image(rst,xlim=c(eo[3],eo[4]),ylim=c(eo[1],eo[2]),bg="#FFFFFF88",col=col)
    dev.off()
    
    
    h_tem=file("data/template.html")
    h_lines=readLines(h_tem)
    midpt_x=mean(xmin,xmax)
    midpt_y=mean(ymin,ymax)
    minzm=min(zm)
    c_line=paste0("      var mytile =L.tileLayer('file:tiles/{z}/{x}/{y}.png',{")
    h_lines[63]=c_line
    p_line=paste0("      var map = L.map('map').setView([",midpt_y,",",midpt_x,"], ",minzm,");")
    h_lines[57]=p_line
    o_file=paste0(output,"/index.html")
    o_file=file(o_file)
    writeLines(h_lines,o_file)
    close(o_file)
    close(h_tem)
  }
  if(browse==TRUE){
    browseURL(paste0(output,"/index.html"))
  }
  paste0(output,"/index.html")
}

