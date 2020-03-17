#merge raster tiles to one large raster
#!set raster option!!!!
rasterOptions(tmpdir="L:/DATA/temp_raster/")



merge.raster<-function(filesource="L:/Lovtrad_model"
                       ,file_collection="pa_"
                       ,temp_file="L:/DATA/temp_raster/temp.tif"
                       ,target_file="L:/Lovtrad_model/lovtrad_sverige.tif",
                       proj="+init=epsg:3006")
{
  #1.read in all raster files that should be merged
  scene_list<-dir(path=filesource,pattern=".tif",full.names = TRUE)
  files.tm<-grep(file_collection,scene_list,value=TRUE)
  
  k1<-raster(files.tm[1])
  k2<-raster(files.tm[2])
  test_m<-list(k1,k2)
  
  for (i in c(3:length(files.tm)))
  {
    k3<-raster(files.tm[i])
    test_m<-append(test_m,k3)
  }
  
  test_m$filename <-temp_file
  test_m$overwrite <- TRUE
  mm <- do.call(merge, test_m)
  proj4string(mm)<-proj
  writeRaster(mm, filename=target_file, format="GTiff", overwrite=TRUE) 
  
  
  
  
  
}