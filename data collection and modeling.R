# modelling decidious tree in south Sweden


library(raster)
library(rgdal)
library(RStoolbox)
library(lidR)
library(dynatopmodel)
library(dplyr)
library(BalancedSampling)

rasterOptions(tmpdir="L:/DATA/temp_raster/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"
biogeo<-readRDS("M:/Sicherung neuer Rechner/R_rds/biogeografiska_regioner_sw99.rds")
boreal<-subset(biogeo,EU_REGION=="Boreal")
altitude<-raster("M:/THUF-Fjall/hojddata2.tif")





#data from GEE

library(googledrive)
drive_find()    #choose "2"
setwd("F:/Lovtrad_model/GEE")

t1<-drive_find()
files1<-grep(c("ndvi1"),t1$name,value=TRUE)
files2<-grep(c("ndvi2"),t1$name,value=TRUE)
files3<-grep(c("ndvi_diff"),t1$name,value=TRUE)
files4<-grep(c("tree_hight"),t1$name,value=TRUE)
files<-c(files1,files2,files3,files4)




for (i in 1:length(files))
  drive_download(file=files[i],overwrite=TRUE)


scene_list<-dir(path="F:/Lovtrad_model/GEE",pattern=".tif",full.names = TRUE)




parameter_list<-c(
 "ndvi1",
 "ndvi2",
 "ndvi_diff",
 "tree_hight"
)

for (k in 12:length(parameter_list))
{                  
  
  parameter<-parameter_list[k]
  #parameter="ndwi_m7_8"
  scene_list_red<-grep(parameter,scene_list,value = TRUE)
  file.copy(from=scene_list_red, to="F:/Geo-Data/temp_calc/",overwrite=TRUE)
  scene_list_red<-dir(path="F:/Geo-Data/temp_calc/",pattern=".tif",full.names = TRUE)
  
  k1<-raster(scene_list_red[1])   
  k2<-raster(scene_list_red[2])  
  
  test_m<-list(k1,k2)
  
  if (length(scene_list_red)>2)
  {
    for (i in (3):length(scene_list_red))
    {
      k3<-raster(scene_list_red[i])
      test_m<-append(test_m,k3)
    }
  }
  
  test_m$filename <-"F:/Geo-Data/try1.tif"
  test_m$overwrite <- TRUE
  mm <- do.call(merge, test_m)
  projection(mm)<-projSWEREF
  #mm_1<-crop(mm,extent(alp))
  #mm_1<-mask(mm_1,alp)
  file_name<-paste("F:/Härjedalen/Geo-Data/",parameter,".tif", sep="")
  #file_name<-paste("F:/THUF/Fjäll-habitat-modell/Geo-data/",parameter,"_alp.tif", sep="")
  writeRaster(mm, filename=file_name, format="GTiff", overwrite=TRUE)
  file.remove(scene_list_red)
}

