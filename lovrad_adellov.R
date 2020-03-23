# modelling deciduous tree in constrast to real wood (triviallöv mod ädellöv)


library(raster)
library(rgdal)
library(RStoolbox)
library(dplyr)
library(sp)

rasterOptions(tmpdir="L:/DATA/temp_raster/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"
biogeo<-readRDS("M:/Sicherung neuer Rechner/R_rds/biogeografiska_regioner_sw99.rds")
boreal<-subset(biogeo,EU_REGION=="Boreal")
#altitude<-raster("M:/THUF-Fjall/hojddata2.tif")


#######################################################################################################################################



library(googledrive)
drive_find()    #choose "2"
setwd("L:/Lovtrad_model/GEE")

t1<-drive_find()
files1<-grep(c("adellov_juni"),t1$name,value=TRUE)



for (i in 1:length(files1))
  drive_download(file=files1[i],overwrite=TRUE)



filelist_adlov<-dir(path="L:/Lovtrad_model/GEE",pattern="adellov",full.names = TRUE)


#NFI data
taxdata<-readRDS("F:/Lovtrad_model/trainings_data.rds")                
taxdata<-taxdata %>% filter(DelytaNr==0) %>% filter(Taxar>2013)

taxdata.sp<-SpatialPointsDataFrame(coords=taxdata[,c("Ostkoordinat","Nordkoordinat")],data=taxdata,proj4string=CRS(projSWEREF))




#tif files have more than one layer -> multilayer -> the band needt to chose!

tile.extract.multilayer<-function(file_list=files1,data.sp=taxdata.sp,bandnr=1)
{
  train_m<-NA
  #ndvi1
  for (i in 1:length(file_list))
  {
    #i=1
    ras.t<-raster(file_list[i],band=bandnr)                   #.t for "tiles"
    e.t<-extent(ras.t)
    train.sp<-crop(data.sp,e.t)
    if(is.null(train.sp)==FALSE)
    {
      train<-train.sp@data
      train$extract<-extract(ras.t,train.sp)
      train_m<-rbind(train_m,train)
    }
  }
  train.res<-train_m %>% dplyr::select(Ostkoordinat,Nordkoordinat,extract)
  var.name=paste("band_",bandnr,sep="")
  names(train.res)[[3]]<-var.name
  print(train.res)
}


dat.band1<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=1)
dat.band2<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=2)
dat.band3<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=3)
dat.band4<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=4)
dat.band5<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=5)
dat.band6<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=6)
dat.band7<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=7)
dat.band8<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=8)
dat.band9<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=9)
dat.band10<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=10)
dat.band11<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=11)
dat.band12<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=12)
dat.band13<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=13)
dat.band14<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=14)
dat.band15<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=15)
dat.band16<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=16)





