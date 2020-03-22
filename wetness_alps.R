# trying to uses saga for the 10x10m pixel resolution for the swedish alpine region
#the data set is too large
#-> creating smal data sets (whole rande from east to west but only smal area north-south
#-> large overlapp to garantee that the estimation of the index is adequate
#-> just using the not overlapping area to produce a the map.
#-> using Hans G. shape for the definition of the alpine region 
#   which based on the "fastighetskartan" kalfjäll+gacier+fjällbjörkskog"



#requirements 
# SAGA must be insatlled 
# (alternatively can RQGIS be used, we have to try out that!!)


#install.packages("RSAGA")


library(raster)
library(sp)
library(sf)
library(rgdal)

library(RSAGA)
#myenv<-rsaga.env(path="C:/Program Files/saga/saga_2.2.3_x64")
#myenv<-rsaga.env("C:/Program Files (x86)/SAGA-GIS/saga_7.6.1_x64")

env <- rsaga.env()

#fjall_buffer<-readRDS("fjall_buffer_5km.rds")
#plot(fjall_buffer)
#st_write(fjall_buffer, "fjall_buffer.shp")
fjall_buffer<-readOGR("F:/Lovtrad_model","fjall_buffer")
e.buff<-extent(fjall_buffer)
dem10<-raster("M:/Geo-Data/Nnh_10m.tif")
e.dem10<-extent(dem10)
plot(e.dem10)

dem10.f<-crop(dem10,e.buff)

#plot(dem10.f)
#plot(fjall_buffer,add=T)

e.dem10.f<-extent(dem10.f)

diff<-e.dem10.f[4]-e.dem10.f[3]
diff1<-diff/20


j=1

e.smal<-extent(e.dem10.f[1],e.dem10.f[2],e.dem10.f[3]+(j-1)*diff1,e.dem10.f[3]+(j)*diff1)

fjall_buffer.s<-crop(fjall_buffer,e.smal)
e.buff.s<-extent(fjall_buffer.s)
dem.s<-crop(dem10.f,e.buff.s)

writeRaster(dem.s, filename="M:/Geo-Data/dem_alp_for_wetness.tif", format="GTiff", overwrite=TRUE)

rsaga.import.gdal(in.grid="M:/Geo-Data/dem_alp_for_wetness.tif",env=env)
name<-paste("M:/Geo-Data/wetness_new_e_test_",j,"_s1_a1.sgrd",sep="")
rsaga.wetness.index(in.dem="M:/Geo-Data/dem_alp_for_wetness.sgrd",out.wetness.index=name,env=env,area.type=0,slope.type=0,suction=100)
rsaga.sgrd.to.esri(name,env=myenv)













