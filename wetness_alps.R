# trying to uses saga for the 10x10m pixel resolution for the swedish alpine region
#the data set is too large
#-> creating smal data sets (whole range from east to west but only smal area north-south
#-> large overlapp to garantee that the estimation of the index is adequate
#-> just using the not overlapping area to produce  the resulting map.
#-> using Hans G. shape for the definition of the alpine region 
#   which based on the "fastighetskartan" kalfjäll+gacier+fjällbjörkskog"




#requirements 
# SAGA must be insatlled 
# (alternatively can RQGIS be used, we have to try out that!! phython version has to e the right one, takes time if
# you need to have different version for different applications)


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
dem10<-raster("M:/Geo-Data/Nnh_10m.tif") #it is the same file that lies under \\YKSI\6_Analys\beräkningsunderlag_GIS_filer\Nnh_10m

e.dem10<-extent(dem10)
plot(e.dem10)

dem10.f<-crop(dem10,e.buff)

#plot(dem10.f)
#plot(fjall_buffer,add=T)

e.dem10.f<-extent(dem10.f)

diff<-e.dem10.f[4]-e.dem10.f[3]
diff1<-diff/120                      # 120 because otherwise the files will be too large




for (j in 1:120)
{
e.smal<-extent(e.dem10.f[1],e.dem10.f[2],(e.dem10.f[3]+(j-1)*diff1)-3000,(e.dem10.f[3]+(j)*diff1)+3000)

fjall_buffer.s<-crop(fjall_buffer,e.smal)
e.buff.s<-extent(fjall_buffer.s)
dem.s<-crop(dem10.f,e.buff.s)

writeRaster(dem.s, filename="M:/Geo-Data/dem_alp_for_wetness.tif", format="GTiff", overwrite=TRUE)

rsaga.import.gdal(in.grid="M:/Geo-Data/dem_alp_for_wetness.tif",env=env)
name<-paste("M:/Geo-Data/wetness_new_e_test_",j,"_s1_a1.sgrd",sep="")
rsaga.wetness.index(in.dem="M:/Geo-Data/dem_alp_for_wetness.sgrd",out.wetness.index=name,env=env,area.type=0,slope.type=0,suction=100)
rsaga.sgrd.to.esri(name,env=env)
}


#merge files without the buffer 
#1:read files->crop files->save files
#2:->merge all

#diff1<- 7548.667


for (j in c(1:71))
{
name<-paste("M:/Geo-Data/wetness_new_e_test_",j,"_s1_a1.asc",sep="")
wet_area1<-raster(name)
e.smal<-extent(e.dem10.f[1],e.dem10.f[2],(e.dem10.f[3]+(j-1)*diff1),(e.dem10.f[3]+(j)*diff1))
wet_area1.1<-crop(wet_area1,e.smal)
f_name<-paste("M:/Geo-Data/wetness_fjall/wetness_part_",j,".tif",sep="")
writeRaster(wet_area1.1, filename=f_name, format="GTiff", overwrite=TRUE)
}






merge.raster(filesource="M:/Geo-Data/wetness_fjall/"
                       ,file_collection="wetness_part"
                       ,temp_file="L:/DATA/temp_raster/temp.tif"
                       ,target_file="M:/Geo-Data/wetness_fjall_part2.tif",
                       proj="+init=epsg:3006")
  
  
  












