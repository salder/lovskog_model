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

rasterOptions(tmpdir="C:/TEMP")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"

#myenv<-rsaga.env(path="C:/Program Files/saga/saga_2.2.3_x64")
#myenv<-rsaga.env("C:/Program Files (x86)/SAGA-GIS/saga_7.6.1_x64")

env <- rsaga.env()

#fjall_buffer<-readRDS("fjall_buffer_5km.rds")
#plot(fjall_buffer)
#st_write(fjall_buffer, "fjall_buffer.shp")

dem10<-raster("C:/wetness/Nnh_10m.tif") #it is the same file that lies under \\YKSI\6_Analys\beräkningsunderlag_GIS_filer\Nnh_10m

sweden.sh<-readOGR("C:/wetness","an_riks")


e.dem10<-extent(dem10)
plot(e.dem10)




diff<-e.dem10[4]-e.dem10[3]
diff1<-diff/350                      # 300 because otherwise the files will be too large




for (j in 1:350)
{
  e.smal<-extent(e.dem10[1],e.dem10[2],(e.dem10[3]+(j-1)*diff1)-4000,(e.dem10[3]+(j)*diff1)+4000)
  #e.smal<-e1
  #fjall_buffer.s<-crop(fjall_buffer,e.smal)
  #e.buff.s<-extent(fjall_buffer.s)
  #e.smal<-extent(c(285578,614517,6423000,6430000))
  dem.s<-crop(dem10,e.smal)
  e.cr<-extent(dem.s)
  e.swe.cr<-crop(sweden.sh,e.cr)
  dem.s<-crop(dem.s,e.swe.cr)
  
  e.cr<-extent(dem.s)
  
  diff_e_w<-e.cr[2]-e.cr[1]
  overlap1<-e.cr[1]+(diff_e_w/2)+(diff_e_w/2)*0.05
  overlap2<-e.cr[1]+(diff_e_w/2)-(diff_e_w/2)*0.05
  e.cr1<-extent(c(e.cr[1],overlap1,e.cr[3],e.cr[4]))
  e.cr2<-extent(c(overlap2,e.cr[2],e.cr[3],e.cr[4]))
  
  #west part
        dem.s1<-crop(dem.s,e.cr1)
        writeRaster(dem.s1, filename="C:/wetness/dem_alp_for_wetness.tif", format="GTiff", overwrite=TRUE)
        
        rsaga.import.gdal(in.grid="C:/wetness/dem_alp_for_wetness.tif",env=env)
        name.w<-paste("C:/wetness/wetness_new_e_test_","west_s1_a1.sgrd",sep="")
        rsaga.wetness.index(in.dem="C:/wetness/dem_alp_for_wetness.sgrd",out.wetness.index=name.w,env=env,area.type=0,slope.type=0,suction=100)
        rsaga.sgrd.to.esri(name.w,env=env)

  #east part
        dem.s1<-crop(dem.s,e.cr2)
        writeRaster(dem.s1, filename="C:/wetness/dem_alp_for_wetness.tif", format="GTiff", overwrite=TRUE)
        
        rsaga.import.gdal(in.grid="C:/wetness/dem_alp_for_wetness.tif",env=env)
        name.e<-paste("C:/wetness/wetness_new_e_test_","east_s1_a1.sgrd",sep="")
        rsaga.wetness.index(in.dem="C:/wetness/dem_alp_for_wetness.sgrd",out.wetness.index=name.e,env=env,area.type=0,slope.type=0,suction=100)
        rsaga.sgrd.to.esri(name.e,env=env)
  
  
        
        
        wet_west<-raster("C:/wetness/wetness_new_e_test_west_s1_a1.asc") 
        wet_east<-raster("C:/wetness/wetness_new_e_test_east_s1_a1.asc") 
        
        e.west<-extent(c(e.cr[1],e.cr[1]+(diff_e_w/2),e.cr[3]+4000,e.cr[4]-4000))
        e.east<-extent(c(e.cr[1]+(diff_e_w/2),e.cr[2],e.cr[3]+4000,e.cr[4]-4000))
      
        #e.west<-extent(c(e.cr[1],e.cr[1]+(diff_e_w/2),e.cr[3]+2000,e.cr[4]-2000))
        #e.east<-extent(c(e.cr[1]+(diff_e_w/2),e.cr[2],e.cr[3]+2000,e.cr[4]-2000))
          
        wet_west<-crop(wet_west,e.west)
        wet_east<-crop(wet_east,e.east)
        wet<-merge(wet_west,wet_east)
        
        f_name<-paste("C:/wetness/wetness_swe/wetness_part_",j,".tif",sep="")
        writeRaster(wet, filename=f_name, format="GTiff", overwrite=TRUE)
        }


#merge files without the buffer 
#1:read files->crop files->save files
#2:->merge all

#diff1<- 7548.667

# 
# for (j in c(1:120))
# {
#   name<-paste("M:/Geo-Data/wetness_new_e_test_",j,"_s1_a1.asc",sep="")
#   wet_area1<-raster(name)
#   e.smal<-extent(e.dem10.f[1],e.dem10.f[2],(e.dem10.f[3]+(j-1)*diff1),(e.dem10.f[3]+(j)*diff1))
#   wet_area1.1<-crop(wet_area1,e.smal)
#   f_name<-paste("M:/Geo-Data/wetness_fjall/wetness_part_",j,".tif",sep="")
#   writeRaster(wet_area1.1, filename=f_name, format="GTiff", overwrite=TRUE)
# }
# 



library(mapview)

nmd<-raster("F:/NMD/nmd2018bas_ogeneraliserad_v1_0.tif")


for (i in c(0:353))
{

file.name<-paste("C:/wetness/sweden_saga/wetness_part_",i,".tif",sep="")
wet.tail<-raster(file.name)

e.w<-extent(wet.tail)

nmd.w<-crop(nmd,e.w)
wet.val<-getValues(wet.tail)
nmd.val<-getValues(nmd.w)
nmd.sjo<-ifelse(nmd.val%in%c(61,62),0,1)
wet.val.c<-ifelse(nmd.sjo==1,wet.val,NA)
wet.tail<-setValues(wet.tail,wet.val.c)

file.name<-paste("C:/wetness/wetness_saga_sjo/wetness_part_utan_sjo_",i,".tif",sep="")
writeRaster(wet.tail, filename=file.name, format="GTiff", overwrite=TRUE)
}





#spatila transfomation
for (i in c(0:352))
{
  
  file.name<-paste("C:/wetness/wetness_saga_sjo/wetness_part_utan_sjo_",i,".tif",sep="")
  wet.tail<-raster(file.name)
  wet.tail99<-projectRaster(wet.tail,crs=crs(projSWEREF))
  
  file.name<-paste("C:/wetness/wetness_saga_sjo_sw99/wetness_part_utan_sjo_sw99_",i,".tif",sep="")
  writeRaster(wet.tail, filename=file.name, format="GTiff", overwrite=TRUE)
}


merge.raster(filesource="C:/wetness/wetness_saga_sjo_sw99/"
             ,file_collection="wetness_part_utan_sjo_sw99"
             ,temp_file="L:/DATA/temp_raster/temp.tif"
             ,target_file="C:/wetness/wetness_swe_complete.tif",
             proj="+init=epsg:3006")










