####D

library(raster)
library(rgdal)
library(RStoolbox)
library(dplyr)
library(sp)

rasterOptions(tmpdir="C:/TEMP")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"
#biogeo<-readRDS("M:/Sicherung neuer Rechner/R_rds/biogeografiska_regioner_sw99.rds")
#boreal<-subset(biogeo,EU_REGION=="Boreal")
#altitude<-raster("M:/THUF-Fjall/hojddata2.tif")



#this part runs on my litel labtpo without to much disc space
#just dowload 5 tiles, do prediction, deleet the tiles and download the next 5 tiles. 

# prediction

#1. download the required tiles
#2. merge with lovtrad models tif
#3. extract the needed bands
#4. predict
#5. save
#6. merge to one file


#this part runs on my litel labtpo without to much disc space
#just dowload 5 tiles, do prediction, deleet the tiles and download the next 5 tiles. 
library(mgcv)

fit.adel<-readRDS("F:/Lovtrad_model/gam_adellov_d.rds")






#loop to download 5 files and do prediction on them


# file_list<-dir("//SLU012304/l/Lovtrad_model/GEE",full.name=TRUE)
# file_list.s<-dir("//SLU012304/l/Lovtrad_model/GEE")
# file_list.s<-paste("L:/GEE/",file_list.s,sep="")
# file.copy(file_list,file_list.s,overwrite = TRUE)



t1<-dir("L:/Lovtrad_model/GEE/",full.names = TRUE)

file_juni<-grep(c("adellov_juni"),t1,value=TRUE)
file_aug<-grep(c("adellov_august"),t1,value=TRUE)
file_juni<-file_juni[order(file_juni,decreasing =T)]
file_aug<-file_aug[order(file_aug,decreasing =T)]

lovtrad.r<-raster("L:/Lovtrad_model/lovtrad_sverige_sanolikhet_0_1_d.tif")



ceck<-c(
  FALSE, FALSE, FALSE, FALSE, FALSE,  TRUE, TRUE,  TRUE, FALSE,
  FALSE, FALSE,  TRUE,  TRUE,  TRUE,  TRUE, TRUE , TRUE, FALSE,
  FALSE, FALSE,  TRUE,  TRUE,  TRUE, TRUE , TRUE,  TRUE, FALSE,
  TRUE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  TRUE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  TRUE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  TRUE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  TRUE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  TRUE,
  TRUE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE,
  TRUE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE,
  FALSE,  TRUE,  TRUE, TRUE,  TRUE,  TRUE,  TRUE,  TRUE,  FALSE)


file_juni<-file_juni[ceck==TRUE]
file_aug<-file_aug[ceck==TRUE]




#clipp to lovtradmodel rasterlovtrad.r<-raster("D:/UMEA/Lovtrad/lovtrad_sverige_sanolikhet_0_1.tif")

#borders for the single parallel loops 
#1 1:22
#2 23:44
#3 45:66
#4 67:83
#


for (m in c(10:22))
{
  aug.r<-raster(file_aug[m],band=1)
  jun.r<-raster(file_juni[m],band=1)
  print(extent(jun.r)==extent(aug.r))
  e.r<-extent(jun.r)
  lov.r<-crop(lovtrad.r,e.r)
  e.l<-extent(lov.r)    #extent of the forest modle is not hte same as the tiles for ädellov
  
  band_3.r<-crop(raster(file_juni[m],band=3),e.l,file="C:/TEMP/temp_3c.tif",overwrite=TRUE)
  band_4.r<-crop(raster(file_juni[m],band=4),e.l,file="C:/TEMP/temp_4.ctif",overwrite=TRUE)
  #writeRaster(band_4.r, filename="C:/Lovtrad/tiles/band_4.r", format="GTiff", overwrite=TRUE)
  band_5.r<-crop(raster(file_juni[m],band=5),e.l,file="C:/TEMP/temp_5c.tif",overwrite=TRUE)
  band_6.r<-crop(raster(file_juni[m],band=6),e.l,file="C:/TEMP/temp_6c.tif",overwrite=TRUE)
  band_7.r<-crop(raster(file_juni[m],band=7),e.l,file="C:/TEMP/temp_7c.tif",overwrite=TRUE)
  band_8.r<-crop(raster(file_juni[m],band=8),e.l,file="C:/TEMP/temp_8c.tif",overwrite=TRUE)
  band_9.r<-crop(raster(file_juni[m],band=9),e.l,file="C:/TEMP/temp_9c.tif",overwrite=TRUE)
  
  band_10.r<-crop(raster(file_juni[m],band=10),e.l,file="C:/TEMP/temp_10c.tif",overwrite=TRUE)
  band_11.r<-crop(raster(file_juni[m],band=11),e.l,file="C:/TEMP/temp_11c.tif",overwrite=TRUE)
  
  
  band_aug2.r<-crop(raster(file_aug[m],band=2),e.l,file="C:/TEMP/temp_12c.tif",overwrite=TRUE)
  band_aug3.r<-crop(raster(file_aug[m],band=3),e.l,file="C:/TEMP/temp_13c.tif",overwrite=TRUE)
  band_aug4.r<-crop(raster(file_aug[m],band=4),e.l,file="C:/TEMP/temp_14c.tif",overwrite=TRUE)
  band_aug6.r<-crop(raster(file_aug[m],band=6),e.l,file="C:/TEMP/temp_15c.tif",overwrite=TRUE)
  band_aug7.r<-crop(raster(file_aug[m],band=7),e.l,file="C:/TEMP/temp_16c.tif",overwrite=TRUE)
  band_aug8.r<-crop(raster(file_aug[m],band=8),e.l,file="C:/TEMP/temp_17c.tif",overwrite=TRUE)
  band_aug9.r<-crop(raster(file_aug[m],band=9),e.l,file="C:/TEMP/temp_18c.tif",overwrite=TRUE)
  band_aug10.r<-crop(raster(file_aug[m],band=10),e.l,file="C:/TEMP/temp_19c.tif",overwrite=TRUE)
  band_aug11.r<-crop(raster(file_aug[m],band=11),e.l,file="C:/TEMP/temp_20.tif",overwrite=TRUE)
  band_aug12.r<-crop(raster(file_aug[m],band=12),e.l,file="C:/TEMP/temp_21.tif",overwrite=TRUE)
  
  
  
  
  
  out <- raster(lov.r)
  bs <- blockSize(out,minblocks=300)
  filename="C:/TEMP/temp_d.tif"
  out <- writeStart(out, filename, overwrite=TRUE)
  for (i in 1:bs$n)
  {
    band_3<-getValues(band_3.r, row=bs$row[i], nrows=bs$nrows[i])
    band_4<-getValues(band_4.r, row=bs$row[i], nrows=bs$nrows[i])
    band_5<-getValues(band_5.r, row=bs$row[i], nrows=bs$nrows[i])
    band_6<-getValues(band_6.r, row=bs$row[i], nrows=bs$nrows[i])
    band_7<-getValues(band_7.r, row=bs$row[i], nrows=bs$nrows[i])
    band_8<-getValues(band_8.r, row=bs$row[i], nrows=bs$nrows[i])
    band_9<-getValues(band_9.r, row=bs$row[i], nrows=bs$nrows[i])
    band_10<-getValues(band_10.r, row=bs$row[i], nrows=bs$nrows[i])
    band_11<-getValues(band_11.r, row=bs$row[i], nrows=bs$nrows[i])
    #band_12<-getValues(band_12.r, row=bs$row[i], nrows=bs$nrows[i])
    lovtrad.m<-getValues(lov.r, row=bs$row[i], nrows=bs$nrows[i])
    
    #band_aug1<-getValues(band_aug1.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug2<-getValues(band_aug2.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug3<-getValues(band_aug3.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug4<-getValues(band_aug4.r, row=bs$row[i], nrows=bs$nrows[i])
    #band_aug5<-getValues(band_aug5.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug6<-getValues(band_aug6.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug7<-getValues(band_aug7.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug8<-getValues(band_aug8.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug9<-getValues(band_aug9.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug10<-getValues(band_aug10.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug11<-getValues(band_aug11.r, row=bs$row[i], nrows=bs$nrows[i])
    band_aug12<-getValues(band_aug12.r, row=bs$row[i], nrows=bs$nrows[i])
    
    data.pred<-data.frame(band_3
                          ,band_4
                          ,band_5
                          ,band_6
                          ,band_7
                          ,band_8
                          ,band_9
                          ,band_10
                          ,band_11
                          #,band_12
                          ,lovtrad.m
                          #,band_aug1
                          ,band_aug2
                          ,band_aug3
                          ,band_aug4
                          #,band_aug5
                          ,band_aug6
                          ,band_aug7
                          ,band_aug8
                          ,band_aug9
                          ,band_aug10
                          ,band_aug11
                          ,band_aug12
                          
    )
    
    
    # pred.all<-NULL
    # kmax<-round(dim(data.pred)[1]/800,0)+1
    # 
    # for (mk in (1:kmax))
    # {dat<-data.pred[c((1+(mk-1)*800):(mk*800)),]
    # pred.s<-round(predict(fit.adel,newdata=dat,type="response"),3)
    # pred.all<-c(pred.all,pred.s)
    # }
    pred<-round(predict(fit.adel,newdata=data.pred,type="response"),3)
    out <- writeValues(out, pred, bs$row[i])
  }
  out <- writeStop(out)
  file.nam<-paste("L:/Lovtrad_model/lovtrad_result/adellov_prediction_",m,"_tile.tif",sep="")
  writeRaster(out, filename=file.nam, format="GTiff", overwrite=TRUE)
  
}

#}

#file.remove(file_aug)
#file.remove(file_juni)


