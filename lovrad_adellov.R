# modelling deciduous tree in constrast to real wood (triviallöv mod ädellöv)


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


#######################################################################################################################################

######juni scene

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
#dat.band13<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=13)
#dat.band14<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=14)
#dat.band15<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=15)
#dat.band16<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=16)


taxdata.m<-taxdata %>% left_join(.,dat.band1,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band2,by=c("Ostkoordinat","Nordkoordinat"))%>% 
  left_join(.,dat.band3,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band4,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band5,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band6,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band7,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band8,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band9,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band10,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band11,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band12,by=c("Ostkoordinat","Nordkoordinat")) 

saveRDS(taxdata.m,file="F:/Lovtrad_model/model_data_lov_adel.rds")

#merge with lovtrad model

lovtrad.r<-raster("L:/Lovtrad_model/lovtrad_sverige_sanolikhet_0_1.tif")

taxdata.m.sp<-SpatialPointsDataFrame(coords=taxdata.m[,c("Ostkoordinat","Nordkoordinat")],data=taxdata.m,proj4string=CRS(projSWEREF))

taxdata.m$lovtrad.m<-extract(lovtrad.r,taxdata.m.sp)
saveRDS(taxdata.m,file="F:/Lovtrad_model/model_data_lov_adel.rds")

#################################################################################################################################################
#august scene


library(googledrive)
drive_find()    #choose "2"
setwd("L:/Lovtrad_model/GEE")

t1<-drive_find()
files1<-grep(c("adellov_august"),t1$name,value=TRUE)



for (i in 1:length(files1))
  drive_download(file=files1[i],overwrite=TRUE)



filelist_adlov<-dir(path="L:/Lovtrad_model/GEE",pattern="adellov_aug",full.names = TRUE)


#NFI data
taxdata<-readRDS("F:/Lovtrad_model/model_data_lov_adel.rds")                
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
  var.name=paste("band_aug",bandnr,sep="")
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
#dat.band13<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=13)
#dat.band14<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=14)
#dat.band15<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=15)
#dat.band16<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=16)


taxdata.m<-taxdata %>% left_join(.,dat.band1,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band2,by=c("Ostkoordinat","Nordkoordinat"))%>% 
  left_join(.,dat.band3,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band4,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band5,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band6,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band7,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band8,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band9,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band10,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band11,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band12,by=c("Ostkoordinat","Nordkoordinat")) 

saveRDS(taxdata.m,file="F:/Lovtrad_model/model_data_lov_adel_aug.rds")

#merge with lovtrad model

lovtrad.r<-raster("L:/Lovtrad_model/lovtrad_sverige_sanolikhet_0_1.tif")

taxdata.m.sp<-SpatialPointsDataFrame(coords=taxdata.m[,c("Ostkoordinat","Nordkoordinat")],data=taxdata.m,proj4string=CRS(projSWEREF))

taxdata.m$lovtrad.m<-extract(lovtrad.r,taxdata.m.sp)
saveRDS(taxdata.m,file="F:/Lovtrad_model/model_data_lov_adel_aug.rds")




#################################################################################################################################################
#october scene


library(googledrive)
drive_find()    #choose "2"
setwd("L:/Lovtrad_model/GEE")

t1<-drive_find()
files1<-grep(c("adellov_host"),t1$name,value=TRUE)



for (i in 1:length(files1))
  drive_download(file=files1[i],overwrite=TRUE)



filelist_adlov<-dir(path="L:/Lovtrad_model/GEE",pattern="adellov_aug",full.names = TRUE)


#NFI data
taxdata<-readRDS("F:/Lovtrad_model/model_data_lov_adel_aug.rds")                
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
  var.name=paste("band_oct",bandnr,sep="")
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
#dat.band13<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=13)
#dat.band14<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=14)
#dat.band15<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=15)
#dat.band16<-tile.extract.multilayer(file_list=filelist_adlov,data=taxdata.sp,bandnr=16)


taxdata.m<-taxdata %>% left_join(.,dat.band1,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band2,by=c("Ostkoordinat","Nordkoordinat"))%>% 
  left_join(.,dat.band3,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band4,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band5,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band6,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band7,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band8,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band9,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band10,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band11,by=c("Ostkoordinat","Nordkoordinat")) %>% 
  left_join(.,dat.band12,by=c("Ostkoordinat","Nordkoordinat")) 

saveRDS(taxdata.m,file="F:/Lovtrad_model/model_data_lov_adel_aug_okt.rds")

#merge with lovtrad model

lovtrad.r<-raster("L:/Lovtrad_model/lovtrad_sverige_sanolikhet_0_1.tif")

taxdata.m.sp<-SpatialPointsDataFrame(coords=taxdata.m[,c("Ostkoordinat","Nordkoordinat")],data=taxdata.m,proj4string=CRS(projSWEREF))

taxdata.m$lovtrad.m<-extract(lovtrad.r,taxdata.m.sp)
saveRDS(taxdata.m,file="F:/Lovtrad_model/model_data_lov_adel_aug_okt.rds")







########################################################################################################################################
########################################################################################################################################

#model building
taxdata<-readRDS("D:/UMEA/Lovtrad/model_data_lov_adel_aug_okt.rds")


taxdata.red<-taxdata %>% 
  filter(DelytaNr==0) %>%  # only undevided plots
  filter(!is.na(band_4)) %>%   #south Sweden 
  mutate(trad=Tallandel+Contortaandel+Granandel+Bjorkandel+Aspandel+Oadeltandel+Ekandel+Bokandel+Adelandel) %>% 
  mutate(lovtrad=Bjorkandel+Aspandel+Oadeltandel+Ekandel+Bokandel+Adelandel) %>% 
  mutate(lovandel=lovtrad/trad) %>% 
  mutate(adelandel=Ekandel+Bokandel) %>% 
  filter(trad>80) %>%  #should be always 100 but it seems to be errors in the data
  filter(Taxar>2013) %>%  #time period 2014-2019
  #filter(Krontackning>60) %>% #(test the effect!)
  mutate(lovskog=ifelse(lovandel>0.55,1,0)) %>% # %>% dplyr::select(lovandel,ndvi1,ndvi2,ndvi_diff,lovskog,trad_hight,IsPermanent)
  mutate(lovandel_k=Krontackning*lovandel/100) %>% 
  mutate(lovskog_k=ifelse(lovandel_k>0.55,1,0)) #%>% 
# filter(lovskog==1)
#dplyr::select(-Tackningsarea1) %>% 
#filter(tree_hight_swe>50) %>% data.frame() #tree larger than 7 m (test the effect!)

#plot(taxdata.red$Ostkoordinat,taxdata.red$Nordkoordinat)


trainingsdata<-taxdata.red%>% filter(IsPermanent==1) %>% filter(lovskog==1)
testdata<-taxdata.red%>% filter(IsPermanent==0)# %>% filter(lovskog==1)


library(mgcv)

form<-as.formula(adelandel/100~
                   #  s(band_1)
                   #  s(band_2)
                   +s(band_3,k=3) #new
                 +s(band_4,k=3)
                 +s(band_5)
                 +s(band_6,k=3)#new
                 +s(band_7)
                 +s(band_8,k=3)    #sist med!
                 +s(band_9)
                 +s(band_10)
                 +s(band_11)
                 #+s(band_12,k=3)#new
                 +s(lovtrad.m,k=3)
                 #+s(band_aug1,k=3)#new
                 +s(band_aug2,k=3)
                 +s(band_aug3,k=3)
                 +s(band_aug4,k=3)
                 # +s(band_aug5,k=3)#new
                 +s(band_aug6,k=3)
                 +s(band_aug7,k=3)
                 +s(band_aug8,k=3)
                 +s(band_aug9,k=3)  #new
                 +s(band_aug10,k=3) #new
                 +s(band_aug11,k=3)
                 +s(band_aug12,k=3)
                 #+s(band_oct2)
                 #+s(band_oct3)
                 #+s(band_oct4)
                 #+s(band_oct4)
                 # +s(band_oct5)
                 # +s(band_oct6)
                 # +s(band_oct7)
                 # +s(band_oct8)
                 # +s(band_oct9)
                 # +s(band_oct10)
                 # +s(band_oct11)
                 # +s(band_oct12)
                 #+s(band_oct7)
                 
                 #+as.factor(lovskog)
)

fit.adel<-gam(form,data=(trainingsdata),"quasibinomial")
#fit.adel<-bam(form,data=(trainingsdata),"quasibinomial")
summary(fit.adel)
saveRDS(fit.adel,file="D:/UMEA/Lovtrad/gam_adellov.rds")
saveRDS(fit.adel,file="D:/UMEA/Lovtrad/gam_adellov_a.rds")
saveRDS(fit.adel,file="D:/UMEA/Lovtrad/gam_adellov_b.rds")
saveRDS(fit.adel,file="D:/UMEA/Lovtrad/gam_adellov_c.rds")
saveRDS(fit.adel,file="D:/UMEA/Lovtrad/gam_adellov_d.rds")
#source("drop_cont.R")
#time consuming!! run just one time with the final model!
#res<-drop_cont(form=form,data_use=trainingsdata,fam="quasibinomial")


#alternativt modell

# library(rpart)
# fit.rpart<-rpart(adelandel/100~
#                     band_1
#                  +band_2
#                  +band_3
#                  +band_4
#                  +band_5
#                  +band_6
#                  +band_7
#                  +band_8
#                  +band_9
#                  +band_10
#                  +band_12
#                  +lovandel
#                  
#                  
#                  
#                  
# ,data=trainingsdata)
# 
# pred.rpart<-predict(fit.rpart,testdata)
# testdata$pred.rpart<-pred.rpart
# 
# test.res<-testdata %>% mutate(adellov=ifelse(adelandel/100>0.5,1,0)) %>% 
#   mutate(adellov.pre=ifelse(pred.rpart>0.5,1,0))%>% 
#   mutate(pred.error=adellov-adellov.pre)
# 
# prop.table(table(test.res$pred.error))
# 
# test.res1<-testdata %>% mutate(adellov=ifelse(adelandel/100>0.5,1,0)) %>% 
#   mutate(adellov.pre=ifelse(pred.rpart>0.5,1,0))%>% 
#   mutate(pred.error=adellov-adellov.pre) %>% 
#   mutate(pred.sum=adellov+adellov.pre)%>% filter(lovskog_k==1)
# 
# 
# prop.table(table(test.res1$pred.error))
# 



###########################################################################
#GAM
pred.adel<-predict(fit.adel,subset(testdata),type="response")
testdata$pred.adel<-round(pred.adel,5)





test.res<-testdata %>% mutate(adellov=ifelse(adelandel/100>0.5,1,0)) %>% 
  mutate(adellov.pre=ifelse(pred.adel>0.1,1,0))%>%       #o.1!!!!
  mutate(pred.error=adellov-adellov.pre)
prop.table(table(test.res$pred.error))

test.res1<-testdata %>% mutate(adellov=ifelse(adelandel/100>0.5,1,0)) %>% 
  mutate(adellov.pre=ifelse(pred.adel>0.1,1,0))%>% 
  mutate(pred.error=adellov-adellov.pre) %>% 
  mutate(pred.sum=adellov+adellov.pre)%>% 
  #filter(lovskog_k==1)
  filter(adellov==1)




table(test.res1$pred.error)
prop.table(table(test.res1$pred.error))

View(test.res1)

t(data.frame(test.res1[5,]))

boxplot(test.res1$Ekandel~test.res1$adellov.pre)





test.res2<-test.res1 %>% filter(pred.sum>0) %>% filter(pred.error==1)

test.res2<-test.res1 %>% filter(pred.sum>0)
table(test.res2$pred.error)

test.res2.sp<-SpatialPointsDataFrame(coords=test.res2[,c("Ostkoordinat","Nordkoordinat")],data=test.res2,proj4string=CRS(projSWEREF))

library(mapview)
mapview(test.res2.sp,zcol="pred.sum")




##########################################################################################################################################
###########################################################################################################################################
#########################################################################################################################################

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

fit.adel<-readRDS("D:/UMEA/Lovtrad/gam_adellov.rds")






#loop to download 5 files and do prediction on them


# file_list<-dir("//SLU012304/l/Lovtrad_model/GEE",full.name=TRUE)
# file_list.s<-dir("//SLU012304/l/Lovtrad_model/GEE")
# file_list.s<-paste("L:/GEE/",file_list.s,sep="")
# file.copy(file_list,file_list.s,overwrite = TRUE)



t1<-dir("L:/GEE/",full.names = TRUE)

file_juni<-grep(c("adellov_juni"),t1,value=TRUE)
file_aug<-grep(c("adellov_august"),t1,value=TRUE)
file_juni<-file_juni[order(file_juni,decreasing =T)]
file_aug<-file_aug[order(file_aug,decreasing =T)]

lovtrad.r<-raster("D:/UMEA/Lovtrad/lovtrad_sverige_sanolikhet_0_1.tif")

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




for (m in c(3:22))
{
  aug.r<-raster(file_aug[m],band=1)
  jun.r<-raster(file_juni[m],band=1)
  print(extent(jun.r)==extent(aug.r))
  e.r<-extent(jun.r)
  lov.r<-crop(lovtrad.r,e.r)
  e.l<-extent(lov.r)    #extent of the forest modle is not hte same as the tiles for ädellov
  
  band_3.r<-crop(raster(file_juni[m],band=3),e.l,file="C:/TEMP/temp_3.tif",overwrite=TRUE)
  band_4.r<-crop(raster(file_juni[m],band=4),e.l,file="C:/TEMP/temp_3.tif",overwrite=TRUE)
  #writeRaster(band_4.r, filename="C:/Lovtrad/tiles/band_4.r", format="GTiff", overwrite=TRUE)
  band_5.r<-crop(raster(file_juni[m],band=5),e.l,file="C:/TEMP/temp_3.tif",overwrite=TRUE)
  band_6.r<-crop(raster(file_juni[m],band=6),e.l,file="C:/TEMP/temp_3.tif",overwrite=TRUE)
  band_7.r<-crop(raster(file_juni[m],band=7),e.l,file="C:/TEMP/temp_3.tif",overwrite=TRUE)
  band_8.r<-crop(raster(file_juni[m],band=8),e.l,file="C:/TEMP/temp_5.tif",overwrite=TRUE)
  band_9.r<-crop(raster(file_juni[m],band=9),e.l,file="C:/TEMP/temp_5.tif",overwrite=TRUE)
  
  band_10.r<-crop(raster(file_juni[m],band=10),e.l,file="C:/TEMP/temp_5.tif",overwrite=TRUE)
  band_11.r<-crop(raster(file_juni[m],band=11),e.l,file="C:/TEMP/temp_5.tif",overwrite=TRUE)
  
  
  band_aug2.r<-crop(raster(file_aug[m],band=2),e.l,file="C:/TEMP/temp_8.tif",overwrite=TRUE)
  band_aug3.r<-crop(raster(file_aug[m],band=3),e.l,file="C:/TEMP/temp_9.tif",overwrite=TRUE)
  band_aug4.r<-crop(raster(file_aug[m],band=4),e.l,file="C:/TEMP/temp_10.tif",overwrite=TRUE)
  band_aug6.r<-crop(raster(file_aug[m],band=6),e.l,file="C:/TEMP/temp_12.tif",overwrite=TRUE)
  band_aug7.r<-crop(raster(file_aug[m],band=7),e.l,file="C:/TEMP/temp_13.tif",overwrite=TRUE)
  band_aug8.r<-crop(raster(file_aug[m],band=8),e.l,file="C:/TEMP/temp_14.tif",overwrite=TRUE)
  band_aug9.r<-crop(raster(file_aug[m],band=9),e.l,file="C:/TEMP/temp_14.tif",overwrite=TRUE)
  band_aug10.r<-crop(raster(file_aug[m],band=10),e.l,file="C:/TEMP/temp_14.tif",overwrite=TRUE)
  band_aug11.r<-crop(raster(file_aug[m],band=11),e.l,file="C:/TEMP/temp_17.tif",overwrite=TRUE)
  band_aug12.r<-crop(raster(file_aug[m],band=12),e.l,file="C:/TEMP/temp_18.tif",overwrite=TRUE)
  
  
  
  
  
  out <- raster(lov.r)
  bs <- blockSize(out,minblocks=400)
  filename="C:/TEMP/temp.tif"
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
    pred<-round(predict(fit.adel,newdata=data.pred,type="response"),5)
    out <- writeValues(out, pred, bs$row[i])
  }
  out <- writeStop(out)
  file.nam<-paste("L:/lovtrad_result/adellov_prediction_",m,"_tile.tif",sep="")
  writeRaster(out, filename=file.nam, format="GTiff", overwrite=TRUE)
}

#}

#file.remove(file_aug)
#file.remove(file_juni)












#######################################################################################
#######################################################################################


file.list<-dir("C:/Lovtrad/adel_cor_f_lov/",full.names = TRUE)

swe.pol<-readOGR("C:/Lovtrad","an_riks")
lovtrad.r<-raster("D:/UMEA/Lovtrad/lovtrad_sverige_sanolikhet_0_1.tif")


plot(swe.pol)
for (i in 1:length(file.list))
{
  tile<-raster(file.list[i])
  e<-extent(tile)
  e<-as(e,'SpatialPolygons')
  proj4string(e)<-projSWEREF
  plot(e,add=T,col=2)
}
plot(swe.pol,add=T)     



#sel<-c(37,38,39)
#file.list<-file.list[sel]

for (i in 1:length(file.list))
{
  tile<-raster(file.list[i])
  e<-extent(tile)
  lov.c<-crop(lovtrad.r,e)
  adel.pred<-getValues(tile)
  lov.pred<-getValues(lov.c)
  adel.pred<-ifelse(lov.pred>0.4,adel.pred,0)
  adel.pred<-round(adel.pred,3)
  tile<-setValues(tile,adel.pred)
  tile<-projectRaster(tile,crs=crs(projSWEREF))
  m<-strsplit(file.list[i],"_")[[1]][4]
  file.nam<-paste("C:/Lovtrad/adel_cor_f_lov/adellov_cor_f_lov_",m,"_tile.tif",sep="")
  writeRaster(tile, filename=file.nam, format="GTiff", overwrite=TRUE)
}


source("merge_raster.R")

merge.raster(filesource="C:/Lovtrad/adel_cor_f_lov/"
             ,file_collection="adellov_cor_f_lov"
             ,temp_file="C:/TEMP/temp_adel.tif"
             ,target_file="C:/Lovtrad/adellov_sverige_complete.tif",
             proj="+init=epsg:3006")

