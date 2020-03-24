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





########################################################################################################################################
########################################################################################################################################

#model building
taxdata<-readRDS("F:/Lovtrad_model/model_data_lov_adel.rds")


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
  #dplyr::select(-Tackningsarea1) %>% 
  #filter(tree_hight_swe>50) %>% data.frame() #tree larger than 7 m (test the effect!)

#plot(taxdata.red$Ostkoordinat,taxdata.red$Nordkoordinat)


trainingsdata<-taxdata.red%>% filter(IsPermanent==1) 
testdata<-taxdata.red%>% filter(IsPermanent==0) 


library(mgcv)

form<-as.formula(adelandel/100~
                    s(band_1)
                  +s(band_2)
                  +s(band_3)
                  +s(band_4)
                  +s(band_5)
                  +s(band_6)
                  +s(band_7)
                  +s(band_8)
                  +s(band_9)
                  +s(band_10)
                  +s(band_11)
                  +s(band_12)
                  +s(lovandel)
                  +as.factor(lovskog)
)

fit.adel<-gam(form,data=trainingsdata,"quasibinomial")


pred.adel<-predict(fit.adel,testdata,type="response")
testdata$pred.adel<-pred.adel


test.res<-testdata %>% mutate(adellov=ifelse(adelandel/100>0.5,1,0)) %>% 
                      mutate(adellov.pre=ifelse(pred.adel>0.5,1,0))%>% 
                      mutate(pred.error=adellov-adellov.pre)
prop.table(table(test.res$pred.error))

test.res1<-testdata %>% mutate(adellov=ifelse(adelandel/100>0.5,1,0)) %>% 
  mutate(adellov.pre=ifelse(pred.adel>0.5,1,0))%>% 
  mutate(pred.error=adellov-adellov.pre) %>% filter(lovskog_k==1)
prop.table(table(test.res1$pred.error))



test.res1<-testdata %>% mutate(


