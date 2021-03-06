# modelling deciduous tree in south Sweden


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
altitude<-raster("M:/THUF-Fjall/hojddata2.tif")




#####################################################################################################################################
#data from GEE

#1.download from drive
library(googledrive)
drive_find()    #choose "2"
setwd("L:/Lovtrad_model/GEE")

t1<-drive_find()
files1<-grep(c("ndvi1"),t1$name,value=TRUE)
files2<-grep(c("ndvi2"),t1$name,value=TRUE)
files3<-grep(c("ndvi_diff"),t1$name,value=TRUE)
files4<-grep(c("tree_hight"),t1$name,value=TRUE)
files<-c(files1,files2,files3,files4)




for (i in 1:length(files))
  drive_download(file=files[i],overwrite=TRUE)


scene_list<-dir(path="L:/Lovtrad_model/GEE",pattern=".tif",full.names = TRUE)

#combining to one scen per parameter
parameter_list<-c(
 "ndvi1",
 "ndvi2",
 "ndvi_diff",
 "tree_hight"
)

for (k in 4:length(parameter_list))
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
  
  file_name<-paste("L:/Lovtrad_model/Sentinel2",parameter,".tif", sep="")
  writeRaster(mm, filename=file_name, format="GTiff", overwrite=TRUE)
  file.remove(scene_list_red)
}

####################################################################################################################
####################################################################################################################

#model buildiung


ndvi1.r<-raster("L:/Lovtrad_model/Sentinel2ndvi1.tif")
ndvi2.r<-raster("L:/Lovtrad_model/Sentinel2ndvi2.tif")
ndvi_diff.r<-raster("L:/Lovtrad_model/Sentinel2ndvi_diff.tif")
trad_hight.r<-raster("L:/Lovtrad_model/Sentinel2tree_hight.tif")


taxdata<-readRDS("F:/Lovtrad_model/trainings_data.rds")
taxdata<-taxdata %>% filter(DelytaNr==0) %>% filter(Taxar>2013)

taxdata.sp<-SpatialPointsDataFrame(coords=taxdata[,c("Ostkoordinat","Nordkoordinat")],data=taxdata,proj4string=CRS(projSWEREF))

taxdata$ndvi1<-extract(ndvi1.r,taxdata.sp)
taxdata$ndvi2<-extract(ndvi2.r,taxdata.sp)
taxdata$ndvi_diff<-extract(ndvi_diff.r,taxdata.sp)
taxdata$trad_hight<-extract(trad_hight.r,taxdata.sp)

#buffer just to test different radius för extraction
taxdata$ndvi1b<-extract(ndvi1.r,taxdata.sp,buffer=10,fun=mean)
taxdata$ndvi2b<-extract(ndvi2.r,taxdata.sp,buffer=10,fun=mean)
taxdata$ndvi_diffb<-extract(ndvi_diff.r,taxdata.sp,buffer=10,fun=mean)
taxdata$trad_hightb<-extract(trad_hight.r,taxdata.sp,buffer=10,fun=mean)

saveRDS(taxdata,file="F:/Lovtrad_model/model_data.rds")

#plot(taxdata$Ostkoordinat,taxdata$Nordkoordinat)


#data selection before modelling
taxdata.red<-taxdata %>% 
  filter(!is.na(ndvi1)) %>%     # only plots that are "forest" (pixels with tree(laser) > 5 meter)
  filter(DelytaNr==0) %>%       # only undevided plots
  filter(Nordkoordinat<6600000) %>%  #south Sweden
  mutate(trad=Tallandel+Contortaandel+Granandel+Bjorkandel+Aspandel+Oadeltandel+Ekandel+Bokandel+Adelandel) %>% 
  mutate(lovtrad=Bjorkandel+Aspandel+Oadeltandel+Ekandel+Bokandel+Adelandel) %>% 
  mutate(lovandel=lovtrad/trad) %>% 
  filter(trad>80) %>%  #should be always 100 but it seems to be errors in the data
  filter(Taxar>2013) %>%  #time period 2014-2019
  #filter(Krontackning>60) %>% #(test the effect!)
  mutate(lovskog=ifelse(lovandel>0.55,1,0)) %>% # %>% dplyr::select(lovandel,ndvi1,ndvi2,ndvi_diff,lovskog,trad_hight,IsPermanent)
  mutate(lovandel_k=Krontackning*lovandel/100) %>% 
  mutate(lovskog_k=ifelse(lovandel_k>0.55,1,0)) %>%  
  filter(trad_hight>50) #tree larger than 7 m (test the effect!)

#plot(taxdata.red$Ostkoordinat,taxdata.red$Nordkoordinat)

taxdata.red<-taxdata.red[,c(1:50,99:112)]  #excluding no needet columns
taxdata.red<-na.omit(taxdata.red)          #remove all rows with NA values   

taxtrain<-taxdata.red %>% filter(IsPermanent==1) 
taxtest<-taxdata.red %>%filter(IsPermanent==0)

#plot(taxtrain$lovandel~taxtrain$ndvi_diff)

library(mgcv)

fit.gam<-gam(lovandel~s(ndvi1)
             +s(ndvi2)
             +s(ndvi_diff)
             +s(trad_hight)
             ,data=taxtrain,"quasibinomial")
summary(fit.gam)
gam.check(fit.gam)
plot(fit.gam,pages=1,scale=F,shade=T)
plot(taxtrain$lovskog,fitted(fit.gam))


fit2.gam<-gam(lovandel_k~s(ndvi1)                
             +s(ndvi2)
             +s(ndvi_diff)
             +s(trad_hight)
             ,data=taxtrain,"quasibinomial")
summary(fit2.gam)
gam.check(fit2.gam)
plot(fit2.gam,pages=1,scale=F,shade=T)
plot(taxtrain$lovskog,fitted(fit.gam))






pred<-predict(fit.gam,taxtest,type="response")
taxtest$pred<-ifelse(pred>0.55,1,0)
taxtest$diff=taxtest$lovskog-taxtest$pred
round(prop.table(table(taxtest$lovskog-taxtest$pred)),3)

#more right model!
  pred<-predict(fit2.gam,taxtest,type="response")
  taxtest$pred_k<-ifelse(pred>0.55,1,0)
  taxtest$diff_k=taxtest$lovskog_k-taxtest$pred_k
  round(prop.table(table(taxtest$lovskog_k-taxtest$pred_k)),3)









taxtest<-as.data.frame(taxtest)
taxtest$pred1<-unlist(pred1)
taxtest<-taxtest %>% mutate(lovskog_p=unlist(pred1))

taxtest$diff<-taxtest$lovskog-taxtest$pred1

taxtest$pred<-round(pred,4)

d1<-data.frame(taxtest$Ostkoordinat,taxtest$Nordkoordinat)#,taxtest$diff)

taxtest.sp<-SpatialPointsDataFrame(coords=taxtest[,c("Ostkoordinat","Nordkoordinat")],data=taxtest,proj4string=CRS(projSWEREF))



library(mapview)
mapview(taxtest.sp,zcol="diff")

taxtest %>% filter(Taxar==2018,TraktNr.x==4133,PalslagNr.x==408) %>% t()
taxtest %>% filter(Taxar==2018,TraktNr.x==4133,PalslagNr.x==404) %>% t()
taxtest %>% filter(Taxar==2016,TraktNr.x==4236,PalslagNr.x==204) %>% t()
taxtest %>% filter(Taxar==2016,TraktNr.x==5039,PalslagNr.x==406) %>% t()


plot(taxtest$Ostkoordinat,taxtest$Nordkoordinat,col=taxtest$diff+1,pch=19)

d1<-as.data.frame(taxtest[taxtest$diff==1,])







hist(taxtest$mod.error)

plot(taxtest$mod.error~taxtest$VolymAlla)





fit2.gam<-gam(lovskog~s(ndvi1)
             +s(ndvi2)
             +s(ndvi_diff)
             ,data=subset(taxtrain) ,"binomial")
summary(fit2.gam)
gam.check(fit2.gam)


plot(taxtrain$lovskog,fitted(fit.gam))










####################################################################################################
#####################################################################################################

#B: whole Sweden! 

# try: do not merge the raster tiles that come from GEE handel them seperately
# - loop to extract the three values
# - model for the south Sweden / model for northern Sweden due to differnt ndvi values in Mars / April for lövskogar
# - loop over the tiels to predict lov/barr träd
# - there is a little place 10x10 km west on Jokkmokk not covered by ndvi in the spring situation
# - as there were a gap in the souh for the data above the selection data as the clowd cover was changed, cheque the models for that!!!



#data from GEE

#1.download from drive
library(googledrive)
drive_find()    #choose "2"
setwd("L:/Lovtrad_model/GEE")

t1<-drive_find()
files1<-grep(c("ndvi_swe2"),t1$name,value=TRUE)
files2<-grep(c("ndvi2_swe2"),t1$name,value=TRUE)
files3<-grep(c("ndvi_diff_swe2"),t1$name,value=TRUE)
files4<-grep(c("tree_hight_swe2"),t1$name,value=TRUE)
files<-c(files1,files2,files3,files4)




for (i in 1:length(files))
  drive_download(file=files[i],overwrite=TRUE)







#merging/extracting with tax data
#number of files might be different (e.g. for tree_hight) -> four different loops (not nessesarely if all the same!)
scene_list<-dir(path="L:/Lovtrad_model/GEE",pattern=".tif",full.names = TRUE)
files1<-grep(c("ndvi_swe2"),scene_list,value=TRUE)
files2<-grep(c("ndvi2_swe2"),scene_list,value=TRUE)
files3<-grep(c("ndvi_diff_swe2"),scene_list,value=TRUE)
files4<-grep(c("tree_hight_swe2"),scene_list,value=TRUE)



#NFI data
taxdata<-readRDS("F:/Lovtrad_model/trainings_data.rds")                
taxdata<-taxdata %>% filter(DelytaNr==0) %>% filter(Taxar>2013)

taxdata.sp<-SpatialPointsDataFrame(coords=taxdata[,c("Ostkoordinat","Nordkoordinat")],data=taxdata,proj4string=CRS(projSWEREF))



tile.extract<-function(file_list=files1,data.sp=taxdata.sp,var.name="ndvi1_swe")
    {
          train_m<-NA
          #ndvi1
          for (i in 1:length(file_list))
          {
           #i=1
            ras.t<-raster(file_list[i])                   #.t for "tiles"
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
          names(train.res)[[3]]<-var.name
          print(train.res)
}



dat.ndvi1_swe<-tile.extract(file_list=files1,data=taxdata.sp,var.name="ndvi1_swe")
dat.ndvi2_swe<-tile.extract(file_list=files2,data=taxdata.sp,var.name="ndvi2_swe")
dat.ndvi_diff_swe<-tile.extract(file_list=files3,data=taxdata.sp,var.name="ndvi_diff_swe")
dat.tree_high_swe<-tile.extract(file_list=files4,data=taxdata.sp,var.name="tree_hight_swe")
#join with tax data using the coordinates!
taxdata.m<-taxdata %>% left_join(.,dat.ndvi1_swe,by=c("Ostkoordinat","Nordkoordinat")) %>% 
                       left_join(.,dat.ndvi2_swe,by=c("Ostkoordinat","Nordkoordinat"))%>% 
                       left_join(.,dat.ndvi_diff_swe,by=c("Ostkoordinat","Nordkoordinat")) %>% 
                       left_join(.,dat.tree_high_swe,by=c("Ostkoordinat","Nordkoordinat")) 

saveRDS(taxdata.m,file="F:/Lovtrad_model/model_data_lov_sweden.rds")

###################################################################################################################
#modelbuilding
#the same as above but with a model south and a model north
#

#data selection before modelling
taxdata.m<-readRDS("F:/Lovtrad_model/model_data_lov_sweden.rds")

taxdata.red<-taxdata.m %>% 
  filter(!is.na(ndvi1_swe)) %>%     # only plots that are "forest" (pixels with tree(laser) > 5 meter)
  filter(DelytaNr==0) %>%       # only undevided plots
  mutate(data.set=ifelse(Nordkoordinat<6600000,"S","N")) %>%  #south Sweden
  mutate(trad=Tallandel+Contortaandel+Granandel+Bjorkandel+Aspandel+Oadeltandel+Ekandel+Bokandel+Adelandel) %>% 
  mutate(lovtrad=Bjorkandel+Aspandel+Oadeltandel+Ekandel+Bokandel+Adelandel) %>% 
  mutate(lovandel=lovtrad/trad) %>% 
  filter(trad>80) %>%  #should be always 100 but it seems to be errors in the data
  filter(Taxar>2013) %>%  #time period 2014-2019
  #filter(Krontackning>60) %>% #(test the effect!)
  mutate(lovskog=ifelse(lovandel>0.55,1,0)) %>% # %>% dplyr::select(lovandel,ndvi1,ndvi2,ndvi_diff,lovskog,trad_hight,IsPermanent)
  mutate(lovandel_k=Krontackning*lovandel/100) %>% 
  mutate(lovskog_k=ifelse(lovandel_k>0.55,1,0)) %>% 
  dplyr::select(-Tackningsarea1) %>% 
  filter(tree_hight_swe>50) %>% data.frame() #tree larger than 7 m (test the effect!)

#plot(taxdata.red$Ostkoordinat,taxdata.red$Nordkoordinat)

taxdata.red<-taxdata.red[,c(1:50,96:108)]  #excluding no needet columns
taxdata.red<-na.omit(taxdata.red)          #remove all rows with NA values   






#fit.south


taxtrain<-taxdata.red %>% filter(IsPermanent==1,data.set=="S") 
taxtest<-taxdata.red %>%filter(IsPermanent==0,data.set=="S")

#plot(taxtrain$lovandel~taxtrain$ndvi_diff)

library(mgcv)

# fit.gam<-gam(lovandel~s(ndvi1_swe)
#              +s(ndvi2_swe)
#              +s(ndvi_diff_swe)
#              +s(tree_hight_swe)
#              ,data=subset(taxtrain),"quasibinomial")
# summary(fit.gam)
# gam.check(fit.gam)
# plot(fit.gam,pages=1,scale=F,shade=T)
# plot(taxtrain$lovskog,fitted(fit.gam))


fit_south.gam<-gam(lovandel_k~s(ndvi1_swe)                
              +s(ndvi2_swe)
              +s(ndvi_diff_swe)
             # +s(tree_hight_swe)
              ,data=taxtrain,"quasibinomial")
summary(fit_south.gam)
gam.check(fit_south.gam)
plot(fit_south.gam,pages=1,scale=F,shade=T)
plot(taxtrain$lovskog,fitted(fit_south.gam))


# pred<-predict(fit.gam,taxtest,type="response")
# taxtest$pred<-ifelse(pred>0.55,1,0)
# taxtest$diff=taxtest$lovskog-taxtest$pred
# round(prop.table(table(taxtest$lovskog-taxtest$pred)),3)

#more right model!
pred<-predict(fit_south.gam,taxtest,type="response")
taxtest$pred_k<-ifelse(pred>0.55,1,0)
taxtest$diff_k=taxtest$lovskog_k-taxtest$pred_k
round(prop.table(table(taxtest$lovskog_k-taxtest$pred_k)),3)




#fit.north


taxtrain<-taxdata.red %>% filter(IsPermanent==1,data.set=="N") 
taxtest<-taxdata.red %>%filter(IsPermanent==0,data.set=="N")


fit_north.gam<-gam(lovandel_k~s(ndvi1_swe)                
                   +s(ndvi2_swe)
                   +s(ndvi_diff_swe)
                  # +s(tree_hight_swe)
                   ,data=taxtrain,"quasibinomial")
summary(fit_north.gam)
gam.check(fit_north.gam)
plot(fit_north.gam,pages=1,scale=F,shade=T)
plot(taxtrain$lovskog,fitted(fit_south.gam))


# pred<-predict(fit.gam,taxtest,type="response")
# taxtest$pred<-ifelse(pred>0.55,1,0)
# taxtest$diff=taxtest$lovskog-taxtest$pred
# round(prop.table(table(taxtest$lovskog-taxtest$pred)),3)

#more right model!
pred<-predict(fit_north.gam,taxtest,type="response")
taxtest$pred_k<-ifelse(pred>0.55,1,0)
taxtest$diff_k=taxtest$lovskog_k-taxtest$pred_k
round(prop.table(table(taxtest$lovskog_k-taxtest$pred_k)),3)





#################################################################################################################
#################################################################################################################

#prediction to the raster tiles
#two possibilities, parallel processing (snow+raster as i the MAS project) or prediction as function looping over the raster
# ->problem with missing laser data in the mountain area
# ->idea: using nmd. If nmd indicates forest where laser data are missing -> set tree hight to 6 meter (mostly mountain birtch forest)
# ->easier within a loop than in parallel processing

rasterOptions(tmpdir="L:/DATA/temp_raster/")
projRT90 <- "+init=epsg:3021 +towgs84=414.0978567149,41.3381489658,603.0627177516,-0.8550434314,2.1413465,-7.0227209516,0 +no_defs"
projSWEREF <- "+init=epsg:3006"


tree_hight<-raster("L:/Lovtrad_model/p95.tif")

scene_list<-dir(path="L:/Lovtrad_model/GEE",pattern=".tif",full.names = TRUE)
files1<-grep(c("ndvi_swe2"),scene_list,value=TRUE)
files2<-grep(c("ndvi2_swe2"),scene_list,value=TRUE)
files3<-grep(c("ndvi_diff_swe2"),scene_list,value=TRUE)


for( j in 1:length(files1))
{

ndvi1_swe.t<-raster(files1[j])
ndvi2_swe.t<-raster(files2[j])
ndvi_diff_swe.t<-raster(files3[j])


if (ndvi1_swe.t@extent[1]>6600000) fit.gam<-fit_north.gam else fit.gam<-fit_south.gam

    out <- raster(ndvi1_swe.t)
    bs <- blockSize(out)
    filename="L:/DATA/temp_raster/temp.tif"
    out <- writeStart(out, filename, overwrite=TRUE)
    for (i in 1:bs$n)
            {
            ndvi1_swe <- getValues(ndvi1_swe.t, row=bs$row[i], nrows=bs$nrows[i] )
            ndvi2_swe <- getValues(ndvi2_swe.t, row=bs$row[i], nrows=bs$nrows[i] )
            ndvi_diff_swe <- getValues(ndvi_diff_swe.t, row=bs$row[i], nrows=bs$nrows[i] )
            data.pred<-data.frame(ndvi1_swe,ndvi2_swe,ndvi_diff_swe)
            pred<-round(predict(fit.gam,newdata=data.pred,type="response"),3)
            out <- writeValues(out, pred, bs$row[i])
            }
    out <- writeStop(out)
file.nam<-paste("L:/Lovtrad_model/tree_prediction_",j,"_tile.tif",sep="")
writeRaster(out, filename=file.nam, format="GTiff", overwrite=TRUE)
}





##########################################################
#transfomartion to 0 (non deciduous tree) and 1 (> more than 50% deciduous tree


scene_list<-dir(path="L:/Lovtrad_model",pattern="tree_prediction")

source("F:/Lovtrad_model/raster transformation.R")

for (j in 1:length(scene_list))
        raster_to_01(filename=scene_list[j],
                    sourcelocation="L:/Lovtrad_model",
                    targetlocation="L:/Lovtrad_model/pa_lovtrad",
                    tempfile="L:/DATA/temp_raster/temp.tif",
                    border=0.51 #larger than this value
                    )







##########################################################
#merge the sinlge tiles

source("F:/Lovtrad_model/merge_raster_tiles.R")
merge.raster(filesource="L:/Lovtrad_model/pa_lovtrad/"
                                     ,file_collection="pa_"
                                     ,temp_file="L:/DATA/temp_raster/temp.tif"
                                     ,target_file="L:/Lovtrad_model/lovtrad_sverige.tif",
                                     proj="+init=epsg:3006")

merge.raster(filesource="L:/Lovtrad_model"
             ,file_collection="tree_pred"
             ,temp_file="L:/DATA/temp_raster/temp.tif"
             ,target_file="L:/Lovtrad_model/lovtrad_sverige_sanolikhet_0_1.tif",
             proj="+init=epsg:3006")



#########################################################
#Gotland (Anton + Sören Wulff)

library(raster)
lovtrad<-raster("L:/Lovtrad_model/lovtrad_sverige_sanolikhet_0_1.tif")
#clip Gotland
e.g<-extent(c(638126.4,778304,6263141,6467036 ))
lovtrad.got<-crop(lovtrad,e.g)  
writeRaster(lovtrad.got, filename="L:/Lovtrad_model/lovtrad_gotland.tif", format="GTiff", overwrite=TRUE) 
  
  

raster_to_01(filename="lovtrad_gotland.tif",
             sourcelocation="L:/Lovtrad_model",
             targetlocation="L:/Lovtrad_model",
             tempfile="L:/DATA/temp_raster/temp.tif",
             border=10 #larger than this value /100
)





raster_to_01(filename="lovtrad_gotland.tif",
             sourcelocation="L:/Lovtrad_model",
             targetlocation="L:/Lovtrad_model",
             tempfile="L:/DATA/temp_raster/temp.tif",
             border=20 #larger than this value /100
)


raster_to_01(filename="lovtrad_gotland.tif",
             sourcelocation="L:/Lovtrad_model",
             targetlocation="L:/Lovtrad_model",
             tempfile="L:/DATA/temp_raster/temp.tif",
             border=30 #larger than this value /100
)

raster_to_01(filename="lovtrad_gotland.tif",
             sourcelocation="L:/Lovtrad_model",
             targetlocation="L:/Lovtrad_model",
             tempfile="L:/DATA/temp_raster/temp.tif",
             border=50 #larger than this value /100
)

raster_to_01(filename="lovtrad_gotland.tif",
             sourcelocation="L:/Lovtrad_model",
             targetlocation="L:/Lovtrad_model",
             tempfile="L:/DATA/temp_raster/temp.tif",
             border=5 #larger than this value /100
)


#areal lovtrad gottland
lov.got<-raster("L:/Lovtrad_model/lovtrad_gotland.tif")
lov.val<-getValues(lov.got)
sum(lov.val*10^2,na.rm=T)


lov.got<-raster("L:/Lovtrad_model/pa_border_10_lovtrad_gotland.tif")
lov.val<-getValues(lov.got)
sum(lov.val*10^2,na.rm=T)

lov.got<-raster("L:/Lovtrad_model/pa_border_20_lovtrad_gotland.tif")
lov.val<-getValues(lov.got)
sum(lov.val*10^2,na.rm=T)

lov.got<-raster("L:/Lovtrad_model/pa_border_30_lovtrad_gotland.tif")
lov.val<-getValues(lov.got)
sum(lov.val*10^2,na.rm=T)


lov.got<-raster("L:/Lovtrad_model/pa_border_50_lovtrad_gotland.tif")
lov.val<-getValues(lov.got)
sum(lov.val*10^2,na.rm=T)



lov.got<-raster("L:/Lovtrad_model/pa_border_5_lovtrad_gotland.tif")
lov.val<-getValues(lov.got)
sum(lov.val*10^2,na.rm=T)




