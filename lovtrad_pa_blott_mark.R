#combine wetness index with lovtrad model


library(raster)


######################################################

wetness<-raster("C:/wetness/wetness_swe_complete.tif")
lovtrad<-raster("L:/Lovtrad_model/lovtrad_sverige_sanolikhet_0_1_c.tif")

e1<-extent(lovtrad)
wetness.e<-crop(wetness,e1)
e1<-extent(wetness.e)
lovtrad.e<-crop(lovtrad,e1)

extent(lovtrad.e)==extent(wetness.e)



out <- raster(lovtrad.e)
bs <- blockSize(out,minblocks=400)
filename="C:/TEMP/temp.tif"
out <- writeStart(out, filename, overwrite=TRUE)
for (i in 1:bs$n)
{
  lov.p<-getValues(lovtrad.e, row=bs$row[i], nrows=bs$nrows[i])
  wet.p<-getValues(wetness.e, row=bs$row[i], nrows=bs$nrows[i])
  lov.p<-ifelse(wet.p>12,lov.p,NA)
  out <- writeValues(out,lov.p, bs$row[i])
}
out <- writeStop(out)
file.nam<-"L:/lovtrad_result/lovtrad_pa_blott_mark.tif"
writeRaster(out, filename=file.nam, format="GTiff", overwrite=TRUE)


