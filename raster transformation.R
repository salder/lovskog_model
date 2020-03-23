
#transfomr raster values by a given border to 1 (lager than the border) ore 0 (lower than the border)
#do not forgett to set raster options
rasterOptions(tmpdir="L:/DATA/temp_raster/")




raster_to_01<-function(filename="tree_prediction_2_tile.tif",
                       sourcelocation="L:/Lovtrad_model",
                       targetlocation="L:/Lovtrad_model",
                       tempfile="L:/DATA/temp_raster/temp.tif",
                       border=0.51 #larger than this value
                       )

{
      filename.c<-paste(sourcelocation,"/",filename,sep="")
      x <- raster(filename.c)
      out<-x
      bs <- blockSize(out)
      file.temp=tempfile
      out <- writeStart(out, file.temp, overwrite=TRUE)
      for (i in 1:bs$n)
      {
        val <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
        val.t<-ifelse(val>border,1,0)
        out <- writeValues(out, val.t, bs$row[i])
      }
      out <- writeStop(out)
      nam<-paste(targetlocation,"/pa_border_",border,"_",filename,sep="")
      writeRaster(out, filename=nam, format="GTiff", overwrite=TRUE,datatype='INT1U')

}