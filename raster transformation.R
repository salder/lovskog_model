
#transfomr raster values by a given border to 1 (lager than the border) ore 0 (lower than the border)
#do not forgett to set raster options
rasterOptions(tmpdir="L:/DATA/temp_raster/")




raster_to_01<-function(filename="tree_prediction_2_tile.tif",
                       sourcelocation="L:/Lovtrad_model",
                       targetlocation="L:/Lovtrad_model",
                       tempfile="L:/DATA/temp_raster/temp.tif",
                       border=0.51
                       )

{
      x <- raster(filename)
      out<-x
      bs <- blockSize(out)
      filename=tempfile
      out <- writeStart(out, filename, overwrite=TRUE)
      for (i in 1:bs$n)
      {
        val <- getValues(x, row=bs$row[i], nrows=bs$nrows[i] )
        val.t<-ifelse(val>border,1,0)
        out <- writeValues(out, pred, bs$row[i])
      }
      out <- writeStop(out)
      nam<-paste("pa_",filename,sep="")
      writeRaster(out, filename=nam, format="GTiff", overwrite=TRUE)

}