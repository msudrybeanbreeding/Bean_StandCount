########################################
# Counting the number of objects - stand
########################################
library(raster)
library(sp)
library(tidyr)
library(FIELDimageR)
library(rgdal)
library(future)
library(parallel)
library(doParallel)
######################

rm(list=ls())
######################Separete individually plots according to shp ---- Fullplots shp
###Get all the files to process from the WD
setwd("C:/Users/leoag/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2022/SVREC/Pix4D_projects_RGB/6_13_22_SVREC_RGB_7m/3_dsm_ortho/2_mosaic")
#(mosaics <- dir())
imgFiles <-list.files(pattern="*.tif$") #get the orthomosaic. Files thad their name ends in _mosaic.tif
imgFiles

indPlots <- readOGR(dsn = "C:/Users/leoag/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2022/SVREC/Shapefile/StandCount/shp", layer = "SVREC_final_plot_7m")
names(indPlots)

#indPlots[243,]
#2201_1001_7m

ortho_name <- substr(imgFiles,1,nchar(imgFiles)-30)
ortho_name

beans_count <- function(ortho, shape, field_name, ortho_name){
  
  output_sc <- data.frame()
  
  for(k in 1:length(ortho)){
    message("processing ",ortho[k])

    ortho <- stack(ortho[k])
      
    #for (i in 1:2) {
    for (i in 1:nrow(shape)) {

      p <- shape[i,]
      plot_raster <- crop(x = ortho, y = extent(p))
      plot_raster <- mask(plot_raster, p)
      #names(plot_raster)[i] <- shape$Global_ID[i]

      message("Done plot clip - ", shape$Global_ID[i])
      
      temp <- fieldMask(mosaic = plot_raster, Red = 1, Green = 2, Blue = 3,  
                        myIndex = c("((2*Green) - Red - Blue) - ((1.4*Red)-Green)"),
                        cropValue = 1, plot = F)
      
      masks <- temp$mask
      
      message("Done plot mask - ", shape$Global_ID[i])
      
      count.plot = fieldCount(mosaic = masks, fieldShape = shape[i,], cex = 0.8, col = "red", 
                              minSize = 0.01, value = 1, na.rm = TRUE, n.core=12)
      output_sc <- rbind(output_sc, data.frame(
        Global_ID = shape@data[i, field_name],        
        count = count.plot$fieldCount,
        ortho = ortho_name
      ))
      
      message("Counting plot done - ", shape$Global_ID[i])
      
      rm(count.plot,masks, plot_raster)
      gc()

    }
  }
 return(output_sc)
}
      
test_count<- beans_count(imgFiles,indPlots, "Global_ID", ortho_name)

setwd("C:/Users/leoag/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2022/SVREC/Analysis/StandCount/R_results")
write.csv(test_count, "Stand_Count_6_13_22_SVREC_RGB_7m_ExV.csv", row.names = F )


