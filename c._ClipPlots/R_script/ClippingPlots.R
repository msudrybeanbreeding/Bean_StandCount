### Method 1

rm(list=ls())

my.path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(my.path)

library(raster)
library(EBImage)
library(rgdal)
library(sp)
library(future)
library(future.apply)

######################Separete individually plots according to shp ---- Fullplots shp
###Get all the files to process from the WD
#imgFiles <- dir("C:/Users/leoag/Michigan State University/MSU Dry Bean Breeding Lab - General/UAS_Beans/2022/SVREC/Pix4D_projects_RGB/6_13_22_SVREC_RGB_7m/3_dsm_ortho/2_mosaic",
#               pattern="*.tif$")
imgFiles <-list.files(pattern="*.tif$") #get the orthomosaic. Files thad their name ends in _mosaic.tif
imgFiles
indPlots <- readOGR(dsn = ".\\shp", layer = "SVREC_final_plot_7m")
names(indPlots)
#indPlots <- readOGR(dsn = folder_shp, layer = layer_prefix_shp)


## Only to add name extension
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

outFile <- substr(imgFiles,1,nchar(imgFiles)-29)
#outFile <- paste0("Plot_", outFile)
#shape_name <- c("Global_ID")

plot_clip <- function(ortho, shape, shape_name){
for(i in ortho){
  message("processing ",i)
  
  ## Data frame with plot names
  indPlots_list <- as.data.frame(shape[,c(shape_name)])
  #head(indPlots_list)
  indPlots_list$units <- seq.int(nrow(indPlots_list))
  plots <- indPlots_list$units
  #i=ortho
  #roi_plot <- spTransform(indPlots,CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))#Reproject to have on the same CRS as the images (assumed to be WGS84)

  #Iterate trough all the plots to get the separete plots, using the ID list of the plots
  
  folder_name <- tools::file_path_sans_ext(basename(outFile)) # Name to make folder for the ROI, removed path and extention
  dir.create(folder_name, recursive=TRUE) 
  
  future.apply::future_lapply(plots, function(p){
    
    message("procesing ",p)
   # p=13
    # crop the plots to the extent of every ortomosaic
    plot_crop <-crop(brick(i,bands = c(1,2,3)), extent(shape[p,]))
    plot_crop <- mask(plot_crop, shape[p,])
    
    #outFile <- paste0(indPlots[p,]@data$Global_ID)
    #Save file
    ## Copy all based on the list to the nested folder
    # writeRaster(plot_crop, filename=file.path(folder_name, paste0("Plot_",outFile,'_', i)),datatype='FLT4S',format="GTiff",overwrite=TRUE)
    writeGDAL(as(plot_crop, "SpatialGridDataFrame"),fname = file.path(folder_name, paste0(indPlots_list[p,], "_7m",".png", sep = "")), drivername = "PNG", type = "CFloat64")
    
    #inform about saving
    print(paste("saved raster:",indPlots_list[p,]," Succesfully")) 
    
  }) 

}
}


# clipping all seeds in single shots
plot_clip(imgFiles, indPlots, "Global_ID")


## Method 2

# ####################################
# # Clipping mosaic into single shots
# ###################################
# library(raster)
# library(EBImage)
# library(FIELDimageR)
# library(rgdal)
# 
# #importing DH mosaics
# setwd("D:/OneDrive - Michigan State University/HTP_Esalq_Classes/datasets/datasets/DHmosaic")
# (mosaics <- dir())
# 
# mosaic_1 <- stack(mosaics[1])
# print(mosaic_1)
# plot(mosaic_1) #plot all bands
# plotRGB(mosaic_1)
# 
# # creating a shapefile on R (plots)
# DH.Shape <- fieldShape(mosaic = mosaic_1 ,ncols = 7, nrows = 3)
# # the count is always from left to right and from top to bottom
# DH.Shape$fieldShape
# 
# plotRGB(mosaic_1)
# plot(DH.Shape$fieldShape, add = T, col = "Blue", alpha = 0.2)
# 
# #clipping raster by plots - a function
# plot_clip <- function(ortho, shape){
#   plot_raster <- list()
#   for (i in 1:nrow(shape)) {
#     cat("plot_", i, " ")
#     p <- shape[i,]
#     plot_raster[[i]] <- crop(x = ortho, y = extent(p))
#     plot_raster[[i]] <- mask(plot_raster[[i]], p)
#     names(plot_raster)[i] <- shape$plot[i]
#   }
#   return(plot_raster)
# }

# # clipping all seeds in single shots
# rasterbyplots <- plot_clip(mosaic_1, DH.Shape$fieldShape)
# class(rasterbyplots)
# length(rasterbyplots)
# plotRGB(rasterbyplots[[1]])
# 
# # save the single shots in a folder
# # First, let's create a new directory
# dir.create("D:/HTP_Esalq_Classes/datasets/datasets/ssDH")
# out.dir <- "D:/HTP_Esalq_Classes/datasets/datasets/ssDH"
# 
# # Save the singles images .png format into the newest directory
# for (i in 1:length(rasterbyplots)) {
#   writeGDAL(as(rasterbyplots[[i]], "SpatialGridDataFrame"), paste(out.dir, "/", "D", "_", i, ".png", sep = ""), drivername = "PNG", type = "CFloat64")
#   
# }

##### The End #######


