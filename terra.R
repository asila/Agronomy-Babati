library(terra)

help(terra)

library(dplyr)

mpc <- rast('~/Downloads/MostProbable.vrt')

# Get sampled locations
pts <- read.csv('~/Alliance/Job/Data/Cleaned/Field_survey_quality.csv')

pts <- pts %>%
select(c('gpslatitude', 'gpslongitude','gpsaltitude'))

pts <- as.matrix(na.omit(pts[,1:2]))

pts <- vect(pts,crs="+proj=longlat +datum=WGS84")


bb_p <- extract(mpc, pts)


a <- rast(ncols=40, nrows=40, xmin=-110, xmax=-90, ymin=40, ymax=60, 
          crs="+proj=longlat +datum=WGS84")
          
values(a) <- 1:ncell(a)
## SpatVector
f <- system.file("ex/lux.shp", package="terra")

v <- vect(f)

crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84"

project(v,crs)


# from a matrix 
m <- matrix(1:50, nrow=5, ncol=10)
rm <- rast(m)


library(rgdal)
library(gdalUtils)
raste

bb=c(-337500.000,1242500.000,152500.000,527500.000) # Example bounding box (homolosine) for Ghana
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs' # proj string for Homolosine projection

sg_url="/vsicurl?max_retry=3&retry_delay=1&list_dir=no&url=https://files.isric.org/soilgrids/latest/data/"

gdal_translate(paste0(sg_url,'ocs/ocs_0-30cm_mean.vrt'),
    "./crop_roi_igh_r.tif",
    tr=c(250,250),
    projwin=bb,
    projwin_srs =igh,
    verbose=TRUE)

gdal_translate("~/Downloads/MostProbable.vrt",  
    ".~/Downloads/MostProbable.tif", 
    co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES"))
test <- raster('~/Downloads/MostProbable.vrt.ovr')

str(test)