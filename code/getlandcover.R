#' Get Land Cover
#' 
#' Get the consensus land cover and convert to a 1km x 1km raster from https://www.earthenv.org/landcover. These are massive files and if you go to the url, you can download just for your region. I did not see an API to download just an extent like you can from the ERDDAP servers for remote sensing data.
#' 
#' If you have already downloaded the global files and have them, it won't download again but will read in the existing files and make cropped rasters based on your extent.
#' 
#' The CRS is +proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0  
#' and the resolution is the same as the worldclim on a 0.5 grid and 
#' the elevation data so you can stack this on the worldclim and elevation raster layers.
#' 
#' @variable Number or numbers 1 to 12 for the variables to download. By default, all are downloaded.
#' @param gisdir Where the files will be saved. Needs to exist so make ahead of time. Default is a folder data in your working directory.
#' @param ext The spatial extent as a raster Extent object or 4 numbers (minlon, maxlon, minlat, maxlat).
#' @param clean If TRUE and download is TRUE, it will delete the downloaded global files.
#' @param download If TRUE, the land cover files will be downloaded even if they already exist. If false, it will look for previously downloaded global files and use those.
#' @param overwrite If TRUE will re-download the files (if download=TRUE). 
#' 
#' @details The land cover variables are the following and value in the layers are 0 to 100 (percent cover).
#' * 1	Evergreen/Deciduous Needleleaf Trees	 
#' * 2	Evergreen Broadleaf Trees	 
#' * 3	Deciduous Broadleaf Trees	 
#' * 4	Mixed/Other Trees	 
#' * 5	Shrubs	 
#' * 6	Herbaceous Vegetation	 
#' * 7	Cultivated and Managed Vegetation	 
#' * 8	Regularly Flooded Vegetation	 
#' * 9	Urban/Built-up	 
#' * 10	Snow/Ice	 
#' * 11	Barren	 
#' * 12	Open Water
#' 
#' @value Saves a raster as a tif called `lc_1km_i` where i is a number 1 to 12 or dom for the dominant land cover.
#' 
#' @examples 
#' # to run this example, make sure the data/landcover dir exists
#' # getlandcover(gisdir="data/landcover", ext=c(-70, 72, 40, 42))
#' 
#' # this will be on a scale of 1 to 12 for which cover is dominant
#' fil <- "data/landcover/lc_1km_dom.tif"
#' r <- raster(fil)
#' plot(r)
#' 
#' # this will be on a scale of 1 to 100 for percent cover
#' # water
#' fil <- "data/landcover/lc_1km_12.tif"
#' r12 <- raster(fil)
#' plot(r12)
#' # trees
#' for(i in 1:4){
#' fil <- paste0("data/landcover/lc_1km_",i,".tif")
#' r <- raster(fil)
#' if(i==1) rt <- r else rt <- r+rt
#' }
#' plot(rt)
#' @md
getlandcover <- function (variable=1:12, gisdir="data", 
                          ext=raster::extent(c(-180, 180, -56, 90)), 
                          clean=FALSE, download=TRUE, overwrite=FALSE) 
{
  if(!inherits(ext, "Extent")) ext <- raster::extent(ext)
  oldwd = getwd()
  on.exit(expr = setwd(oldwd))
  setwd(gisdir)
  
  # if !download check that all the files exist
  if(!download){
    eefils <- paste0("Consensus_reduced_class_", variable, ".tif")
    if(!all(file.exists(eehils)))
      stop("Missing some of the global files. Set download to TRUE to get them.")
  }
  # Get files from EarthEnv
  if(download){
  url <- "http://data.earthenv.org/consensus_landcover/without_DISCover/Consensus_reduced_class_"
  for (i in variable) {
    eefil <- paste0("Consensus_reduced_class_", i, ".tif")
    if(file.exists(eefil) && !overwrite) next # skip to next file
    download.file(
      paste0(url, i, ".tif"), 
      destfile = eefil, mode = "wb")
  }
  }
  for (i in variable) {
    lcfil <- paste0("lc_1km_", i, ".tif")
    eefil <- paste0("Consensus_reduced_class_", i, ".tif")
    rast <- raster::raster(paste("Consensus_reduced_class_", i, ".tif", sep = ""))
    rast <- raster::crop(rast, ext)
    writeRaster(rast, lcfil, overwrite=TRUE)
    if(download & clean) file.remove(paste("Consensus_reduced_class_", i, ".tif", sep = ""))
    gc() # clean up memory
  }
  remove(rast)
  # This part gets the dominant landcover from all 12 layers
  max1 <- raster()
  for (i in 1:12) {
    rast <- raster(paste("lc_1km_", i, ".tif", sep = ""))
    max1 <- raster::stack(max1, rast)
  }
  max1 <- which.max(max1)
  writeRaster(max1, "lc_1km_dom.tif", overwrite=TRUE)
  remove(max1)
  gc() # garbage collection
  setwd(oldwd)
}