#' Parallelized function to extract data from a raster or raster stack for indivdiual polygons within a shapefile.
#'
#' @param shpfile The shapefile to be used to intersect with the raster layers
#' @param rast The raster layer(s) to be intersected
#' @param catid_col the catchment id field in the shpfile
#' @param fun The function to apply. Requires a single value is returned
#' (e.g. fun = mean, fun = max). see `raster::extract`` for more details
#' @param weights see `Raster::extract`
#' @param normalizeWeights see `Raster::extract`
#' @param na.rm see `Raster::extract`
#' @param small see `Raster::extract`
#' @param start the first polygon in the shapefile to be used for intersection
#' Defaults to a value of 1 (the first row)
#' @param end The last polygon in the shapfile to be intersected.
#' @return a dataframe of area-weighted raster values for each polygon.
#'
#' @examples
#'
#' #Note the code below will only work on Mac/Unix based systems.
#' #Other implementations of the parallel processors (e.g. PSOCK) may work on Windows but are untested.
#' #The following to lines are required to initiate the parallel processors
#'
#' library(parallel)
#' no_cores=detectCores()-1
#'
#' #Define a bounding box for the city of Melbourne, Australia and surrounds
#' melb <- list(x = c(144.0000, 146.5000), y = c(-39.0000, -37.0000))
#' awap_stack <- stack_awap(bbox=melb, start='20130131')
#' vic_cats<-readShapePoly('/Users/nbond/Data/GIS_Data/Australia/BaseData/Catchments/VIC/VIC_CATCHMENTS.shp')
#' proj4string(vic_cats) <- CRS('+init=epsg:4283')
#' vic_cats <- spTransform(vic_cats, CRS('+init=epsg:28355'))
#' vic_cats <- vic_cats[which(vic_cats@data$BNAME %in% c('YARRA RIVER', 'MARIBYRNONG RIVER')),]
#' plot(awap_stack[[1]])
#' plot(vic_cats, add=TRUE)
#'
#' vic_cats_runoff<-extractRasterPoly_cl(vic_cats, rast=melb_awap, catid_col='BNAME')
#'
#' @export

extractRasterPoly_cl <- function(shpfile, rast, catid_col, fun = mean, weights = TRUE, normalizeWeights = TRUE, na.rm=TRUE, small = TRUE, start = 1, end = nrow(shpfile)) {
  loc.values <- vector("list", length(start:end))
  shpfile <- shpfile[start:end, ]
  cl<-parallel::makeCluster(no_cores, type="FORK")
  fits<-parallel::parLapplyLB(cl,1:length(shpfile),function(i) {
  loc.values[[i]] <- raster::extract(rast, shpfile[i,], na.rm = na.rm, weights = weights, fun = fun, normalizeWeights = normalizeWeights, small = small)
  })
  parallel::stopCluster(cl)
  loc.values.df <- as.data.frame(t(do.call("rbind", fits)))
  names(loc.values.df) <- shpfile[[catid_col]]
  return(loc.values.df)
}


