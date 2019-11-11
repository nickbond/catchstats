#' Parallelized code to build a raster stack from AWAP grids for use in further analyses.
#'
#' @param bbox A list of coordinates defining a subregion for the raster stack
#' @param stack_proj A projection definition for the final projected raster stack
#' @param start_date The first month in the AWAP series to inlcude in the stack. If
#' only interested in recent years, this will greatly improve the efficiciency.
#' @param variable The AWAP variable to extract data for.
#' @param sequence Monthly or Annual series
#' @param stat Build a stack of the raw data or the percentile sequence (by monthly only).
#' @param no_cores The number of parallel cores to use.
#' @return A raster stack of CSIRO AWAP files, stacked by date.
#' @note The function assumes the awap data files are in the directory raw_awap_data, which is
#' a subdirectory of the working directory from which the function is being run (see download_awap for more details).
#' @examples
#'
#' library(parallel)
#' no_cores=detectCores()-1
#'
#' #Define a bounding box for the city of Melbourne, Australia and surrounds
#' melb <- list(x = c(144.0000, 146.5000), y = c(-39.0000, -37.0000))
#' stack_awap_cl(bbox=melb, start_date='20130131', seq="monthly")
#' #South-east Queensland
#' seqld <- list(x = c(149, 154.000), y = c(-21.0000, -29.0000))
#' seq_awap <- stack_awap_cl(bbox=seqld, start_date='20130131')
#' writeRaster(seq_awap, filename='seq_awap.grd', bandorder='BIL', overwrite=TRUE)
#' vic <- list(x = c(141.0, 150.2), y = c(-34.0, -39.2))
#' vic_awap <- stack_awap(bbox=vic, start_date='19900131')
#' writeRaster(vic_awap, filename='vic_awap_1990_2014.grd', bandorder='BIL', overwrite=TRUE)
#' @export

stack_awap_cl <- function(bbox = NULL, start_date = "20000131", variable = "FWDis", sequence = "monthly", stat = "raw") {
  if (is.null(bbox))
    stop("please specify a bounding box or specify 'none' for the whole country")

  # We want the flt files
  hdr_files <- list.files(path=file.path("awap_raw_data"), pattern = "\\.flt$")
  # get the files pertaining to the variable of interest
  hdr_files_var <- hdr_files[grepl(variable, hdr_files)]
  if (stat == "raw") {
    hdr_files_var <- hdr_files_var[!grepl("pcr", hdr_files_var)]
    } else {
    if (stat == "percentile") {
      hdr_files_var <- hdr_files_var[grepl("pcr", hdr_files_var)]
    }
  }
  # get monthly or annual files
  if (sequence == "monthly") {
    hdr_files_to_stack <- hdr_files_var[!grepl("ann", hdr_files_var)]
  } else {
    if (sequence == "annual") {
      hdr_files_to_stack <- hdr_files_var[grepl("ann", hdr_files_var)]
    }
  }
  if (sequence == "monthly") {
    month_name <- regmatches(hdr_files_to_stack, regexpr("[0-9].*[0-9]", hdr_files_to_stack))
    final_name_files_correct_order <- sort(month_name, decreasing = FALSE, index.return = TRUE)[[2]]
  } else {

    # list the files and sort out correct ordering by date
    if (sequence == "annual") {
      month_name <- regmatches(hdr_files_to_stack, regexpr("[0-9].*[0-9]", hdr_files_to_stack))
      final_name_files_correct_order <- sort(month_name, decreasing = FALSE, index.return = TRUE)[[2]]
    }
  }
  bound_box <- NULL
  if (length(bbox) > 1) {
    bound_box <- raster::extent(bbox)
  }
  # Set unprojected reference system GDA94
  #unref <- CRS("+init=epsg:4283")
  list_raster<-list()
  # try building the stack now
  start = which(month_name == start_date)
  end = final_name_files_correct_order[length(final_name_files_correct_order)]
  final_name_files_correct_order <- final_name_files_correct_order[start:end]

  #loop_raster <- raster::raster(paste0("awap_raw_data/", hdr_files_to_stack[final_name_files_correct_order[1]]), crs=4283)
  #if (!is.null(bound_box)) {
  #  loop_raster <- raster::crop(loop_raster, bound_box)
  #}
 # proj4string(loop_raster) <- unref
#  proj_raster <- raster::projectRaster(from = loop_raster, crs = CRS("+init=epsg:4326"), method = "bilinear")
#  proj_raster <- raster::disaggregate(proj_raster, fact=5)
 # list_raster[[1]] <- proj_raster
  no_cores=parallel::detectCores()-1

  cl<-parallel::makeCluster(no_cores, type = "FORK")

  stack_list<-parallel::parLapply(cl,1:length(final_name_files_correct_order),function(i) {
    loop_raster <- raster::raster(paste0("awap_raw_data/", hdr_files_to_stack[final_name_files_correct_order[i]]), crs = CRS("+init=epsg:4283"))
    if (!is.null(bound_box)) {
      loop_raster <- raster::crop(loop_raster, bound_box)
    }
      if(i==1){
        rast_extent <- raster::extent(loop_raster)
        }
    else{
        loop_raster <- raster::setExtent(loop_raster, rast_extent)
        }
        list_raster[[i]] <- loop_raster
    #proj4string(loop_raster) <- unref
    #proj_raster <- raster::projectRaster(from = loop_raster, crs = CRS("+init=epsg:4283"), method = "bilinear")
  })
  parallel::stopCluster(cl)
  awap_stack<-raster::stack(stack_list)
  return(awap_stack)
}
