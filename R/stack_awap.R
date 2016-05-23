#' Code to build a raster stack from AWAP grids for use in further analyses.
#'
#' @param loc_dir The local directory in wich the AWAP zip files are stored
#' If not specified, the function looks for the folder "/AWAP/Run26j/FWDis/"
#' within the local working directory.
#' @param bbox A list of coordinates defining a subregion for the raster stack
#' @param stack_proj A projection definition for the final projected raster stack
#' @return A raster stack of CSIRO AWAP files, stacked by date.
#' @note The individual zip files are stacked from a directory inside the working
#' directory specfiied above (see AWAP_download for more details).
#' @examples
#' Define a bounding box for the city of Melbourne, Australia and surrounds
#' melb <- list(x = c(144.0000, 146.5000), y = c(-39.0000, -37.0000))
#' stack_awap(bbox=melb, start="20130131")
#' South-east Queensland
#' seqld <- list(x = c(149, 154.000), y = c(-21.0000, -29.0000))
#' seq_awap <- stack_awap(bbox=seq, start="20130131")
#' writeRaster(seq_awap, filename="seq_awap.grd", bandorder='BIL', overwrite=TRUE)
#' vic <- list(x = c(141.0, 150.2), y = c(-34.0, -39.2))
#' vic_awap <- stack_awap(bbox=vic, start="19900131")
#' writeRaster(vic_awap, filename="vic_awap_1990_2014.grd", bandorder='BIL', overwrite=TRUE)
#' @export
require(rgdal)
require(raster)
require(stringr)
require(maptools)
require(lubridate)

stack_awap<-function(loc_dir=NULL, bbox=NULL, stack_proj=c("+init=epsg:28355"), start_date="20120131") {
  if(is.null(bbox)) stop("please specify a bounding box or specify 'none' for the whole country")
  wd<-getwd()
  if(!is.null(loc_dir)) {
    setwd(loc_dir)
  } else {
    setwd(paste0(getwd(), "/AWAP/Run26j/FWDis"))
  }

#We want the flt files, with the file beginning with mth_FWDis_
get_hdr_files <- list.files(pattern = "\\.flt$")
#Get start of file strings, and return just the monthly files
string_start_file <- substr(get_hdr_files, 1, 10 )
pos_files_int <- which(string_start_file == "mth_FWDis_")

#data files of interest now
#the data files appear to be in units of m/day
mon_run_m_d_file <- get_hdr_files[pos_files_int]

#list the files and sort out correct ordering by date
month_name <- substr(mon_run_m_d_file, 11, 18)
final_name_files_correct_order <- paste("mth_FWDis_", sort(month_name, decreasing = FALSE), ".flt", sep = "")

#save the above , it is the sorted file names
#save(final_name_files_correct_order, file = "correct_sorted_file_names.Rdata")

#get the bounding box?
bound_box<-NULL
if(length(bbox)>1) {
bound_box <- extent(bbox)
}
#Set unprojected reference system WGS84
unref <- CRS("+init=epsg:4326")

list_raster <- list()
#try building the stack now
start=which(sort(month_name, decreasing=FALSE)==start_date)
end=length(final_name_files_correct_order)
final_name_files_correct_order <- final_name_files_correct_order[start:end]

for(i in 1:length(final_name_files_correct_order)) {

  #Nick recons the best approach is to
  #1 read in and clip the raster
  #2 reproject
  #3 add to raster stack
  #4 save raster stack

  #Read in the raster
  loop_raster <- raster(final_name_files_correct_order[i])


  #Crop it based on bbox extent (if specified)
  if(!is.null(bound_box)) {
  loop_raster <- crop(loop_raster, bound_box)
  }

  #define WGS84 for cropped raster
  proj4string(loop_raster) <- unref


  #reproject the raster to defined proj rast_proj
  proj_raster <- projectRaster(loop_raster, crs = stack_proj, method = "ngb")


  #best to disaggregate the raster, to make sure that the weights work
  #so that it does not round some weights to zero
  proj_raster <- disaggregate(proj_raster, 5)

  #rename, might fix
 # names(proj_raster) <- paste("val_D", i, sep ="")

  #assign file name
#  file_name <- paste(wd, "/awap_raster/awap_raster", i, ".Rdata", sep = "")

  #save each raster and try later building
#  save(proj_raster, file = file_name)  }

#Try building the stack with all the files
list_raster[[i]]<-proj_raster

#for(j in 1:length(final_name_files_correct_order)) {

  #create name and give it null value
 # name_raster <- paste("awap_raster_", j, sep = "")
#  assign(name_raster, NULL)

  #load raster of interest
 # name_to_load <- paste(wd, "/awap_raster/awap_raster", j, ".Rdata", sep = "")
#  load(name_to_load)

  #assign raster to variable
 # assign(name_raster,  proj_raster)
 # list_raster[[j]] <- get(name_raster)   }
}

#build the stack now
awap_stack <- stack(list_raster)
setwd(wd)
return(awap_stack)
}

