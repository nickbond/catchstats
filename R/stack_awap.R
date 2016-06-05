#' Code to build a raster stack from AWAP grids for use in further analyses.
#'
#' @param bbox A list of coordinates defining a subregion for the raster stack
#' @param stack_proj A projection definition for the final projected raster stack
#' @param start_date The first month in the AWAP series to inlcude in the stack. If
#' only interested in recent years, this will greatly improve the efficiciency.
#' @param variable The AWAP variable to extract data for.
#' @param sequence Monthly or Annual series
#' @param stat Build a stack of the raw data or the percentile sequence (by monthly only).
#' @return A raster stack of CSIRO AWAP files, stacked by date.
#' @note The function assumes the awap data files are in the directory raw_awap_data, which is
#' a subdirectory of the working directory from which the function is being run (see download_awap for more details).
#' @examples
#' Define a bounding box for the city of Melbourne, Australia and surrounds
#' melb <- list(x = c(144.0000, 146.5000), y = c(-39.0000, -37.0000))
#' stack_awap(bbox=melb, start_date='20130131', seq="monthly")
#' South-east Queensland
#' seqld <- list(x = c(149, 154.000), y = c(-21.0000, -29.0000))
#' seq_awap <- stack_awap(bbox=seqld, start_date='20130131')
#' writeRaster(seq_awap, filename='seq_awap.grd', bandorder='BIL', overwrite=TRUE)
#' vic <- list(x = c(141.0, 150.2), y = c(-34.0, -39.2))
#' vic_awap <- stack_awap(bbox=vic, start_date='19900131')
#' writeRaster(vic_awap, filename='vic_awap_1990_2014.grd', bandorder='BIL', overwrite=TRUE)
#' @export


stack_awap <- function(bbox = NULL, stack_proj = c("+init=epsg:28355"), start_date = "20120131", variable = "FWDis", sequence = "monthly", stat = "raw") {
    if (is.null(bbox))
        stop("please specify a bounding box or specify 'none' for the whole country")
  #  wd <- getwd()
  #  if (!is.null(loc_dir)) {
   #     setwd(loc_dir)
  #  } else {
   #     setwd(paste0(getwd(), "/awap_raw_data"))
  #  }

    # We want the flt files
    hdr_files <- list.files(path="awap_raw_data/", pattern = "\\.flt$")
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


    # data files of interest now the data files appear to be in units of m/day

    # list the files and sort out correct ordering by date
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



    # save the above , it is the sorted file names save(final_name_files_correct_order, file = 'correct_sorted_file_names.Rdata')

    # get the bounding box?
    bound_box <- NULL
    if (length(bbox) > 1) {
        bound_box <- extent(bbox)
    }
    # Set unprojected reference system WGS84
    unref <- CRS("+init=epsg:4326")

    list_raster <- list()
    # try building the stack now
    start = which(month_name == start_date)
    end = length(final_name_files_correct_order)
    final_name_files_correct_order <- final_name_files_correct_order[start:end]

    for (i in 1:length(final_name_files_correct_order)) {

        # Nick recons the best approach is to 1 read in and clip the raster 2 reproject 3 add to raster stack 4 save raster stack

        # Read in the raster
        loop_raster <- raster(paste0("awap_raw_data/", hdr_files_to_stack[final_name_files_correct_order[i]]))


        # Crop it based on bbox extent (if specified)
        if (!is.null(bound_box)) {
            loop_raster <- crop(loop_raster, bound_box)
        }

        # define WGS84 for cropped raster
        proj4string(loop_raster) <- unref


        # reproject the raster to defined proj rast_proj
        proj_raster <- projectRaster(from = loop_raster, crs = stack_proj, method = "bilinear")



        # best to disaggregate the raster, to make sure that the weights work so that it does not round some weights to zero
        if(i==1) {
          proj_raster <- disaggregate(proj_raster, fact=5)
        } else {
        # rename, might fix names(proj_raster) <- paste('val_D', i, sep ='')

          proj_raster <- resample(proj_raster, list_raster[[i-1]])
        }

        # assign file name file_name <- paste(wd, '/awap_raster/awap_raster', i, '.Rdata', sep = '')

        # save each raster and try later building save(proj_raster, file = file_name) }

        # Try building the stack with all the files
        list_raster[[i]] <- proj_raster

        # for(j in 1:length(final_name_files_correct_order)) {

        # create name and give it null value name_raster <- paste('awap_raster_', j, sep = '') assign(name_raster, NULL)

        # load raster of interest name_to_load <- paste(wd, '/awap_raster/awap_raster', j, '.Rdata', sep = '') load(name_to_load)

        # assign raster to variable assign(name_raster, proj_raster) list_raster[[j]] <- get(name_raster) }
    }

    # build the stack now
    awap_stack <- stack(list_raster)
 #   setwd(wd)
    return(awap_stack)
}


