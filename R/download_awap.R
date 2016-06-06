#' Code to download monthly AWAP grids to a specified directory
#'
#' @return Downloads CSIRO AWAP zip files into the sub-directory awap_zip_files
#' and unzips those files into awap_raw_data for further processing.
#' @note The files will be downloaded to the directories awap_zip files and awap_raw_data
#' within the current working directory. The function first checks to see which zip
#' files have previously been downloaded, and for this reason, if deleting files to save
#' disk space it is best to delete the unzipped files in the directory 'awap_raw_data'.
#' Re-running the function will then unzip the files again after checking for any new
#' data files on the server.
#' @examples
#' download_awap()
#' @export

download_awap <- function() {
   # loc_dir <- ifelse(!is.null(loc_dir), loc_dir, getwd())

    local_file_names <- list.files(path = file.path("awap_zip_files"), pattern = "\\.flt.zip")

    hist_remote_file_names <- RCurl::getURL("ftp://ftp.eoc.csiro.au/pub/awap/Australia_historical/Run26j/FWDis/", verbose = TRUE, dirlistonly = TRUE, ftp.use.epsv = TRUE)
    op_remote_file_names <- RCurl::getURL("ftp://ftp.eoc.csiro.au/pub/awap/Australia_operational_v26/monthly/", verbose = TRUE, dirlistonly = TRUE, ftp.use.epsv = TRUE)
    hist_grid_names <- unlist(strsplit(hist_remote_file_names, "\n"))
    op_grid_names <- unlist(strsplit(op_remote_file_names, "\n"))
    op_grid_names <- op_grid_names[grepl("2015|2016", op_grid_names)]
    op_grid_names <- op_grid_names[grepl(".flt", op_grid_names)]

    hist_grids_to_dwnld <- gsub("\\r", "", setdiff(hist_grid_names, local_file_names))
    op_grids_to_dwnld <- gsub("\\r", "", setdiff(op_grid_names, local_file_names))

    # Download all the files and unzip them
    if (!dir.exists("awap_zip_files")) {
        dir.create("awap_zip_files")
    }

    if(length(hist_grids_to_dwnld>1)) {
    for (i in 1:length(hist_grids_to_dwnld)) {

        download.file(file.path("ftp.eoc.csiro.au","pub","awap","Australia_historical","Run26j","FWDis", hist_grids_to_dwnld[i]), destfile = file.path("awap_zip_files", hist_grids_to_dwnld[i]), method="libcurl")
    }
    }

    if(length(op_grids_to_dwnld>1)) {
    for (i in 1:length(op_grids_to_dwnld)) {
      download.file(file.path("ftp.eoc.csiro.au","pub","awap","Australia_operational_v26","monthly", op_grids_to_dwnld[i]), destfile = file.path("awap_zip_files", op_grids_to_dwnld[i]), method="libcurl")
    }
    }



    updated.local.files <- list.files(path = file.path("awap_zip_files"), pattern = "\\.flt.zip", full.names = TRUE)

    if (!dir.exists("awap_raw_data")) {
        dir.create("awap_raw_data")
    }

    for (i in 1:length(updated.local.files)) {
        unzip(updated.local.files[i], exdir = file.path("awap_raw_data"), junkpaths = TRUE)
    }
}
