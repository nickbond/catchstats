#' Code to download monthly AWAP grids to a specified directory
#'
#' @param loc_dir The local directory to which the files will be downloaded and unzipped
#' @return Downloads CSIRO AWAP files to the specified directory ready for further use
#' @note The files will be downloaded to the directory specified (defaults to the
#' current working directory) and unzipped. The function first checks to see which zip
#' files have previously been downloaded, and for this reason, if deleting files to save
#' disk space it is best to delete the unzipped files in the directory "/AWAP/Run26j/FWDis/".
#' @examples
#' awap_download()
#' @export
require("RCurl")


awap_download <- function(loc_dir = NULL) {
  loc_dir <-ifelse(!is.null(loc_dir), loc_dir, getwd())

local_file_names <-list.files(path=loc_dir, pattern = "\\.flt$", recursive = TRUE)

remote_file_names <- getURL("ftp://ftp.eoc.csiro.au/pub/awap/Australia_historical/Run26j/FWDis/", verbose = TRUE, dirlistonly = TRUE, ftp.use.epsv = TRUE)
remote_file_names2 <- getURL("ftp://ftp.eoc.csiro.au/pub/awap/Australia_operational_v26/monthly/", verbose = TRUE, dirlistonly = TRUE, ftp.use.epsv = TRUE)
grid_names <- unlist(strsplit(remote_file_names, "\n"))
grid_names2 <- unlist(strsplit(remote_file_names2, "\n"))

grids_to_dwnld<- setdiff(local_file_names, c(grid_names, grid_names2))
#Download all the files and unzip them

for(i in 1:length(grids_to_dwnld)) {

  download.file(paste("ftp://ftp.eoc.csiro.au/pub/awap/Australia_historical/Run26j/FWDis/", grid_names[i], sep = ""),
                destfile = grid_names[i])

  unzip(grid_names[i]) }

}
