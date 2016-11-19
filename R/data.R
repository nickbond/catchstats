#' Physiographic and climatic catchment variables
#'
#' A dataset containing catchment variables for catchments around Melbourne, Australia.
#'
#' @format A data frame with 12319 rows and 12 variables:
#' \describe{
#'   \item site. Site code
#'   \item nextds. Next downstream site code
#'   \item scstrlen. Subcatchment stream length (km)
#'   \item catarea. subcatchment area (km2)
#'   \item elevation. mean subcatchment elevation
#'   \item mean.ann.q.mm. mean annual runoff (mm)
#'   \item attt.forest. Attenuated Forest Cover (forest cover; 0-1, weigthed by proximity to streamlines)
#'   \item dai9. Attenuated impervious surface cover
#'   \item dai92030. projected attenuated impervious surface cover for 2030
#'   \item stranntemp. Annual stream temperature
#'   \item rdi. River disturbance index
#'   \item strahler. Strahler stream order.
#' }
#'
#' @docType data
#' @keywords datasets
#' @name mwcats
#' @usage data(mwcats)
#' @format A data frame with 12319 rows and 10 variables
NULL
