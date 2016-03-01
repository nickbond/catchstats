#' Identify the next downstream site from a set of candidates
#'
#' @param cat.hier a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchnames a vector of catchment ids for which a a list of downstream catchments
#' ids will be returned.
#' @return a list of downstream catchment ids for each catchment in catchnames
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' Can be used to support further aggregation of environmental variables for sub-catchments downstream
#' of a list of catchments of interest (e.g. for calculating barrier numbers).
#' @examples
#'
#' @export


findnds <- function(cat.hier,catchnames,candidates) {
  y <- cat.hier$nextds[cat.hier$site == upstream]
  if(y %in% candidates & length(y)!=0) {
    return(y) } else{
      y <- cat.hier$nextds[cat.hier$site == upstream]

      test <- NULL
      while(length(test) == 0 ) {
        y <- cat.hier$nextds[cat.hier$site == y]
        test <- candidates %in% y
        test <- test[test]
        if(!length(test)==TRUE) {
          y <- -1
          break
        }
      }
    }
  return(y)
}
