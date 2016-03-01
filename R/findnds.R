#' Identify the next downstream site from a set of candidates
#'
#' @param cat.hier a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchname a catchment id for which a a list of downstream catchments
#' ids will be returned.
#' @param candidates a vector of candidate catchment ids. Note this need not be a complete hierarchy
#' @return a list of downstream catchment ids for each catchment in catchnames. Returns -1 where there
#' is no downstream site
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' Can be used to support further aggregation of environmental variables for sub-catchments downstream
#' of a list of catchments of interest (e.g. for calculating barrier numbers).
#' @examples
#'data(mwcats)
#'
#'#find site next downstream of the first site in the catchment list
#'
#'findnds(cat.hier = mwcats[,c(1:2)], catchname = mwcats$site[1], candidates=mwcats$site)
#'
#' @export

findnds <- function(cat.hier,catchname,candidates) {
  names(cat.hier)<-c("site", "nextds")
  y <- cat.hier$nextds[cat.hier$site == catchname]
  if(y %in% candidates & length(y)!=0) {
    return(y) } else{
      y <- cat.hier$nextds[cat.hier$site == catchname]

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
