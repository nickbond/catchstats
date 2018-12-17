#' Identify the next downstream site from a set of candidates
#'
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields
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
#'findnds(hierarchy = mwcats[,c(1:2)], catchname = mwcats$site[1], candidates=mwcats$site)
#'
#' @export

findnds <- function(hierarchy, catchname, candidates) {
    names(hierarchy) <- c("site", "nextds")
    y <- hierarchy$nextds[hierarchy$site == catchname]
    if (y %in% candidates & length(y) != 0) {
        return(y)
    } else {
        y <- hierarchy$nextds[hierarchy$site == catchname]
        
        test <- NULL
        while (length(test) == 0) {
            y <- hierarchy$nextds[hierarchy$site == y]
            test <- candidates %in% y
            test <- test[test]
            if (!length(test) == TRUE) {
                y <- -1
                break
            }
        }
    }
    return(y)
}
