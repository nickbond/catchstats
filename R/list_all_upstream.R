#' Identify all the subcatchments upstream of a list of specified catchments
#'
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchnames a vector of catchment ids for which a a list of upstream catchment
#' ids will be returned.
#' @return a list of upstream catchment ids for each catchment in catchnames
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' Can be used to support further aggregation of environmental variables for sub-catchments downstream
#' of a list of catchments of interest (e.g. for calculating barrier numbers).
#' @examples
#'#'data(mwcats)
#'
#'#find all sites upstream of the first five sites in the catchment list
#'data(mwcats)
#'
#'list_all_upstream(hierarchy = mwcats, catchname = mwcats$site[1:5])
#'
#' @export

list_all_upstream <- function(hierarchy, catchnames) {
    
    all.us.sites <- vector("list", length(catchnames))
    
    for (i in 1:length(catchnames)) {
        us.sites <- allupstream(hierarchy, catchnames[i])
        all.us.sites[[i]] <- us.sites
    }
    return(all.us.sites)
}
