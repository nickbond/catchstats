#' Identify the next downstream site for each site in a list of sites.
#'
#' @param cat.hier a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchnames a vector of catchment ids (sites) for which a a list of downstream catchments
#' ids will be returned.
#' @param candidates a vector of candidate catchment ids
#' @return a list of downstream catchment ids for each catchment in catchnames
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' Can be used to support further aggregation of environmental variables for sub-catchments downstream
#' of a list of catchments of interest (e.g. for calculating barrier numbers).
#' @examples
#'
#' @export


list_all_nds <- function(cat.hier, catchnames, candidates) {

  all.nds.sites<-vector(, length = length(catchnames))

  for (i in seq_along(catchnames)) {
    ds.site<-findnds(cat.hier, sites[i],candidates)
    all.nds.sites[i]<-ds.site
  }
  return(all.nds.sites)
}

