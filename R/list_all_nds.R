#' Identify the next downstream site for each site in a list of sites.
#'
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchnames a vector of catchment ids (sites) for which a a list of downstream catchments
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
#'#find next downstream sites from a list of candidates for a list of sites
#'data(mwcats)
#'
#'list_all_nds(hierarchy = mwcats[,c(1:2)], catchnames = mwcats$site[1:5],
#'candidates=sample(mwcats$site, size = 4000))
#'
#' @export


list_all_nds <- function(hierarchy, catchnames, candidates) {
names(hierarchy)<-c("site","nextds")

  all.nds.sites<-vector(, length = length(catchnames))

  for (i in seq_along(catchnames)) {
    ds.site<-findnds(hierarchy, catchnames[i],candidates)
    all.nds.sites[i]<-ds.site
  }
  return(all.nds.sites)
}

