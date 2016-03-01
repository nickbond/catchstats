#' Identify all the subcatchments downstream of a list of specified catchments
#'
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchnames a vector of catchment ids for which a vector of next downstream catchment
#' ids will be returned.
#' @return a vector of next downstream catchment ids for each catchment in catchnames
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' @examples
#'#'#'data(mwcats)
#'
#'#find all sites downstream of the first five sites in the catchment list
#'data(mwcats)
#'
#'list_all_downstream(hierarchy = mwcats, catchname = mwcats$site[1:5])
#'
#' @export

list_all_downstream<-function(hierarchy, catchnames) {

  all.ds.sites<-vector("list",length(catchnames))

  for (i in 1:length(catchnames))
  {
    ds.sites<-alldownstream(hierarchy,catchnames[i])
    all.ds.sites[[i]]<-ds.sites
  }
  return(all.ds.sites)
}
