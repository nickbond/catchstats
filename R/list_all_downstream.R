#' Identify all the subcatchments downstream of a list of specified catchments
#'
#' @param cat.hier a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchnames a vector of catchment ids for which a vector of next downstream catchment
#' ids will be returned.
#' @return a vector of next downstream catchment ids for each catchment in catchnames
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' @examples
#'
#' @export

list_all_downstream<-function(cat.hier, catchnames) {

  all.ds.sites<-vector("list",length(catchnames))

  for (i in 1:length(catchnames))
  {
    ds.sites<-alldownstream(cat.hier,catchnames[i])
    all.ds.sites[[i]]<-ds.sites
  }
  return(all.ds.sites)
}
