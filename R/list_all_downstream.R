#' Identify all the subcatchments downstream of a list of specified catchments
#' 
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchnames a vector of catchment ids for which a a list of downstream catchments
#' ids will be returned.
#' @return a list of downstream catchment ids for each catchment in catchnames
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' Can be used to support further aggregation of environmental variables for sub-catchments downstream 
#' of a list of catchments of interest (e.g. for calculating barrier numbers).
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