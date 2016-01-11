#' Return a vector of all subcatchments upstream of a specified catchment
#'
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchname a dataframe containing catchment id for which a vector of upstream catchment 
#' ids will be returned.
#' @return a vector of downstream catchment ids
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' When used in conjunction with list_all_upstream it is possible to get a list of catchments upstream 
#' of a vector of sites. This can then be used to support further aggregation of environmental variables
#' for contributing catchments upstream of a list of catchments of interest (for example for aggregating rainfall).
#' @examples
#' 
#' @export

allupstream <- function(hierarchy,catchname){
  if(length(which(hierarchy$site==catchname))>0)
  {
    catchname <- as.vector(catchname)
    allsc <- as.vector(hierarchy$site[hierarchy$nextds==catchname])
    allsc <- allsc[!is.na(allsc)]
    #subcatchments immediately upstream
    nbrnch <- end <- length(allsc)
    #number of branches immediately upstream
    start <- 1
    while(nbrnch>0)
    {
      for(i in start:end)
      {
        allsc <- c(allsc,as.vector(hierarchy$site[hierarchy$nextds==allsc[i]]))
        allsc <- allsc[!is.na(allsc)]
      }
      start <- end + 1
      end <- length(allsc)
      nbrnch <- end - (start - 1) 
    }
    allsc <- c(catchname,allsc)
    allsc
  } else
    cat(paste(catchname,"is not a site listed in the hierarchy table","\n"))
}