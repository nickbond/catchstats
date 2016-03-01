#' Return a vector of all subcatchments downstream of a specified catchment

#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields
#' @param catchname a dataframe containing catchment id for which a vector of downstream catchment
#' ids will be returned.
#' @return a vector of downstream catchment ids
#' @note Function depends on the next downstream field in a stream network 'hierarchy' table (dataframe).
#' When used in conjunction with list_all_downstream it is possible to get a list of catchments downstream
#' of a vector of sites. This can then be used to support further aggregation of environmental variables
#' for sub-catchments downstream of a list of catchments of interest (e.g. for calculating barrier numbers).
#' @examples
#'#'data(mwcats)
#'
#'#find all sites downstream of the first site in the catchment list
#'data(mwcats)
#'
#'alldownstream(hierarchy = mwcats, catchname = mwcats$site[1])
#' @export

alldownstream <- function(hierarchy, catchname){
  if(length(which(hierarchy$site==catchname))>0)
  {
    catchname <- as.vector(catchname)
    allsc <- as.vector(hierarchy$nextds[hierarchy$site==catchname])
    allsc <- allsc[!is.na(allsc)]
    #subcatchments immediately upstream
    nbrnch <- end <- length(allsc)
    #number of branches immediately upstream
    start <- 1
    while(nbrnch > 0 & !-1 %in% allsc)
    {
      for(j in start:end)
      {
        allsc <- c(allsc,as.vector(hierarchy$nextds[hierarchy$site == allsc[j]]))
        allsc <- allsc[!is.na(allsc)]
      }
      start <- end + 1
      end <- length(allsc)
      nbrnch <- end - (start - 1)
    }
    allsc <- c(catchname,allsc)
    allsc <- allsc[allsc != -1]
    allsc
  } else
    cat(paste(catchname,"is not a site listed in the hierarchy table","\n"))
}
