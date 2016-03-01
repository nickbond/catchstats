#' function for building a data frame for each site in sitelist
#'
#' @param sitelist a single row from a dataframe with list of sites with corresponding d2ol values
#' @param hierarchy must have fields site, nextds and d2ol (can be < sitelist$d2ol)
#' containing all the data in catchstats for all upstream catchments
#' @param catchstats ?
#' @return?
#' @note?
#' @examples
#'data(mwcats)
#'
#'#to be added.
#'
#' @export

build_catch <- function(sitelist,hierarchy,catchstats)
  #function for building a data frame for each site in sitelist
  #dataframe with list of sites with corresponding d2ol values
  #containing all the data in catchstats for all upstream catchments
  #using hierarchy and the allupstream function.  The dataframes are assembled in a list
{
  sitelist <- sitelist[order(sitelist$site),]
  hierarchy <- hierarchy[order(hierarchy$site),]
  n <- dim(sitelist)[1]
  built.data <- list()
  for(i in 1:n)
  {
    allsc <- allupstream(hierarchy, as.vector(sitelist$site[i]))
    a <- catchstats[catchstats$site %in% allsc,]
    a$d2ol <- hierarchy$d2ol[match(a$site,hierarchy$site)]
    a$d2ol <- a$d2ol - sitelist$d2ol[i]
    if(length(a$d2ol[a$d2ol<0]) > 0){
      nnegs <- sum(a$d2ol<0)
      a$d2ol[a$d2ol<0] <- 0
      cat(paste("Warning. negative d2ol values converted to zero.",
                nnegs, " objects in site",sitelist$site[i],"\n"))
    }
    a$drn2str <- hierarchy$drn2str[match(a$site,hierarchy$site)]
    built.data[[i]] <- a
  }
  names(built.data) <- sitelist$site
  built.data
}
