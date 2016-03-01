#' Create a list of all reaches between two sites in a river network (inclusive of those sites)
#'
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields.
#' @param site1 a site in a river network
#' @param site2 a site in the same river network as site1 (can be up or downstream)
#' @note Produces a vector of sites between the site pair
#' @examples
#' data(mwcats)
#'
#' #create a dataframe of pair reaches
#' pairReaches(hierarchy=mwcats[,c(1:2)], mwcats$site[1], mwcats$site[5])
#'
#' #create a dataframe with all pairs from a vector of sites
#' df<- all_pairs(mwcats$site[1:5])
#'
#' #create a list of sites between each site pair
#' apply(df,1 , function(x) pairReaches(hierarchy=mwcats[,c(1:2)], x[[1]],x[[2]]))
#'
#' @export

pairReaches<-function(hierarchy, site1, site2) {
  sites<-c(site1, site2)
  all.ds.sites<-list_all_downstream(hierarchy, sites)
  ds.sites<-unique(c(setdiff(all.ds.sites[[1]], all.ds.sites[[2]]), setdiff(all.ds.sites[[2]],all.ds.sites[[1]])))
  ds.sites<-unique(c(ds.sites,site1,site2))
  return(ds.sites)
}
