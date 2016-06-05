#' summarise river network character between site pairs
#'
#' @param site1 a vector of sites for which all pairs are sought
#' @param site2 a second vector of sites for which all pairs are sought
#' @param env.vars environmental variables for which summary stats are required
#' @param FUN quoted scalar function to apply to env.vars (e.g. 'sum')
#' @return  a dataframe with site pairs and network characteristics
#' @note ...
#' @examples
#'#data(mwcats)
#'
#'#all_pairs(mwcats$site[1:5])
#'
#' @export


networkstats <- function(site1, site2, FUN = "max", env.vars = NULL) {
    
    # library(utils) library(parallel) library(data.table) library(dplyr)
    
    # all.ds.sites<-list_all_downstream(hierarchy, c(site1,site2)) ds.sites<-unique(c(setdiff(all.ds.sites[[1]], all.ds.sites[[2]]), setdiff(all.ds.sites[[2]],all.ds.sites[[1]])))
    # ds.sites<-unique(c(ds.sites,site1,site2))
    
    # if(env.vars) slope<-hierarchy[match(ds.sites, hierarchy$site), 'slope'] rd<-hierarchy[match(ds.sites, hierarchy$site), 'length']
    
    # sapply(env.vars)
    
    summary <- data.frame(site1 = site1, site2 = site2, max.slope = max(slope, na.rm = T), river.dist = sum(rd, na.rm = T))
    return(summary)
    
}

# clus<-makeCluster(8) clusterExport(clus, c('networkstats', 'list.all.downstream', 'alldownstream', 'hierarchy')) system.time(test<-parRapply(clus, all.pairs, function(x)
# networkstats(x[1],x[2])))
