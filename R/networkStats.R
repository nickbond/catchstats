#' summarise river network character between site pairs
#'
#' @param site1 a vector of sites for which all pairs are sought
#' @param site2 a second vector of sites for which all pairs are sought
#' @param cat_data the name of the dataframe containing the catchment data
#' @param catid_col the column containing the catchment ids
#' @param nextds_col the column containing the next downstream catchment ids
#' @param env_vars quoted vector of environmental variables for which summary stats are required
#' @param FUN quoted scalar function to apply to env.vars (e.g. 'sum')
#' @param ... additional arguments to FUN
#' @return  a dataframe with site pairs and network characteristics
#' @note ...
#' @examples
#'data(mwcats)
#'
#'all_pairs(mwcats$site[1:5])
#'
#' @export


 networkstats<-function(site1,site2, cat_data, catid_col, nextds_col, env_vars, FUN, ...) {
   hierarchy <- data.frame(site = cat_data[[catid_col]], nextds = cat_data[[nextds_col]])

   all.ds.sites<-list_all_downstream(hierarchy, c(site1,site2))
   ds.sites<-unique(c(setdiff(all.ds.sites[[1]], all.ds.sites[[2]]), setdiff(all.ds.sites[[2]],all.ds.sites[[1]])))
   ds.sites<-unique(c(ds.sites,site1,site2))

   df<- cat_data[match(ds.sites, hierarchy[['site']]),][env_vars]

   out<-data.frame(site1=site1, site2=site2, t(apply(df, 2, FUN, ...)))

   return(out)

 }

