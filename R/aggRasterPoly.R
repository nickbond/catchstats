#' function to accumulate or calculate average polygon values within a set of upstream catchment polygons.
#' @note The function can be used to extract values from a raster layer (or stack) or be applied ot existing dataframe of polygon values. For extraction the shapefile and raster stack must be projected in the same projection.
#' @param shpfile The shapefile to be used to intersect with the raster layers (optional)
#' @param rast The raster layer(s) to be intersected
#' @param catid_col The field in the shapefile with the catchment ID values (e.g. 'site', 'SEGMENTNO')
#' @param nextds_col The field in the shapefile indicating the next downstream catchment. Required for identifying catchments to aggregate.
#' @param catarea_col The field containing the catchment areas. This is used for calculating the relative contributing area for weighting values when fun #' ='average'.
#' @param reporting_cats A vector of 'sites' etc. to calculate values for. Otherwise defaults to all fields in catid_col. See optional start and end for #'alternative subsetting approach.
#' @param start the first polygon in the shapefile to be used for intersection
#' Defaults to a value of 1 (the first row).
#' @param end. The last polygon in the shapfile to be intersected.
#' @param loc.cat.df A dataframe containing values for individual subcatchments to aggreagte - produced by running aggRasterPoly.
#' If provided, there is no need to specify the raster layer, although the shapefile is still required to produce the catchment hierarchy.
#' @param fun The aggregation function - 'average', 'accumulate', 'area_sum'. The former would typically be used for variables such as rainfall,
#'  whereas number of survey sites might be summed ('accumulate'), while runoff might be accumulated as runoff volume (mm/km^2 * catchment area)
#'  by taking into account sub-catchment areas using 'area_sum'.
#' @return a dataframe of area-weighted or summed values derived from all sub-catchments contributing to each catchment.
#'
#' @examples
#'
#' Need to complete with appropriate shapefile.
#'
#'
#' @export
#'



aggRasterPoly <- function(shpfile, rast = NULL, catid_col, nextds_col, nextds2_col,catarea_col, reporting_cats = NULL, start = 1, end = nrow(shpfile), loc.cat.df = NULL, fun = NULL) {

  if (is.null(fun)) {
    stop("Please select the catchment aggregation function (\"average\", \"accumulate\" or \"area_sum\")")
  }

  data <- slot(shpfile, "data")

  if (is.null(reporting_cats)) {
    reporting_cats <- data[[catid_col]][start:end]
  }

  hierarchy <- data.frame(site = data[[catid_col]], nextds = data[[nextds_col]], scarea = data[[catarea_col]])
  lst <- list_all_upstream(hierarchy, catchnames = reporting_cats)
  hierarchy_2<-data.frame(site=data[[catid_col]],nextds=data[[nextds2_col]])
  lst_2<-list_all_upstream(hierarchy_2,catchnames = reporting_cats)

  for(i in 1:length(lst)){
    if(length(lst_2[[i]])>1){
      values<-c()
      for(j in 2:length(lst_2[[i]])){
        values<-lst_2[[i]][j]
        lst[[i]]<-append(lst[[i]],values)
        lst[[i]]<- unique(lst[[i]])#added unique.
      }
    }
  }


  if (is.null(loc.cat.df)) {

    loc.cat.df <- extractRasterPoly(shpfile, rast, catid_col = catid_col, start = start, end = end)

  }

  all.agg.output <- matrix(nrow = nrow(loc.cat.df), ncol = length(reporting_cats), dimnames = list(row.names(loc.cat.df), reporting_cats))



  for (j in 1:length(lst)) {
    subs <- na.omit(names(loc.cat.df)[match(lst[[j]], names(loc.cat.df))])
    #subs<-sort(subs,decreasing = FALSE)

    df.subs <- loc.cat.df[, names(loc.cat.df) %in% subs]

    if (unique(lapply(df.subs, is.null)) == TRUE) {
      all.agg.output[, j] <- NA
    } else if (fun == "accumulate") {
      if (!is.vector(df.subs)) {
        # get the sub-catchment areas df.subs.careas<-hierarchy[match(subs, hierarchy$site),'scarea'] df.subs.accum<-t(t(df.subs)*df.subs.careas)
        all.agg.output[, j] <- rowSums(df.subs, na.rm = T)
      } else {
        # df.subs.carea<-hierarchy[match(subs, hierarchy$site),'scarea'] #note not plural as for single catchments only
        all.agg.output[, j] <- unlist(df.subs)
      }
    } else if (fun == "area_sum") {
      if (!is.vector(df.subs)) {
        # get the sub-catchment areas
        df.subs.careas <- hierarchy[match(names(df.subs), hierarchy$site),'scarea']
        # multiply areas by subc values
        df.subs.area.accum <- t(t(df.subs) * df.subs.careas)
        all.agg.output[, j] <- rowSums(df.subs.area.accum, na.rm = T)

      } else {
        df.subs.carea <- hierarchy[match(subs, hierarchy$site), 'scarea']  #Note a single area only here as just 1 catchment.
        all.agg.output[, j] <- unlist(df.subs) * df.subs.carea
      }
    } else if (fun == "average") {
      if (!is.vector(df.subs)) {
        df.subs.careas <- hierarchy[match(names(df.subs), hierarchy$site), "scarea"]
        df.subs.rel.careas <- df.subs.careas/sum(df.subs.careas, na.rm = T)
        df.subs.area.weighted.vals <- t(t(df.subs) * df.subs.rel.careas)
        all.agg.output[, j] <- rowSums(df.subs.area.weighted.vals, na.rm = T)
      } else {
        all.agg.output[, j] <- unlist(df.subs)
      }
    }

  }

  # all.agg.output <- round(all.agg.output)
  return(as.data.frame(all.agg.output))
}
