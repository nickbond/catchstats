#' Calculate full catchment statistics by summing field(s) value(s) for all upstream catchments
#'
#' @param sitelist a vector of sites for which compiled statistics are to be calculated.
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields.
#' @param subcatchment.data a vector or dataframe containing variables for which statistics are sought.
#' @param FUN an unquoted scalar function used to summarise each field (e.g. min, mean, max).
#' @param ... further arguments passed to or from other methods.
#' @return a vector or dataframe of aggregate values
#' @note function to calculate full catchment statistics by summing each field
#' of the table (or vector) subcatchment.data (length = no. of rows in hierarchy)
#' For each site in sitelist, statistics as per scalar function FUN
#' for all upstream catchments are deriveded using hierarchy in the
#' function allupstream.
#' @examples
#'data(mwcats)
#'
#'catch_stats(sitelist=mwcats$site[1:5], hierarchy = mwcats[,c(1:2)],
#'subcatchment.data=mwcats$scstrlen, FUN=sum)
#'
#' @export

catch_stats <- function(sitelist,hierarchy,subcatchment.data,FUN,...) {
  names(hierarchy)<-c("site", "nextds")
  tot <- length(sitelist); nc <- 1
  if(!is.vector(subcatchment.data))
    nc <- dim(subcatchment.data)[2]
  out <- data.frame(site = sitelist, matrix(NA,nrow=tot,ncol=nc))
  if(nc == 1)
    names(out)[2] <- "catchment.data"
  else
    names(out)[2:(nc+1)] <- names(subcatchment.data)
  for(i in 1:tot){
    #    write.csv(sitelist[1:i], "progress.csv")
    cat(paste(sitelist[i],"\n"))
    allsci <- allupstream(hierarchy,as.vector(sitelist[i]))
    if(!is.vector(subcatchment.data))
    {
      allscii <- subcatchment.data[hierarchy$site %in% allsci,]
      out[i,2:(nc+1)] <- as.vector(apply(allscii,2,FUN,...))
    } else
      out[i,2] <- FUN(subcatchment.data[hierarchy$site %in% allsci],...)
  }
  out
}
