sum_allupstream <- function(hierarchy, catchname, envar) {
  
  names(hierarchy)[1:2] <- c("site", "nextds")
  
  if (length(which(hierarchy$site == catchname)) > 0) {
    catchname <- as.vector(catchname)
    allsc <- as.vector(hierarchy$site[hierarchy$nextds == catchname])
    allsc <- allsc[!is.na(allsc)]
    # subcatchments immediately upstream
    nbrnch <- end <- length(allsc)
    # number of branches immediately upstream
    start <- 1
    while (nbrnch > 0) {
      for (i in start:end) {
        allsc <- c(allsc, as.vector(hierarchy$site[hierarchy$nextds == allsc[i]]))
        allsc <- allsc[!is.na(allsc)]
      }
      start <- end + 1
      end <- length(allsc)
      nbrnch <- end - (start - 1)
    }
    allsc <- c(catchname, allsc)
    
    sum_allsc <- sum(hierarchy[[envar]][which(hierarchy[["site"]] %in% allsc)], na.rm=TRUE)
    sum_allsc
  } else cat(paste(catchname, "is not a site listed in the hierarchy table", "\n"))
}
