#' function for building a data frame for a single site in sitelist
#'
#' @param sitelist a single row from a dataframe with list of sites with corresponding d2ol values
#' @param hierarchy must have fields site, nextds and d2ol (can be < sitelist$d2ol)
#' containing all the data in catchstats for all upstream catchments
#' @return a dataframe
#' @note using hierarchy and the allupstream function.
#' it assembles a dataframe from the vectors subcID, luno, d2ol, d2str, and fa
#' @examples
#'data(mwcats)
#'
#'#to be added.
#'
#' @export

build_catch_pxls <- function(sitelist,hierarchy)
{
  i <- 1
  allsc <- allupstream(hierarchy, as.vector(sitelist$site[i]))
  pxls <- which(subcID %in% allsc)
  a <- data.frame(site = subcID[pxls], luno = luno[pxls], d2ol = d2ol[pxls],
                  d2str = d2str[pxls], fac = fac[pxls])
  a$subcd2ol <- hierarchy$d2ol[match(a$site, hierarchy$site)]
  if(sum(is.na(a$subcd2ol)) > 0)
    a$subcd2ol[is.na(a$subcd2ol)] <- min(a$subcd2ol, na.rm = TRUE)
  #not ideal, but only a small number of difficult subcs with no stream within cooee
  a$dodgy.pxl <- 0
  a$dodgy.pxl[a$d2ol < a$subcd2ol] <- 1
  a$d2ol <- a$d2ol - sitelist$d2ol[i]
  a$subcd2ol <- a$subcd2ol - sitelist$d2ol[i]
  a <- a[a$d2ol - a$d2str >= 0,]
  a
}
