#' returns list of subcatchments between 2 subcatchments on the same river
#'
#' @param hierarchy a dataframe containing catchment id and next downstream (nextds) id fields.
#' Hierarchy must have fields 'site' and 'nextds'
#' @param upstream the upstream subcatchment id
#' @param downstream the downstream subcatchment id
#' @param riverdist ?
#' @return Returns list of subcatchments between 2 subcatchments
#' on the same river.
#' @note riverdist, if used, should be the column number of the riverdistance
#' measure in hierarchy.
#' Returns the following error if downstream is not downstream of upstream:
# 'Error in while (y != downstream) { : argument is of length zero'
#' @examples
#'data(mwcats)
#'#function needs checking.
#'#riverreach(hierarchy = mwcats[,c(1:2)], upstream='YARR2415', downstream='YARR001')
#'
#' @export
#'
riverreach <- function(hierarchy, upstream, downstream, riverdist = NULL) {

    x <- y <- upstream
    if (!is.null(riverdist))
        rd <- hierarchy[hierarchy$site == x, riverdist]
    while (y != downstream) {
        y <- hierarchy$nextds[hierarchy$site == y]
        if (identical(y, character(0)) == TRUE) {
            cat("upstream value,", upstream, ", not upstream of downstream value,", downstream, ".\n")
        } else x <- c(x, y)
        if (!is.null(riverdist))
            rd <- c(rd, hierarchy[hierarchy$site == y, riverdist])
    }
    if (!is.null(riverdist))
        x <- data.frame(reach = x, riverdist = rd)
    x
}
