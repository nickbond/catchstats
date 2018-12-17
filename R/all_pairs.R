#' Create a dataframe with all pairs in a vector of sites
#'
#' @param sites a vector of sites for which all pairs are sought
#' @return a dataframe of site pairs (2 columns denoted site1 and site2)
#' @note Produces the input for calculating network statistics
#' @examples
#'data(mwcats)
#'
#'all_pairs(mwcats$site[1:5])
#'
#' @export

all_pairs <- function(sites) {
    out <- as.data.frame(t(utils::combn(sites, 2, simplify = TRUE)))
    # names(out)<-c('site1', 'site2')
    return(out)
}
