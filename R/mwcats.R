#' Melbourne stream catchment variables
#'
#' A dataset containing the catchment id and other attributes for 12319 sub-catchments
#' around the Melbourne region in southeastern Australia.
#'
#' @format A data frame with 12319 rows and 10 variables:
#'\describe{
#'\item{site}{site names}
#'\item{nextds}{next dowsntream site}
#'\item{scstrlen}{local subcatchment stream lengths}
#'\item{catarea}{upstream catchment area}
#'\item{elevation}{mean subcatchment elevation}
#'\item{mean.ann.q.mm}{mean annual runoff in mm (Q/catchment area)}
#'\item{att.forest}{weighted forest cover}
#'\item{dai9}{\% connected impervious surfaces in the catchment}
#'\item{dai92030}{forecast dai9 for 2030}
#'\item{stranntemp}{average annaul air temp for each subcatchment (degC)}
#'\item{rdi}{river disturbance index}
#'\item{strahler}{strahler stream order}
#'}
#' @source {Walsh, C.J. and Webb, J.A. (2013).  Predicting stream macroinvertebrate assemblage composition
#' as a function of land use, physiography and climate: a guide for strategic planning for river and water
#' management in the Melbourne Water management region.  Melbourne Waterway Protection and Restoration
#' Science-Practice Partnership Report 13-1. Department of Resource Management and Geography,
#' The University of Melbourne. October 2013.}
"mwcats"
