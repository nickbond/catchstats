<!-- README.md is generated from README.Rmd. Please edit that file -->
catchstats
==========

catchstats includes a number of functions written by Chris Walsh and Nick Bond for working with nested catchment or river-network data tables, to identify all upstream areas, all downstream areas, segments or sub-catchments between catchment pairs etc. There are also functions for intersecting raster and polygon layers to aggregate values downstream through a catchment. All of these functions require a dataframe that includes a unique catchment identifier field (column) and a field (column) identifying the next downstream catchment identity. Together these are referred to as a 'hierarchy table', and are included with many DEM derived stream networks. There are also several functions for downlaoding and manipulating data from the Australian Water Availability Project (AWAP).

Installation
============

To install run the following code:

      # install devtools pacakge
      install.packages(c("devtools"))

      # install catchstats package
        devtools::install_github("nickbond/catchstats")

        # Remove the package zip after installation
        unlink("catchstats.zip")
        
        #load library
        library(catchstats)

Example
=======

\#To be completed

<!--  The code below produces the following plot for Cooper Creek, a highly ephemeral river in western Queensland, Australia. The function uses geom_raster() from the ggplot2 package. Addtional options are available (see ?ctf_heatmap for details). -->
<!-- ``` -->
<!--  library(hydrostats) -->
<!--  library(hydroplots) -->
<!--  data(Cooper) -->
<!--  ctf_heatmap(Cooper) -->
<!-- ```  -->
<!-- ![Alt tag](https://github.com/nickbond/hydroplots/raw/master/ctf_heatmap.png "CTF Heatmap") -->
Developer
=========

Nick Bond <n.bond@latrobe.edu.au>
