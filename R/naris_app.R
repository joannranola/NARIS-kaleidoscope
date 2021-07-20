#-------------------------------------------------------------------------------
# Copyright © 2021 the Alliance for Sustainable Energy, LLC, All Rights Reserved
#------------------------------------------------------------------------------- 

#----------------------------------------------------------------------------
#' R version: 4.0.3
#' Load the required R packages
options("rgdal_show_exportToProj4_warnings"="none")
library(circlize)
library(dplyr)
library(devtools)
library(data.table)
library(gganimate)
library(ggplot2)
library(ggmap)
library(gifski)
library(igraph)
library(magrittr)
library(maps)
library(maptools)
library(MASS)
library(NightDay)
library(raster)
library(rgdal)
library(rgeos)
library(RColorBrewer)
library(scales)
library(shape)
library(sf)
library(sp)
library(tidyr)

#----------------------------------------------------------------------------
#' Clean the environment
rm(list=ls())

#----------------------------------------------------------------------------
#' Define scenario names in alphabetical order
#' Limit the scenario names to at most 5 letters, if possible
Scenario1 <- "Base"
Scenario2 <- "Scenario2"
Scenario3 <- "Scenario3"
Scenario4 <- "Scenario4"
scenarios <- data.frame(Scenario1, Scenario2, Scenario3, Scenario4)

#----------------------------------------------------------------------------
#' Enter the start and end datetime
t0 <- as.POSIXlt("2024-06-27 05:00:00", tz="EST")
tn <- as.POSIXlt("2024-06-27 05:00:00", tz="EST")

#----------------------------------------------------------------------------
#' Enter the scenario and resource types for the map
scenario <- Scenario1
types <- c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment")#c("Coal", "Hydro", "Gas", "Fuel_Oil", "Biomass", "Solar", "Wind", "Pumped_Storage", "Curtailment")

#----------------------------------------------------------------------------
#' Include Mexico? Yes or No
Include_Mexico <- "Yes"

#----------------------------------------------------------------------------
#' Source the vn_kaleidoscope package
base.dir <- getwd()
source(file.path(base.dir, "/R/naris_kaleidoscope.R"))
#' End
