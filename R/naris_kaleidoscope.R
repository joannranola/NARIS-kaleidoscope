#-------------------------------------------------------------------------------
# Copyright © 2021 the Alliance for Sustainable Energy, LLC, All Rights Reserved
#------------------------------------------------------------------------------- 

#----------------------------------------------------------------------------
#' Define the directories
data.dir <- file.path(base.dir, "data")
R.dir <- file.path(base.dir, "R")
animation.dir <- file.path(base.dir, "animation")

#----------------------------------------------------------------------------
#' Call the functions in kaleidoscope package
source(file.path(R.dir, "kaleidoscope.R"))

#----------------------------------------------------------------------------
#'Call the functions in vn plots
source(file.path(R.dir, "naris_plots.R"))

#----------------------------------------------------------------------------
#' Load time-series generation, dispatch stack, and interface data
naris_generation <- read.csv(file.path(data.dir,'naris_generation.csv'), header = T)
naris_dispatch_stack_all <- read.csv(file.path(data.dir,'naris_dispatch_stack.csv'), header = T)
naris_dispatch_stack_no_mexico <- naris_dispatch_stack_all
naris_dispatch_stack_no_mexico <- subset(naris_dispatch_stack_no_mexico, 
                                         !(naris_dispatch_stack_no_mexico$zone %in% c("Mexico")))
#' Include Mexico?
naris_dispatch_stack <- if(Include_Mexico=="Yes"){
  naris_dispatch_stack <- naris_dispatch_stack_all} else{
    naris_dispatch_stack <- naris_dispatch_stack_no_mexico
  }
naris_dispatch_stack_all <- NULL
naris_dispatch_stack_no_mexico <- NULL
naris_netinterchange <- read.csv(file.path(data.dir,'naris_net_interchange.csv'), header = T)
naris_generation$time = as.POSIXlt(naris_generation$time, format='%Y-%m-%d %H:%M:%S', tz="EST")
naris_dispatch_stack$time = as.POSIXlt(naris_dispatch_stack$time, format='%Y-%m-%d %H:%M:%S', tz="EST")
naris_netinterchange$time = as.POSIXlt(naris_netinterchange$time, format='%Y-%m-%d %H:%M:%S', tz="EST")
naris_generation <- filter(naris_generation, time >= t0 & time <= tn)
naris_dispatch_stack <- filter(naris_dispatch_stack, time >= t0 & time <= tn)
naris_netinterchange <- filter(naris_netinterchange, time >= t0 & time <= tn)
#' Selector is added for filter option
source2sink_selector_all <- read.csv(file.path(data.dir,'source2sink_selector.csv'), header = T)
#' Include Mexico?
source2sink_selector_no_mexico <- source2sink_selector_all
source2sink_selector_no_mexico$Include <- ifelse(source2sink_selector_no_mexico$Source2Sink 
                                                 %in% c("ERCOT - Mexico", "Southwest - Mexico", "California - Mexico"), "No", "Yes")
source2sink_selector <- if(Include_Mexico=="Yes"){
  source2sink_selector <- source2sink_selector_all} else{
    source2sink_selector <- source2sink_selector_no_mexico
  }
source2sink_selector_all <- NULL
source2sink_selector_no_mexico <- NULL
naris_netinterchange <- left_join(naris_netinterchange, source2sink_selector, by="Source2Sink")
naris_netinterchange <- subset(naris_netinterchange, naris_netinterchange$Include == "Yes")
naris_netinterchange$Include <- NULL
naris_netinterchange <- na.omit(naris_netinterchange)

#----------------------------------------------------------------------------
#' Transform time-series interface data
naris_netinterchange$Source <- unlist(lapply(strsplit(as.character(naris_netinterchange$Source2Sink),' '), function(l) { l[1] }))
naris_netinterchange$Sink <- unlist(lapply(strsplit(as.character(naris_netinterchange$Source2Sink),' '), function(l) { l[3] }))
naris_netinterchange$Sink2Source <- paste(naris_netinterchange$Sink,"-",naris_netinterchange$Source)
naris_netinterchange$Source2Sink <- ifelse(naris_netinterchange$value>0, naris_netinterchange$Sink2Source, naris_netinterchange$Source2Sink)
naris_netinterchange$value <- ifelse(naris_netinterchange$value>0, -naris_netinterchange$value, naris_netinterchange$value)
naris_netinterchange$Source <- NULL
naris_netinterchange$Sink <- NULL
naris_netinterchange$Sink2Source <- NULL

#----------------------------------------------------------------------------
#' Load and define map data
naris_regions_1 <- st_read(file.path(data.dir,'shapefiles/NA_PCA_Map.shp'))
naris_regions_1 <- st_transform(naris_regions_1, "+proj=longlat +datum=WGS84 +no_defs")
naris_regions_all <- as_Spatial(st_geometry(naris_regions_1), IDs = as.character(1:nrow(naris_regions_1)))
naris_regions_df <- naris_regions_1
naris_regions_df$geometry <- NULL
naris_regions_df <- as.data.frame(naris_regions_df)
naris_regions_all <- SpatialPolygonsDataFrame(naris_regions_all, data = naris_regions_df)
# class(naris_regions_all)
# head(naris_regions_all@data)
naris_regions_all <- naris_regions_all["PlainInt_2"]
naris_regions_no_mexico <- subset(naris_regions_all, naris_regions_all$PlainInt_2 %in% c("CAN", "USA"))
#' Include Mexico?
naris_regions <- if(Include_Mexico=="Yes"){
  naris_regions <- naris_regions_all} else{
    naris_regions <- naris_regions_no_mexico
  }
naris_regions_all <- NULL
naris_regions_no_mexico <- NULL

#----------------------------------------------------------------------------
#' Load generator data
naris_gen_all <- read.csv(file.path(data.dir,'naris_gen.csv'), header = T)
naris_gen_no_mexico <- subset(naris_gen_all, !(naris_gen_all$Node_Region %in% c("Mexico")))
#' Include Mexico?
naris_gen <- if(Include_Mexico=="Yes"){
  naris_gen <- naris_gen_all} else{
    naris_gen <- naris_gen_no_mexico
  }
naris_gen_all <- NULL
naris_gen_no_mexico <- NULL
naris_generators <- naris_gen
naris_generators$Generator_Name <- naris_generators$Generator_Group_Name
naris_generators <- generators <- naris_generators[!duplicated(naris_gen),]

#----------------------------------------------------------------------------
#' Group the generation by Generator_Group_Name from naris_gen
#' Grouping in this setup is based on resource type, lat, and lon
naris_generation <- merge(naris_generation, naris_gen, by=c('Generator_Name'))
naris_generation <- naris_generation %>% 
  group_by(Generator_Group_Name, time, scenario) %>% 
  summarise(Generator_Group_Name, time=time, power=sum(power), scenario=scenario)
naris_generation <- naris_generation[!duplicated(naris_generation),]
names(naris_generation)[names(naris_generation)=="Generator_Group_Name"] <- "Generator_Name"
naris_generation <- subset(naris_generation, power>0)
#' Generation = 0
naris_generation_zero <- naris_generation
naris_generation_zero$power <- 0

#----------------------------------------------------------------------------
#' Load the centroids by zone
naris_centroids_all <- read.csv(file.path(data.dir,'naris_centroids.csv'), header = T)
naris_centroids_no_mexico <- naris_centroids_all
#' Include Mexico?
naris_centroids_no_mexico <- subset(naris_centroids_no_mexico, 
                                    !(naris_centroids_no_mexico$ISO %in% c("Mexico")))
naris_centroids <- if(Include_Mexico=="Yes"){
  naris_centroids <- naris_centroids_all} else{
    naris_centroids <- naris_centroids_no_mexico
  }
naris_centroids_all <- NULL
naris_centroids_no_mexico <- NULL
naris_centroids$lat <- ifelse(naris_centroids$ISO == "MH", naris_centroids$lat+3, naris_centroids$lat)
naris_centroids$lat <- ifelse(naris_centroids$ISO == "IESO", naris_centroids$lat+5, naris_centroids$lat)
naris_centroids$lat <- ifelse(naris_centroids$ISO == "HQ", naris_centroids$lat+5, naris_centroids$lat)
naris_centroids$lat <- ifelse(naris_centroids$ISO == "MISO", naris_centroids$lat+3, naris_centroids$lat)
naris_centroids$lon <- ifelse(naris_centroids$ISO == "IESO", naris_centroids$lon-3, naris_centroids$lon)
naris_centroids$lon <- ifelse(naris_centroids$ISO == "MISO", naris_centroids$lon-4, naris_centroids$lon)
naris_layout <- layout <- as.matrix(naris_centroids[,2:3])
naris_layout <- as.matrix(naris_centroids[,2:3])
naris_verts <- sapply(naris_centroids$ISO, toString)

#----------------------------------------------------------------------------
#' Define colors by resource type
naris_colors <- data.table::data.table(type=c("Nuclear", "Coal", "Hydro", "Gas", "Oil","Wind", "Other", "Storage", "Solar", "Geothermal", "CSP", "Biomass", "Curtailment"),
                                       color=c("#b22222","#333333", "#add8e6","#6e8b3d","orchid4","#4f94cd", "#AAAAAA", "#4444dd", "#ffc125", "khaki1", "darkorange2","mediumpurple2","#E1E1E1"))

#----------------------------------------------------------------------------
# Define the zones (or regions)
naris_iso <-  naris_centroids$ISO

#----------------------------------------------------------------------------
#' Create animation sub-folder
dir.create(file.path(animation.dir, format(Sys.time(), "%F %H-%M")))
animation1.dir <- file.path(animation.dir, format(Sys.time(), "%F %H-%M"))

#----------------------------------------------------------------------------
#' Run a map series of one scenario
cat("Running map series of one scenario...")
dir.create(file.path(animation1.dir, paste0("map_",scenario)))
map.dir <- file.path(animation1.dir, paste0("map_",scenario))
draw_naris_map_series(t0, tn, prefix=scenario, scenario=scenario, types=types)
png_files <- list.files(map.dir, pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = file.path(map.dir,paste0("naris_map_series_",scenario,".gif")), width = 1920, height = 1080, delay = 0.25)

#----------------------------------------------------------------------------
#' Run a chord and dispatch series of one scenario
cat("Running chord and dispatch series of one scenario...")
dir.create(file.path(animation1.dir, paste0("chord_dispatch_",scenario)))
chord_dispatch.dir <- file.path(animation1.dir, paste0("chord_dispatch_",scenario))
draw_naris_chord_dispatch_series(t0, tn, scenario=scenario, prefix=scenario)
png_files <- list.files(chord_dispatch.dir, pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = file.path(chord_dispatch.dir,paste0("naris_chord_dispatch_series_",scenario,".gif")), width = 1920, height = 1080, delay = 0.25)

#----------------------------------------------------------------------------
#' Run a map, with generation and net interchange, and dispatch series of one scenario
cat("Running map, with generation and net interchange, and dispatch series of one scenario...")
dir.create(file.path(animation1.dir, paste0("map_dispatch1_",scenario)))
map_dispatch1.dir <- file.path(animation1.dir, paste0("map_dispatch1_",scenario))
draw_naris_map_dispatch1_series(t0, tn, scenario=scenario, types=types, prefix=scenario)
png_files <- list.files(map_dispatch1.dir, pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = file.path(map_dispatch1.dir,paste0("naris_map_dispatch1_series_",scenario,".gif")), width = 1920, height = 1080, delay = 0.25)

#----------------------------------------------------------------------------
#' Run a map, with generation only, and dispatch series of one scenario
cat("Running map, with generation only, and dispatch series of one scenario...")
dir.create(file.path(animation1.dir, paste0("map_dispatch2_",scenario)))
map_dispatch2.dir <- file.path(animation1.dir, paste0("map_dispatch2_",scenario))
draw_naris_map_dispatch2_series(t0, tn, scenario=scenario, types=types, prefix=scenario)
png_files <- list.files(map_dispatch2.dir, pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = file.path(map_dispatch2.dir,paste0("naris_map_dispatch2_series_",scenario,".gif")), width = 1920, height = 1080, delay = 0.25)

#----------------------------------------------------------------------------
#' Run a map, with net interchange only, and dispatch series of one scenario
cat("Running map, with net interchange only, and dispatch series of one scenario...")
dir.create(file.path(animation1.dir, paste0("map_dispatch3_",scenario)))
map_dispatch3.dir <- file.path(animation1.dir, paste0("map_dispatch3_",scenario))
draw_naris_map_dispatch3_series(t0, tn, scenario=scenario, types=types, prefix=scenario)
png_files <- list.files(map_dispatch3.dir, pattern = ".*png$", full.names = TRUE)
gifski(png_files, gif_file = file.path(map_dispatch3.dir,paste0("naris_map_dispatch3_series_",scenario,".gif")), width = 1920, height = 1080, delay = 0.25)

#----------------------------------------------------------------------------
#' Run a comparative insight series
# cat("Running comparative insight series...")
# dir.create(file.path(animation1.dir, "comparative_insight..."))
# comparative_insight.dir <- file.path(animation1.dir, "comparative_insight")
# draw_naris_comparative_insight_series(t0, tn, types=types)
# png_files <- list.files(comparative_insight.dir, pattern = ".*png$", full.names = TRUE)
# gifski(png_files, gif_file = file.path(comparative_insight.dir,"naris_comparative_insight_series.gif"), width = 1920, height = 1080, delay = 1)
# cat("Done!")
#' End