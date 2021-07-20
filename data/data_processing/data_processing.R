#-------------------------------------------------------------------------------------
#' Generate the input files in csv format for NARIS kaleidoscope
#' 1. naris_centroids.csv
#' 2. naris_gen.csv
#' 3. Generator.rds -> naris_generation.csv
#' 4. Resource.rds and Region.rds -> naris_dispatch_stack.csv
#' 5. Line.rds -> naris_net_interchange.csv
#'
#' R version: 3.6.3
#' 
#' Load the required R packages
library(data.table)
library(dplyr)
# library(lubridate)
library(rgeos)
library(sp)
# library(tibbletime)
library(tidyr)
#' 
#' #----------------------------------------------------------------------------
#' Clean the environment
rm(list=ls())
#' 
#' Read node lat/lon database
plexos_nodes_naris <- read.csv("//nrelnas01/PLEXOS CEII/Projects/NARIS/ForJoAnn/plexos_nodes_naris_20191106.csv", header=T)
EI_WI_ERCOT_node_pca <- read.csv("//nrelnas01/PLEXOS CEII/Projects/NARIS/ForJoAnn/EI_WI_ERCOT_node_pca_v4.csv", header=T)
plexos_nodes_naris_Mexico <- subset(plexos_nodes_naris, plexos_nodes_naris$interconnect == "")
plexos_nodes_naris_Mexico$PCA <- substr(plexos_nodes_naris_Mexico$plexos_id, 13, 16)
EI_WI_ERCOT_node_pca_Mexico <- subset(EI_WI_ERCOT_node_pca, EI_WI_ERCOT_node_pca$Inter == "Mexico")
nodes_Mexico <- full_join(plexos_nodes_naris_Mexico, EI_WI_ERCOT_node_pca_Mexico, by="PCA")
nodes_Mexico$Latitude <- nodes_Mexico$latitude
nodes_Mexico$Longitude <- nodes_Mexico$longitude
nodes_Mexico <- nodes_Mexico[,c(13:18,12,19:23)]
EI_WI_ERCOT_node_pca_NotMexico <- subset(EI_WI_ERCOT_node_pca, EI_WI_ERCOT_node_pca$Inter != "Mexico")
EI_WI_ERCOT_node_pca_All <- rbind(EI_WI_ERCOT_node_pca_NotMexico, nodes_Mexico)
#'
#' Read region-category database
#region.category <- read.csv(file = "./input_csv/region-category.csv", header=T)
region.category <- read.csv(file = "./input_csv/Region_mapping_withMexico.csv", header=T)
names(region.category)[names(region.category)=="region"] <- "Region"
names(region.category)[names(region.category)=="Usual"] <- "Category"
region.category$Country <- NULL
region.category$Interconnection <- NULL
region.category$CountryInterconnect <- NULL
#'
#' Merge region-category with lat/lon
naris_nodes <- merge(region.category, EI_WI_ERCOT_node_pca_All, by.x="Region", by.y="PLEXOS_Region")
naris_nodes <- naris_nodes[, c(1:3,5,6)]
naris_nodes <- na.omit(naris_nodes)
naris_nodes <- filter(naris_nodes, Latitude != 0, Longitude != 0)
#'
#' Prepare and write naris_centroids.csv
# categories <- sort(as.character(unique(region.category$Category)))
categories <- sort(as.character(unique(naris_nodes$Category)))
num_categories <- length(categories)
temp4 <- data.table()
for (counter in 1:num_categories){
  temp1 <- subset(naris_nodes, Category == categories[counter])
  temp2 <- SpatialPoints(coords = temp1[,c('Latitude','Longitude')])
  temp3 <- gCentroid(temp2)
  temp4 <- rbind(temp4,temp3@coords)
}
naris_centroids <- cbind(categories,temp4)
naris_centroids <- naris_centroids[,c(1,3,2)]
names(naris_centroids)[1] <- "ISO"
names(naris_centroids)[2] <- "lon"
names(naris_centroids)[3] <- "lat"
naris_centroids <- na.omit(naris_centroids)
naris_centroids <- naris_centroids[!duplicated(naris_centroids), ]
write.csv(naris_centroids, file="./output_csv/naris_centroids.csv", row.names=F)
#'
#' Read generator-node database
generator.node <- read.csv(file = "./input_csv/generator-node.csv", header=T)
#' 
#' Read resource type-generator category database
generator.resource <- read.csv(file = "./input_csv/generator-resource.csv", header=T)
#' 
#' Prepare and write naris_gen.csv
names(generator.node)[names(generator.node)=="Node"]<-"PLEXOS_Node"
naris_gen <- full_join(generator.node, naris_nodes, by="PLEXOS_Node")
naris_gen <- full_join(naris_gen, generator.resource, by="Generator")
#' Add Generator_Group_Name for grouping of units into power stations based on resource type, lat, and lon
naris_gen$Generator_Group_Name <- paste0(naris_gen$Resource, naris_gen$Latitude, naris_gen$Longitude)
naris_gen <- naris_gen[,c(8,1,4,7,5,6)]
colnames(naris_gen)[2:6] <- c("Generator_Name", "Node_Region", "Type", "lat", "lon")
naris_gen <- na.omit(naris_gen)
naris_gen <- naris_gen[!duplicated(naris_gen), ]
write.csv(naris_gen, file="./output_csv/naris_gen.csv", row.names=F)
#'
#' Input the start and end datetime
#' Keep timezone format based on the timezone format of the data queried
#' Note: The rds data used in this file used UTC format
# t0 <- as.POSIXlt("2024-06-28 00:00:00", tz="UTC")
# tn <- as.POSIXlt("2024-06-29 00:00:00", tz="UTC")
t0 <- as.POSIXlt("2024-06-28 00:00:00", tz="EST")
tn <- as.POSIXlt("2024-06-29 00:00:00", tz="EST")
#'
#' Prepare and write naris_generation.csv
naris_generation_rds <- readRDS(file = "./input_rds/Generator.rds")
naris_generation_rds$Type <- NULL
naris_generation_rds <- filter(naris_generation_rds, time >= t0, time <= tn)
names(naris_gen)[names(naris_gen)=="Generator_Name"] <- "name"
naris_generation_rds <- full_join(naris_generation_rds, naris_gen, by="name")
naris_generation <- subset(naris_generation_rds, select=c(name, time, Generation, scenario))
names(naris_generation)[names(naris_generation)=="name"]<-"Generator_Name"
names(naris_generation)[names(naris_generation)=="Generation"]<-"power"
naris_generation <- na.omit(naris_generation)
naris_generation <- naris_generation[!duplicated(naris_generation), ]
write.csv(naris_generation, file="./output_csv/naris_generation.csv", row.names=F)
#'
#' Prepare and write naris_dispatch_stack.csv
#' Generation stack
naris_generation_stack <- naris_generation_rds %>% group_by(Type, time, Node_Region, scenario) %>%
  summarise(value = sum(Generation))
names(naris_generation_stack)[names(naris_generation_stack)=="Node_Region"]<-"zone"
#' Curtailment
naris_curtailment <- filter(naris_generation_rds, Type %in% c("CSP", "Solar", "Wind"))
naris_curtailment$Curtailment <- naris_curtailment$`Available Capacity` - naris_curtailment$Generation
naris_curtailment <- naris_curtailment %>% group_by(time, Node_Region, scenario) %>%
  summarise(value = sum(Curtailment))
naris_curtailment$Type <- "Curtailment"
names(naris_curtailment)[names(naris_curtailment)=="Node_Region"]<-"zone"
naris_curtailment <- naris_curtailment[,c(5,1:4)]
#'  Load
naris_load <- readRDS(file = "./input_rds/Region.rds")
naris_load <- filter(naris_load, time >= t0, time <= tn)
naris_load <- merge(naris_load, region.category, by.x="name", by.y="Region")
naris_load <- naris_load %>% group_by(time, Category, scenario) %>%
  summarise(value = sum(Load))
naris_load$Type <- "Load"
names(naris_load)[names(naris_load)=="Category"] <- "zone"
naris_load <- naris_load[,c(5,1:4)]
#' Merge generation, curtailment, and load data
naris_dispatch_stack <- rbind(naris_generation_stack, naris_curtailment, naris_load)
naris_dispatch_stack <- na.omit(naris_dispatch_stack)
naris_dispatch_stack <- naris_dispatch_stack[!duplicated(naris_dispatch_stack), ]
#' Write
write.csv(naris_dispatch_stack, file="./output_csv/naris_dispatch_stack.csv", row.names=F)
#' 
#' Prepare source2sink file based on PLEXOS model and region categories
node.region <- read.csv(file = "./input_csv/node-region.csv", header=T)
names(node.region)[names(node.region)=="Parent.Name"] <- "Node" 
names(node.region)[names(node.region)=="Child.Name"] <- "Region" 
node.region <- left_join(node.region, region.category, by="Region")
node.region$Collection <- NULL
node.region$Parent.Category <- NULL
node.region$Child.Category <- NULL
lines <- read.csv(file = "./input_csv/lines.csv", header=T)
lines$Class <- NULL
lines$Category <- NULL
lines$Description <- NULL
line.node.from <- read.csv(file = "./input_csv/line-node_from.csv", header=T)
line.node.to <- read.csv(file = "./input_csv/line-node_to.csv", header=T)
names(line.node.from)[names(line.node.from)=="Parent.Name"] <- "Name" 
names(line.node.to)[names(line.node.to)=="Parent.Name"] <- "Name" 
names(line.node.from)[names(line.node.from)=="Child.Name"] <- "Node" 
names(line.node.to)[names(line.node.to)=="Child.Name"] <- "Node" 
line.node.from <- left_join(line.node.from, node.region, by="Node")
line.node.to <- left_join(line.node.to, node.region, by="Node")
line.node.from <- line.node.from[,c(2,7)]
line.node.to <- line.node.to[,c(2,7)]
lines <- left_join(lines, line.node.from, by="Name")
names(lines)[names(lines)=="Category"] <- "Region.From"
lines <- left_join(lines, line.node.to, by="Name")
names(lines)[names(lines)=="Category"] <- "Region.To"
lines <- na.omit(lines)
lines$Interregional <- lines$Region.From == lines$Region.To
lines <- subset(lines, lines$Interregional == FALSE)
lines$Source2Sink <- paste0(lines$Region.From, " - ", lines$Region.To)
lines$Sink2Source <- paste0(lines$Region.To, " - ", lines$Region.From)
source2sink.list <- unique(lines$Source2Sink)
lines$Has.Reverse.Flow <- ifelse(lines$Sink2Source %in% source2sink.list, TRUE, FALSE)
lines$Dummy <- as.numeric(lines$Region.From) < as.numeric(lines$Region.To)
lines$Multiplier <- ifelse(lines$Has.Reverse.Flow == TRUE & lines$Dummy == FALSE, -1, 1)
lines$Source2Sink <- ifelse(lines$Multiplier == 1, lines$Source2Sink, lines$Sink2Source)
line.source2sink <- lines[,c(1,5,9)]
names(line.source2sink)[names(line.source2sink)=="Name"] <- "Line"
#' Just for quick check
write.csv(line.source2sink, file="./input_csv/line-source2sink.csv", row.names=F)
source2sink <- line.source2sink
source2sink$Include <- "Yes"
source2sink <- source2sink[,c(2,4)]
source2sink <- source2sink[!duplicated(source2sink), ]
write.csv(source2sink, file="./output_csv/source2sink_selector.csv", row.names=F)
#'
#' Prepare and write naris_net_interchange.csv
naris_line_flow <- readRDS(file = "./input_rds/Line.rds")
naris_line_flow <- filter(naris_line_flow, time >= t0, time <= tn)
# lines.source2sink <- read.csv(file = "./input_csv/line-source2sink.csv", header=T)
naris_line_flow <- merge(naris_line_flow, line.source2sink, by.x="name", by.y="Line")
naris_line_flow$AdjustedFlow <- naris_line_flow$Flow*naris_line_flow$Multiplier
naris_net_interchange <- naris_line_flow %>% group_by(time, scenario, Source2Sink) %>%
  summarise(value = sum(AdjustedFlow))
naris_net_interchange <- na.omit(naris_net_interchange)
naris_net_interchange <- naris_net_interchange[!duplicated(naris_net_interchange), ]
write.csv(naris_net_interchange, file="./output_csv/naris_net_interchange.csv", row.names=F)
#' End