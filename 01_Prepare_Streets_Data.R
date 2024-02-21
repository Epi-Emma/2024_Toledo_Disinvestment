##############################################
## Prepare Toledo Streets Data for Analysis ##
##############################################

# Written By: Emma Gause
# Date: 04/05/23

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("lubridate")
library("mapview")
library("nngeo")
library("stringr")

#Create path to directory
datadir <- "/[path to data directory]/"
dataexp <- "/[path to export directory]/"


#read in intersection points & midpoints [these were created within QGIS]
inters <- read_sf(paste0(dataexp, "Street_Nodes/Street_Intersections/Street_Intersections.shp"))
mids <- read_sf(paste0(dataexp, "Street_Nodes/Street_Midpoints/Street_Midpoints.shp")) 

#Source of streets data: City of Toledo Data Hub [accessed March 3, 2023]

##----------------------------------------------------------------------------##
#combine street lines and intersections
str(inters)
str(mids)
inters <- inters %>% mutate(type = "intersection") %>% select(-OBJECTID, -AssetID) 
colnames(inters)
colnames(mids)
mids <- mids %>% mutate(type = "midpoint") %>% 
  select(LegacyID, ROADTYPE, LSN, geometry, type)

pts <- rbind(inters, mids)
pts <- st_set_geometry(pts, "geometry")
pts <- st_make_valid(pts)
st_crs(pts) #check the projection

#create field with string geography 
pts$geochr <- as.character(pts$geometry)
colnames(pts)
#group by geography and summarize -- we know there are overlapping points
  #because the end of one road segment is the beginning of the next - 
  #we want to remove these overlapping points
uniqpts <- pts %>% group_by(geochr) %>% summarise(LegacyID = first(LegacyID),
                                                  ROADTYPE = first(ROADTYPE),
                                                  LSN = first(LSN),
                                                  type = first(type),
                                                  geochr = first(geochr)) %>% ungroup()
str(uniqpts)
uniqpts <- st_set_geometry(uniqpts, value = "geometry") #set geography again

#create unique id for each node
uniqpts <- uniqpts %>% mutate(streetid = row_number())
#remove 26709 - empty geography
uniqpts <- uniqpts %>% filter(streetid !=26709)

#remove interstate points from street nodes
table(uniqpts$ROADTYPE, useNA = "ifany")
uniqpts <- uniqpts %>% filter(ROADTYPE!="I"&ROADTYPE!="R")

#create dichotomy of major and minor roads
uniqpts <- uniqpts %>% mutate(major_road = if_else((ROADTYPE=="C"|
                                            ROADTYPE=="S"), 1, 0))

write_sf(uniqpts, 
         dsn = paste0(dataexp, "Street_Nodes/Street_Nodes/Street_Nodes_MajorMinor_Subset.shp"), 
         delete_dsn = TRUE)


##----------------------------------------------------------------------------##
