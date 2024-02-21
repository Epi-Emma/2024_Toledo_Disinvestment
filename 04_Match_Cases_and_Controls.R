##############################
## Match Cases to Controls  ##
##############################

# Written By: Emma Gause
# Date: 05/01/23
# Last updated: 05/18/23

#Load in Libraries
library("tidyverse")
library("dplyr")
library("sf")
library("mapview")
library("MatchIt")

#Create path to directory
datadir <- "/[path to data directory]/"

#read in case/control street points
pts <- read_sf(paste0(datadir, "case_control_points/case_control_covariates.shp"))

#read in neighborhood boundaries for subset 
nbh <- read_sf(paste0(datadir, "NeighborhoodV2/Neighborhoods.shp"))

##----------------------------------------------------------------------------##

str(pts)
table(pts$case, pts$exclude, useNA = "ifany")

#first create df for just case points
cases <- pts %>% filter(case == 1)

#now create control pool
#remove all cases, exclusions and any points within 1/8mi of these
toremove <- pts %>% filter(case == 1|exclude==1) #points to remove
toremove <- st_buffer(toremove, dist = 660) #1/8mi buffers around removals
#mapview(toremove, zcol = "case") + mapview(toremove)
toremove <- toremove %>% select(-uid)

spillover <- st_join(toremove, pts, left=TRUE) #find all pts 
rmids <- unique(spillover$uid)

controls <- pts %>% filter(!uid %in% rmids)
#mapview(controls)

str(pts)
test <- pts %>% filter(case==1|exclude==1)
mapview(test, zcol = "case") + mapview(nbh)

## Now remove neighborhoods with zero cases ##
#identify neighborhoods with zero cases
nbh_inter <- st_intersects(nbh,cases)
logic_nbh <- lengths(nbh_inter)>0 #Finds only nbhs where intersection is TRUE
inc_nbh <- nbh[logic_nbh,]
mapview(inc_nbh) + mapview(nbh) + mapview(cases)

#clip control points by possible neighborhoods
str(inc_nbh)
controls <- st_intersection(controls, inc_nbh)
#mapview(controls)

#remove unneccesary columns for rbind below
controls <- controls %>% select(-OBJECTID:-Name)


##----------------------------------------------------------------------------##

nrow(pts) - nrow(cases) # originally 25801 controls. 
nrow(controls) # 15421 after exclusions

#create a flag for case vs. control
cases$cctype <- "case"
controls$cctype <- "control"

#combine them back together 
colnames(cases)
colnames(controls)
dat <- rbind(cases, controls)
str(dat)
table(dat$cctype, useNA = "ifany")


#format data for matching

#check any missingness in matching vars...
summary(dat)
miss <- dat %>% filter(is.na(road_type)|
                          is.na(major_road)|
                          is.na(zonetype)|
                          is.na(pov_pct))

#none exists

#create decile for poverty matching
dat <- dat %>% mutate(pov_dec = ntile(pov_pct,10))

#create factors
table(dat$road_type)
table(dat$major_road)
table(dat$zonetype)
dat <- dat %>% mutate(road_type_fact = factor(road_type, levels = c("intersection", "midpoint"),
                                                labels = c("intersection", "midpoint")),
                        major_road_fact = factor(major_road, levels = c("0", "1"),
                                                 labels = c("Minor", "Major")),
                        zonetype_fact = factor(zonetype, 
                                               levels = c("commercial","industrial", "institutional",
                                                          "mixed","open_space","residential"),
                                               labels = c("commercial","industrial", "institutional",
                                                          "mixed","open_space","residential")))

#fix rare zone types <- there are too few instances to compare these types. 
  #convert to most likely alternative (based on mapping)
table(dat$zonetype, dat$cctype)
dat$zonetype_i <- dat$zonetype
dat$zonetype_i[dat$zonetype=="open_space"] <- "residential"
dat$zonetype_i[dat$zonetype=="mixed"] <- "commercial"
dat$zonetype_i[dat$zonetype=="institutional"] <- "commercial"
dat$zonetype_i_fact <- factor(dat$zonetype_i, levels = c("commercial","industrial",
                                                           "residential"),
                               labels = c("commercial","industrial",
                                          "residential"))
table(dat$zonetype_i_fact, dat$zonetype, useNA = "ifany")

##----------------------------------------------------------------------------##

#create binary variable for cctype
dat$cctype_01 <- if_else(dat$cctype=="case", 1, 0)
table(dat$cctype_01, dat$cctype, useNA = "ifany")

#assign a random ID and arrange so controls don't cluster
set.seed(051823)
dat$randomID <- sample(nrow(dat))
summary(dat$randomID)
dat <- dat %>% arrange(randomID)

#now get 1:4 controls matched using exact matches without replacement 
matches <- matchit(cctype_01 ~ road_type_fact + major_road_fact + zonetype_i_fact + pov_dec, 
                data = dat,
                method = "nearest",
                exact = c("road_type_fact", "major_road_fact", "zonetype_i_fact", "pov_dec"),
                replace = FALSE,
                ratio=4)
summary(matches)
match_mat <- matches$match.matrix

#now get the dataset of cases and controls
data <- match.data(matches)

str(data)
datax <- as.data.frame(data)
datax <- st_set_geometry(datax, value = "geometry")
mapview(datax, zcol = "cctype")


#save out matched dataset
write_sf(datax, dsn = paste0(datadir, "Matched/Matched_Analsis_Set.shp"),
         delete_dsn = TRUE)



