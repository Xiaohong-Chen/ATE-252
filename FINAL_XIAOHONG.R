# Thanks for Mr.Mark Egge's tutorial, providing me with a bunch of useful library and code demo
# This project is created by referencing Mr.Mark Egge's tutorial (mark@eateggs.com)

library(sf) 
library(data.table) 
library("ggplot2")

options(scipen = 10000) # disable scientific notation

segments <- st_read("shp/traffic/traffic.shp") # reed traffic shape file
segments$id <- 1:nrow(segments) # assign a unique id for each segment

crashes <- read.csv(file = 'data/Statewide_2018/CRASH_2018_Statewide.csv') # read crashing data

# Filter to only crashes rows with "2 - Dark - No Street Lights" or "3 - Dark - Street Lights"
crashes <- crashes[which(crashes$ILLUMINATION %in% c(2, 3)),] 
crashes$crash_lighting = ifelse(crashes$ILLUMINATION == 2,"nolight" , "light")

# Remove na rows 
naLongRowsToRemove = which(is.na(crashes$DEC_LONG))
crashes <- crashes[-naLongRowsToRemove,]

naLatRowsToRemove = which(is.na(crashes$DEC_LAT))
crashes <- crashes[-naLatRowsToRemove,]

crashes <- st_as_sf(crashes, coords = c("DEC_LONG", "DEC_LAT"), crs = 4326) # create sf spatial object
crashes <- st_transform(crashes, 2272) # reproject data to EPSG:2272

# buffer road segments by 50 feet
segment_buffer <- st_buffer(segments, dist = 50, endCapStyle = "FLAT")

# join crashes to road segment data 
joined <- st_join(segment_buffer, crashes, left = FALSE) 

# convert sf to data.table
segment_crashes <- as.data.table(st_drop_geometry(joined)) 

# make a integration
counts <- segment_crashes[, .(crash_count = .N), by = .(id, crash_lighting, DLY_VMT)]

# Road Crashing Distribution
ggplot(counts, aes(x = id, y = crash_count , color = "All")) + 
   geom_point(alpha = 0.6, size = 0.5) +
   ggtitle("Road Crashing Distribution") + 
   labs(x = "Road Segment ID") +
   labs(y = "Total Crash for Each Segment") +
   theme(legend.title=element_blank())+
   theme(legend.position="top")

ggplot(counts, aes(x = id, y = crash_count,color = crash_lighting)) + 
   geom_point(alpha = 0.6, size = 0.5) +
   ggtitle("Light vs. No Light") + 
   labs(x = "Road Segment ID") +
   labs(y = "Total Crash of Each Segment)") +
   theme(legend.title=element_blank())+
   theme(legend.position="top")


qplot(data =counts, geom="boxplot",x= crash_lighting, y=as.integer(counts$crash_count),
      xlab = "Illumination Condition", ylab = "Total Crash",
      main="Light vs. No Light",
)


light_segment_crashes <- counts$crash_count[which(counts$crash_lighting=="light")]
light_segment_crashes

nolight_segment_crashes <- counts$crash_count[which(counts$crash_lighting=="nolight")]
nolight_segment_crashes 

# doing t.test and find the p-value
t.test(light_segment_crashes, nolight_segment_crashes)

ggplot(counts, aes(x = DLY_VMT, y = crash_count, color = crash_lighting)) + 
   geom_point(alpha = 0.65, size = 0.5) +
   ggtitle("Total Crash vs. VMT") + 
   labs(x = "VMT(total annual miles of vehicle travel divided by the total population)") +
   labs(Y = "Total Crash of Each Segment)")+
   theme(legend.title=element_blank())+
   theme(legend.position="top")

