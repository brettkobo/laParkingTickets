#Parking Ticket Application
#library("rgdal")
library("ggplot2")
library("ggmap")
library("magrittr")
library("lubridate")
library("RMySQL")
#library("gridExtra")
library("dplyr")
library("reshape2")
library("plyr")
library("scales")
library("cowplot")
require(devtools)
install_github('ramnathv/rCharts')

#query for creating table based on subset of data
# CREATE TABLE la_parking_tickets2016 AS SELECT * FROM la_parking_tickets WHERE `Issue.Date` LIKE '%2016' AND Longitude > -119;

#establishing connection with mysql db
mydb = dbConnect(RMySQL::MySQL(),
                 user='root',
                 password='root', 
                 unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock",
                 port = 3306,
                 dbname = "parking_tickets")

#parameters of map / creates concept of map
lonDiff <- .0065
latDiff <- .0055

#location of interest
centerPointLat <- 34.048670 
centerPointLon <- -118.251751

start.time <- Sys.time()
# query to retreive data points 
subParkingData <- dbGetQuery(mydb, paste("SELECT * FROM la_parking_tickets WHERE Longitude >=", 
                          centerPointLon - lonDiff, "AND Longitude <=",
                          centerPointLon + lonDiff, "AND Latitude >=",
                          centerPointLat - latDiff, "AND Latitude <=",
                          centerPointLat + latDiff, sep = " "))

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# old way of subseting preloaded data going to let db do heavy lifting
# subParkingData <- head(parkingData.head, n = 10000000)
# subParkingData$lon <- as.numeric(subParkingData$Latitude)
# subParkingData$lat <- as.numeric(subParkingData$Longitude)
# setParkingData <- subset(subParkingData, lon >= centerPointLon - lonDiff & lon <= centerPointLon + lonDiff & lat >= centerPointLat - latDiff & lat <= centerPointLat + latDiff)

#retrieves map
mapLAsqaure <- get_map(location = c(lon = centerPointLon, lat = centerPointLat), 
                       zoom = 16, 
                       maptype = "roadmap", 
                       scale = 1,
                       messaging = FALSE)

mapPlotLA <- ggmap(mapLAsqaure) + geom_point(data = setParkingData, aes(x = lon, y = lat, color = "#ff6666"), alpha = .05) + 
  coord_fixed(ratio=1:1) + 
  ggtitle("Map of Past Tickets Around You") + 
  guides(colour=FALSE)

ggmap(mapLAsqaure) +
 # coord_cartesian() +
  stat_density2d(data = setParkingData, aes(x = lon, y = lat, fill = ..level.., alpha = ..level.., color = "black"),
                 size = 5, bins = 10, geom = "polygon") 

#creating heatmap of time and week
setParkingData$time <- sprintf("%04s", as.character(setParkingData$Issue.time))
setParkingData$time <- sub("([[:digit:]]{2,2})$", ":\\1", setParkingData$Issue.time)
setParkingData$time <- as.POSIXct(strptime(setParkingData$time, format = "%H:%M"))
setParkingData$dateTime <- mdy(setParkingData$Issue.Date)
setParkingData$hour <- sprintf("%04s", as.character(setParkingData$Issue.time)) %>% substr(1, 2)
setParkingData$weekday <- weekdays(setParkingData$dateTime)

#summarizing the data
setParkingData.group <- group_by(setParkingData, weekday, hour) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(-count) %>% as.data.frame() %>%
  plyr::ddply(.(hour), transform, rescale = rescale(count)) # from plyr

heatmapPlotLA <- ggplot(setParkingData.group, aes(hour, weekday)) + geom_tile(aes(fill = rescale), color = "black") +
  scale_fill_gradient(low = "white", high = "#66ff66") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = FALSE, fill = FALSE) +
  coord_equal() +
  ggtitle("Time of Day vs Weekday") 

setParkingData.violTable <- table(setParkingData$Violation.Description) %>% as.data.frame()
setParkingData.violTable <- setParkingData.violTable[order(-setParkingData.violTable$Freq),]
setParkingData.violTable$Var1 <- factor(setParkingData.violTable$Var1, levels = setParkingData.violTable$Var1[order(setParkingData.violTable$Freq)])

barPlotLA <- ggplot(data = head(setParkingData.violTable, n = 7)) + 
  geom_bar(aes(x = Var1, y = Freq), stat = "identity",fill = "#a3a3ff") +
  coord_flip() +
  guides(colour = FALSE, fill = FALSE) +
  ggtitle("Most Common Ticket in this Area") 



# laData <- list(mapPlotLA, heatmapPlotLA, barPlotLA)
# lay <- rbind(c(1,1,1,1),
#              c(NA,2,2,NA),
#              c(NA,3,4,NA))


# gridExtra::grid.arrange(grobs = laData, ncol = 1, nrow = 3, height = c(3,1,1))
plot_grid(mapPlotLA, heatmapPlotLA, barPlotLA, ncol = 1, scale = c(1,.8, .9))
})

runExample("07_widgets")


killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)
    +  dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}