library("rgdal")
library("ggplot2")
library("ggmap")
library("magrittr")
library("lubridate")
library("RMySQL")
library("gridExtra")
library("dplyr")
library("reshape2")
library("plyr")
library("scales")
library("cowplot")

# heatmaps of weekday vs hour by violation type 
# heatmap of violation type vs make
# map snipits of violation type


#app idea
#ping and retreive current GPS status
#display mini-map with current GPS - within 2000 feet
#heatmap of time and day
#bar chart of ticket frequency


#Los Angeles Parking Tickets
#https://data.lacity.org/A-Well-Run-City/Parking-Citations/t4h6-r362/data
#NAD_1983_StatePlane_California_V_FIPS_0405_Feet projection.

#updloading data from .csv
parkingData <- read.csv("Parking_Citations.csv", header = TRUE, nrows = 10)
parkLatLong <- parkingData[c("Latitude", "Longitude")]

#reprojecting the 1983 StatePlane 0405 to GPS
coordinates(parkLatLong) <- ~ Latitude+Longitude # where X and Y stand for the name of your lat/lon columns 
proj4string(parkLatLong) <- CRS("+init=esri:102645")  #if your coordinates are in WGS84 
parkLatLong.proj <- spTransform(parkLatLong, CRS("+init=esri:4326"))


#reappending the the reprojected  to the ordinial data 
parkingDFtemp <- as.data.frame(parkLatLong.proj)
parkingData$Latitude <- format(parkingDFtemp$Latitude, digit = 22)
parkingData$Longitude <- format(parkingDFtemp$Longitude, digit = 22)

#establishing connection with a local MySQL database  
mydb = dbConnect(RMySQL::MySQL(),
                 user='root',
                 password='root', 
                 unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock",
                 port = 3306,
                 dbname = "parking_tickets")

#writing data to the local database
#dbWriteTable(mydb, value = parkingData, name = "la_parking_tickets", append = TRUE) 

parkingData.head <- dbGetQuery(mydb, "SELECT * FROM la_parking_tickets limit 0")
parkingData.head$Latitude <- as.numeric(parkingData.head$Latitude)
parkingData.head$Longitude <- as.numeric(parkingData.head$Longitude)

parkingData.head <- dbGetQuery(mydb, "SELECT `Violation.Description` FROM la_parking_tickets LIMIT 50")

violDesrip <- dbGetQuery(mydb, "SELECT `Violation.Description`, COUNT(`Violation.Description`) AS count FROM la_parking_tickets GROUP BY `Violation.Description`")
violDesrip <- violDesrip[order(-violDesrip$count),]

#creating list of makes to clean up
carModel <- dbGetQuery(mydb, "SELECT Make, COUNT(Make) AS count FROM la_parking_tickets GROUP BY Make")
carModel <- carModel[order(-carModel$count),]

carModel$newMake <- recode(carModel$Make, TOYT = 'Toyota', HOND = 'Honda', FORD = 'Ford', CHEV = 'Chevrolet', NISS = 'Nissan', TOYO = 'Toyota', BMW = 'BMQ', DODG = 'Dodge', VOLK = 'Volkswagen', HYUN = 'Hyundai', MERZ = 'Mercedes Benz', LEXS = 'Lexis', JEEP = 'Jeep', AUDI = 'Audi', MAZD = 'Mazda', OTHR = 'Other', KIA = 'Kia', GMC = 'GMC', CHRY = 'Chrysler', ACUR = 'Acura', INFI = 'Infinity', VOLV = 'Volvo', BENZ = 'Mercedes Benz', MITS = 'Mitsubishi', LEXU = 'Lexis', CADI = 'Cadillac', VW = 'Volkswagen', SUBA = 'Subaru', MBZ = 'Mercedes Benz', PONT = 'Pontiac', PORS = 'Porsch', GRUM = 'Grumman', BUIC = 'Buick', LINC = 'Lincoln', SCIO = 'Scion', MERC = 'Mercury', STRN = 'Saturn', MNNI = 'Mini', RROV = 'Range Rover', UNK = 'Other', JAGU = 'Jaguar', UNKN = 'Other', FRHT = 'Freightliner', FIAT = 'Fiat', LNDR = 'Land Rover', MINI = 'Mini', SATU = 'Saturn', OLDS = 'Oldsmobile', ISU = 'Isuzu', PLYM = 'Plymouth')
carModel.m <- melt(carModel, id = "newMake", value = "count") 

sum(carModel$count[1:50]) / sum(carModel$count)

#just trying out some data
noPark <- dbGetQuery(mydb, "SELECT * FROM la_parking_tickets WHERE `Violation.Description` = 'NO PARK/STREET CLEAN'")

#cleaning up the make coloum 
noPark$newMake <- recode(noPark$Make, TOYT = 'Toyota', HOND = 'Honda', FORD = 'Ford', CHEV = 'Chevrolet', NISS = 'Nissan', TOYO = 'Toyota', BMW = 'BMW', DODG = 'Dodge', VOLK = 'Volkswagen', HYUN = 'Hyundai', MERZ = 'Mercedes Benz', LEXS = 'Lexis', JEEP = 'Jeep', AUDI = 'Audi', MAZD = 'Mazda', OTHR = 'Other', KIA = 'Kia', GMC = 'GMC', CHRY = 'Chrysler', ACUR = 'Acura', INFI = 'Infinity', VOLV = 'Volvo', BENZ = 'Mercedes Benz', MITS = 'Mitsubishi', LEXU = 'Lexis', CADI = 'Cadillac', VW = 'Volkswagen', SUBA = 'Subaru', MBZ = 'Mercedes Benz', PONT = 'Pontiac', PORS = 'Porsch', GRUM = 'Grumman', BUIC = 'Buick', LINC = 'Lincoln', SCIO = 'Scion', MERC = 'Mercury', STRN = 'Saturn', MNNI = 'Mini', RROV = 'Range Rover', UNK = 'Other', JAGU = 'Jaguar', UNKN = 'Other', FRHT = 'Freightliner', FIAT = 'Fiat', LNDR = 'Land Rover', MINI = 'Mini', SATU = 'Saturn', OLDS = 'Oldsmobile', ISU = 'Isuzu', PLYM = 'Plymouth')

noPark.make <- group_by(noPark, newMake) %>% summarise(count = n()) %>% ddplyr::arrange(-count) %>% as.data.frame()
noPark.make <- transform(noPark.make, newMake=reorder(newMake, count)) 

str(noPark$Issue.Date)

#cleaning up the date and time 

parkingData.head <- dbGetQuery(mydb, "SELECT * FROM la_parking_tickets")

parkingData$time <- sprintf("%04s", as.character(parkingData$Issue.time))
parkingData$time <- sub("([[:digit:]]{2,2})$", ":\\1", parkingData$Issue.time)
parkingData$time <- as.POSIXct(strptime(paparkingData$time, format = "%H:%M"))

#as.POSIXct(strptime(paste0(parkingData$Issue.Date, " ", parkingData$time), format = "%m-%d-%y %H:%M %p"))

parkingData$dateTime <- mdy_hm(paste0(parkingData$Issue.Date, " ", parkingData$time))
parkingData <- parkingData[parkingData$Violation.Description %in% top25Viol[1:5],]

parkingData$week <- as.Date(cut(parkingData$dateTime, breaks = "week"))

dateLim = as.POSIXct(strptime(c("2012-01-01 00:00","2013-07-10 23:59"), format = "%Y-%m-%d %H:%M"))  

ggplot(parkingData) + geom_line(aes(x = as.POSIXct(week), color = Violation.Description), stat = "count") +
  scale_x_datetime(date_break = "1 month", date_labels = "%m-%Y", limits = dateLim) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#creating heatmap of time and week
parkingData$hour <- sprintf("%04s", as.character(parkingData$Issue.time)) %>% substr(1, 2)
parkingData$weekday <- weekdays(parkingData$dateTime)

#summarizing the data
parkingData.group <- group_by(parkingData, weekday, hour) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(-count) %>% as.data.frame()%>%
  ddply(.(hour), transform, rescale = rescale(count)) # from plyr

#heatmap of hour by weekday
ggplot(parkingData.group, aes(hour, weekday)) + geom_tile(aes(fill = rescale), color = "black") +
  scale_fill_gradient(low = "white", high = "purple") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = FALSE, fill = FALSE)

#creating heat map of ticket violation and make
parkingData.head <- dbGetQuery(mydb, "SELECT * FROM la_parking_tickets")  #LIMIT 5000000")
parkingData.head$newMake <- recode(parkingData.head$Make, TOYT = 'Toyota', HOND = 'Honda', FORD = 'Ford', CHEV = 'Chevrolet', NISS = 'Nissan', TOYO = 'Toyota', BMW = 'BMW', DODG = 'Dodge', VOLK = 'Volkswagen', HYUN = 'Hyundai', MERZ = 'Mercedes Benz', LEXS = 'Lexis', JEEP = 'Jeep', AUDI = 'Audi', MAZD = 'Mazda', OTHR = 'Other', KIA = 'Kia', GMC = 'GMC', CHRY = 'Chrysler', ACUR = 'Acura', INFI = 'Infinity', VOLV = 'Volvo', BENZ = 'Mercedes Benz', MITS = 'Mitsubishi', LEXU = 'Lexis', CADI = 'Cadillac', VW = 'Volkswagen', SUBA = 'Subaru', MBZ = 'Mercedes Benz', PONT = 'Pontiac', PORS = 'Porsch', GRUM = 'Grumman', BUIC = 'Buick', LINC = 'Lincoln', SCIO = 'Scion', MERC = 'Mercury', STRN = 'Saturn', MNNI = 'Mini', RROV = 'Range Rover', UNK = 'Other', JAGU = 'Jaguar', UNKN = 'Other', FRHT = 'Freightliner', FIAT = 'Fiat', LNDR = 'Land Rover', MINI = 'Mini', SATU = 'Saturn', OLDS = 'Oldsmobile', ISU = 'Isuzu', PLYM = 'Plymouth')
parkingData.head <- parkingData.head[parkingData.head$Violation.Description %in% top25Viol[1:25],]
parkingData.head <- parkingData.head[parkingData.head$newMake %in% carModel$newMake[1:25],]

#parkingData.m <- melt(parkingData.head, id = c("Violation.Description", "newMake"))
parkingData.g <- group_by(parkingData.head, Violation.Description, newMake) %>% dplyr::summarise(count = n()) %>% arrange(-count) %>% as.data.frame()
parkingData.gmr <- ddply(parkingData.g, .(Violation.Description), transform, rescale = rescale(count))

#actual creation of heat map for violation description vs. newMake
ggplot(parkingData.gmr, aes(newMake, Violation.Description)) + geom_tile(aes(fill = rescale), color = "black") +
  scale_fill_gradient(low = "yellow", high = "#f03b20") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  guides(colour = FALSE, fill = FALSE) #+
  #scale_fill_viridis(name="# Events", label=comma) +
  #coord_equal()
  #ggtitle("What is the most common experience need?") +
  #ylab("Experience") +
  #xlab("Agency Type") +
  #themeBrettrics

#retreiving the map for projecting
mapLA <- get_map(location = "los angeles", 
                 zoom = 10, 
                 maptype = "roadmap", 
                 scale = 1,
                 messaging = FALSE)

#creates density map
ggmap(mapLA) +
stat_density2d(aes(x = as.numeric(Latitude), y = as.numeric(Longitude), fill = ..level.., alpha = ..level..),
                   size = 2, bins = 10, data = noPark, geom = "polygon")

#creating scale for time series
twelveLim = as.POSIXct(strptime(c("2016-09-10 00:00","2016-09-10 23:59"), format = "%Y-%m-%d %H:%M"))  

#creating time plot
ggplot(data = noPark) + 
      geom_area(aes(x = time), stat = "count") +
      scale_x_datetime(date_break = "2 hour", date_labels = "%I:%M %p", limits = twelveLim) +
      theme(axis.text.x = element_text(angle = 45)) +
      labs(title = violDesrip$Violation.Description[i])

#plotting make and model
ggplot(data = noPark.make[1:30, ]) +
  geom_bar(aes(x = newMake, y = count), stat = "identity") +
  coord_flip()

noPark$time <- sprintf("%04s", as.character(noPark$Issue.time))
noPark$time <- sub("([[:digit:]]{2,2})$", ":\\1", noPark$Issue.time)
noPark$time <- as.POSIXct(strptime(noPark$time, format = "%H:%M"))


#loop through queries for multiply graphs
d <- list()
t <- list()
for (i in 1:5) {
  queryParking <- dbGetQuery(mydb, paste0("SELECT * FROM la_parking_tickets WHERE `Violation.Description` = '", violDesrip$Violation.Description[i], "'"))
  d[[i]] <- ggmap(mapLA) +
    stat_density2d(aes(x = as.numeric(Latitude), y = as.numeric(Longitude), fill = ..level.., alpha = ..level..), size = 2, bins = 10, data = queryParking, geom = "polygon") +
    labs(title = violDesrip$Violation.Description[i])
  
  queryParking$time <- sprintf("%04s", as.character(queryParking$Issue.time))
  queryParking$time <- sub("([[:digit:]]{2,2})$", ":\\1", queryParking$time)
  queryParking$time <- as.POSIXct(strptime(queryParking$time, format = "%H:%M"))
  
  t[[i]] <- ggplot(data = queryParking) + 
    geom_area(aes(x = time), stat = "count") +
    scale_x_datetime(date_break = "2 hour", date_labels = "%I:%M %p", limits = twelveLim) +
    theme(axis.text.x = element_text(angle = 76), axis.text.y = element_blank()) +
    labs(title = violDesrip$Violation.Description[i], y = NULL, x = "Time")
  
    #tempT <- t[[i]]
    #print[tempT]  
  
  print(i)
}

#explorting map
svg("map.svg",
    width = 7,
    height = 14,
    pointsize = 12)    
print(do.call(grid.arrange, list(grobs = d, ncol=1)))
dev.off()

#explorting times
svg("time.svg",
    width = 12,
    height = 21,
    pointsize = 12)    
print(do.call(grid.arrange, list(grobs = t, ncol=3)))
dev.off()

# ----- app prototype ------

library("cowplot")

subParkingData <- head(parkingData.head, n = 10000000)
subParkingData$lon <- as.numeric(subParkingData$Latitude)
subParkingData$lat <- as.numeric(subParkingData$Longitude)

# feetWidth <- 1000 
# radius <- (feetWidth/5)*.001

centerPointLat <- 34.048670 
centerPointLon <- -118.251751

mapLAsqaure <- get_map(location = c(lon = centerPointLon, lat = centerPointLat), 
                 zoom = 16, 
                 maptype = "roadmap", 
                 scale = 1,
                 messaging = FALSE)

lonDiff <- .0065
latDiff <- .0055

setParkingData <- subset(subParkingData, lon >= centerPointLon - lonDiff & lon <= centerPointLon + lonDiff & lat >= centerPointLat - latDiff & lat <= centerPointLat + latDiff)

mapPlotLA <- ggmap(mapLAsqaure) + geom_point(data = setParkingData, aes(x = lon, y = lat, color = "#ff6666"), alpha = .05) + 
  coord_fixed(ratio=1:1) + 
  ggtitle("Map of Past Tickets Around You") + 
  guides(colour=FALSE)
  

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
  
plot_grid(mapPlotLA, heatmapPlotLA, barPlotLA, ncol = 1, scale = c(1,.8, .9))
