library("rgdal")
library("ggplot2")
library("ggmap")
library("ggthemes")
library("magrittr")
library("lubridate")
library("RMySQL")
library("grid")
library("gridExtra")
library("dplyr")
library("reshape2") 
library("plyr") #need to load after dplyr, only use for one function
library("scales")
library("rgdal")
library("raster")
library("maptools")
#library("cowplot")

#establishing connection with self-hosted database
mydb = dbConnect(RMySQL::MySQL(),
                 user='root',
                 password='root', 
                 unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock",
                 port = 3306,
                 dbname = "parking_tickets")

#reading data into R from MySQL
parkingData <- dbGetQuery(mydb, "SELECT * FROM la_parking_tickets")

#recoding the make of the cars
parkingData$newMake <- recode(parkingData$Make, TOYT = 'Toyota', HOND = 'Honda', FORD = 'Ford', CHEV = 'Chevrolet', NISS = 'Nissan', TOYO = 'Toyota', BMW = 'BMW', DODG = 'Dodge', VOLK = 'Volkswagen', HYUN = 'Hyundai', MERZ = 'Mercedes Benz', LEXS = 'Lexis', JEEP = 'Jeep', AUDI = 'Audi', MAZD = 'Mazda', OTHR = 'Other', KIA = 'Kia', GMC = 'GMC', CHRY = 'Chrysler', ACUR = 'Acura', INFI = 'Infinity', VOLV = 'Volvo', BENZ = 'Mercedes Benz', MITS = 'Mitsubishi', LEXU = 'Lexis', CADI = 'Cadillac', VW = 'Volkswagen', SUBA = 'Subaru', MBZ = 'Mercedes Benz', PONT = 'Pontiac', PORS = 'Porsch', GRUM = 'Grumman', BUIC = 'Buick', LINC = 'Lincoln', SCIO = 'Scion', MERC = 'Mercury', STRN = 'Saturn', MNNI = 'Mini', RROV = 'Range Rover', UNK = 'Other', JAGU = 'Jaguar', UNKN = 'Other', FRHT = 'Freightliner', FIAT = 'Fiat', LNDR = 'Land Rover', MINI = 'Mini', SATU = 'Saturn', OLDS = 'Oldsmobile', ISU = 'Isuzu', PLYM = 'Plymouth')

#recoding violation descriptions
parkingData$newViolDescrip <- recode(parkingData$Violation.Description, `NO PARK/STREET CLEAN` = 'No Parking / Street Cleaning', `METER EXP.` = 'Meter Expired', `PREFERENTIAL PARKING` = 'Preferential Parking', `DISPLAY OF TABS` = 'Display of Tabs', `RED ZONE` = 'Red Zone', `NO PARKING` = 'No Parking', `PARKED OVER TIME LIMIT` = 'Parked Over Time Limit', `DISPLAY OF PLATES` = 'Display of Plates', `WHITE ZONE` = 'White Zone', `STOP/STAND PROHIBIT` = 'Stop / Stand Prohibited', `STANDNG IN ALLEY` = 'Standing In Alley', `NO STOP/STAND PM` = 'No Stop / Standing', `YELLOW ZONE` = 'Yellow Zone', `NO STOPPING/ANTI-GRIDLOCK ZONE` = 'No Stopping / Anti-Gridlock Zone', `PARKED ON SIDEWALK` = 'Parked on Sidewalk', `OUTSIDE LINES/METER` = 'Outside Lines/Meter', `18 IN. CURB/2 WAY` = '18 Inches from Curb', `BLOCKING DRIVEWAY` = 'Blocking Driveway', `NO EVIDENCE OF REG` = 'No Evidence of Registration', `NO STOP/STANDING` = 'No Stopping / Standing', `FIRE HYDRANT` = 'Fire Hydrant', `DOUBLE PARKING` = 'Double Parking', `NO STOP/STAND` = 'To Stop / Stand', `EXCEED 72HRS-ST` = 'Exceed 72 Hour Street Parking', `COMM VEH OVER TIME LIMIT` = 'Commercial Vehicle Over Time Limit')

#reformting the time
parkingData$time <- sprintf("%04s", as.character(parkingData$Issue.time))
parkingData$time <- sub("([[:digit:]]{2,2})$", ":\\1", parkingData$Issue.time)
#parkingData$time <- as.POSIXct(strptime(parkingData$time, format = "%H:%M"))

#create dateTime format for easier plotting
parkingData$dateTime <- mdy_hm(paste0(parkingData$Issue.Date, " ", parkingData$time))
parkingData$hour <- sprintf("%04s", as.character(parkingData$Issue.time)) %>% substr(1, 2)
parkingData$weekday <- weekdays(parkingData$dateTime)

#geocoding lat and lon by neighborhood

laCountyNeigh <- readOGR("http://s3-us-west-2.amazonaws.com/mappingla.com/downloads/neighborhoods/la_county_simplified.json", "OGRGeoJSON")
laZip <- readOGR(dsn=path.expand("CAMS_ZIPCODE_STREET_SPECIFIC"), layer="CAMS_ZIPCODE_STREET_SPECIFIC")

#reprojecting LA zipcodes to GPS
laZip <- spTransform(laZip, CRS("+init=esri:4326"))

#geocoding for neighborhood
parkingData <- parkingData %>%
  dplyr::select(Longitude, Latitude) %>%
  coordinates %>%
  SpatialPoints(CRS(proj4string(laCountyNeigh))) %over% laCountyNeigh %>%
  dplyr::select(name, slug) %>%
  cbind(parkingData, .)

#geocoding for zipCode
parkingData <- parkingData %>%
  dplyr::select(Longitude, Latitude) %>%
  coordinates %>%
  SpatialPoints(CRS(proj4string(laZip))) %over% laZip %>%
  dplyr::select(Zip_Num) %>%
  cbind(parkingData, .)

#sum of tickets over years
sum(parkingData$Fine.amount, na.rm = TRUE)
# 655667553 From 2012 to 2016, Los Angeles is given out 655 millon dollar in parking tickets

# --- pulling invidual stats for call out in begining of article --- 

#average number of tickets per day
avgDay <- filter(parkingDataDateSum, format(year, "%Y") %in% 2012:2016)
mean(avgDay$total[142:length(avgDay$total)])
# On average there are 7000 tickets written every day.

#number of tickets written in 2015
sum2015CA <- filter(parkingData, format(dateTime, "%Y") %in% 2015 &  RP.State.Plate == "CA")

#number of tickets written in 2015 divided by the number of cars registerd in CA
(length(sum2015CA$row_names) / 7500000) * 100 
# That is about 1/3 of the cars registered is Los Angeles in 2015

# That come to about 150 millon dollars in revenue
sum(sum2015CA$Fine.amount, na.rm = TRUE)

#violation codes
violationCodes <- parkingData %>%
  group_by(Violation.code, Violation.Description) %>%
  dplyr::summarise(total = n()) %>% 
  arrange(-total) %>%
  head(n=25)

top25Viol <- parkingData %>%
  group_by(newViolDescrip) %>%
  dplyr::summarise(total = n()) %>% 
  arrange(-total) %>%
  head(n=25)

top25Viol <- top25Viol$newViolDescrip

#creating theme for my blog
mar <- 10
themeBrettrics <- theme(plot.title = element_text(size = 15, face = "bold", margin = margin(10, 0, 10, 0)),
                        axis.ticks = element_line(color = "black"),
                        axis.text = element_text(size = 11),
                        axis.title = element_text(size = 13, face = "bold"), # margin = margin(mar, mar, mar, mar)),
                        #panel.margin = unit(c(1, 1, 1, 1), "cm"),
                        axis.title.y = element_text(margin = margin(mar, mar, mar, mar)),
                        axis.title.x = element_text(margin = margin(mar, mar, mar, mar)))


# --- creating graph for total tickets by year ----

#piviot of ticket count by day, week, month, year
parkingDataDateSum <- parkingData %>%
  mutate(day = as.Date(cut(dateTime, breaks = "day")), week = as.Date(cut(dateTime, breaks = "week")), month = as.Date(cut(dateTime, breaks = "month")), year = as.Date(cut(dateTime, breaks = "year"))) %>%
  group_by(day, week, month, year) %>%
  dplyr::summarise(total = n())

#creating limits for graphs
dateLim = as.Date(strptime(c("2012-08-01","2016-05-01"), format = "%Y-%m-%d"))

#time line of volume of tickets
ticketsVol <- ggplot(data = parkingDataDateSum, aes(x = week, y = as.numeric(total))) +
                        stat_summary(fun.y = sum, geom = "line", colour = "dark red", size = 1.4) +
                        scale_x_date(date_break = "3 month", date_labels = "%b %Y", limits = dateLim) +
                        labs(title = "Parking Tickets from 2012 to 2016", y = "# of Tickets", x = "Month & Year") +
                        scale_y_continuous(breaks = c(seq(0,100000, 10000))) +
                        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
                        themeBrettrics

#creating svg
svg("plots/ticketsVol.svg", width = 8, height = 6, pointsize = 12)
print(ticketsVol)
dev.off()

# ------------------------------------------------------------------

#mapping Febuary to figure out what happened

mapFeb2016 <- filter(parkingData, format(dateTime, "%Y") %in% 2016) #& format(month, "%m") %in% 2)

mapLAzoom <- get_map(c(lon = -118.2531412, lat = 34.0459534), 
                 zoom = 17,
                 source = "stamen", maptype = 'toner',
                 #maptype = "terrain", 
                 #scale = 1,
                 messaging = FALSE)

ggmap(mapLAzoom) + 
  stat_density2d(aes(y = Latitude, x = Longitude, fill = ..level.., alpha = ..level..), 
                 size = 1, bins = 15, data = mapFeb2016, geom = "polygon") +
  scale_fill_gradient(low = "orange", high = "#8B0000", guide = FALSE) + 
  guides(fill = FALSE, alpha = FALSE) +
  labs(title = "Parketing Tickets of Los Angeles") +
  #facet_wrap(~ Violation.Description, ncol = 2) +
  themeBrettrics +
  theme(axis.title.x=element_blank(), 
        axis.text.x=element_blank(), 
        axis.ticks.x=element_blank(), 
        axis.title.y=element_blank(), 
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank())

#---- ticket velocity by day of month ----

#creating data set to show counts of tickets by day number  
parkingDataDayNum <- parkingData %>% 
  filter(newViolDescrip %in% top25Viol[1:10]) %>%
  mutate(dayNumber = format(dateTime, "%d")) %>% 
  group_by(newViolDescrip, dayNumber) %>%  
  dplyr::summarise(total = n())

#line graph to show velocity by ticket
ticketVelocity <- ggplot(data = parkingDataDayNum, aes(x = dayNumber, y = as.numeric(total), group = newViolDescrip, color = newViolDescrip)) +
                          stat_summary(fun.y = sum, geom = "line",  size = 1.2, alpha = .8) +
                          scale_color_manual(name = "Ticket Type", values = ptol_pal()(10)) +
                          labs(title = "Number of Tickets per Day", y = "# of Tickets", x= "Day of Month") +
                          scale_y_continuous(breaks = c(seq(0,100000, 10000))) +
                          guides(col = guide_legend(nrow = 3, 
                                                    byrow = TRUE, 
                                                    title.position = "top", 
                                                    title.hjust = .5, 
                                                    title.theme = element_text(face = "bold", angle = 0))) +
                          theme(axis.text.x = element_text(angle = 0), legend.position = "bottom") +
                          themeBrettrics

svg("plots/ticketVelocity.svg", width = 8, height = 6, pointsize = 12)    
print(ticketVelocity)
dev.off()

#--- top 10 ticket over years ---

#creating pivot table for tickets time vs violation
parkingDataDateSumViol <- parkingData %>%
  filter(newViolDescrip %in% top25Viol[1:10]) %>%
  mutate(week = as.Date(cut(dateTime, breaks = "week")), month = as.Date(cut(dateTime, breaks = "month")), year = as.Date(cut(dateTime, breaks = "year"))) %>%
  group_by(newViolDescrip, week, month, year) %>%
  dplyr::summarise(total = n())

#creating plot tickets vs violation
ticketsVolByViolation <- ggplot(data = parkingDataDateSumViol, aes(x = week, y = as.numeric(total), group = newViolDescrip, color = newViolDescrip)) +
                          stat_summary(fun.y = sum, geom = "line", size = 1.2, alpha = .8) +
                          scale_x_date(date_break = "3 month", date_labels = "%b %Y", limits = dateLim) +
                          scale_color_manual(name = "Ticket Type", values = ptol_pal()(10)) +
                          guides(col = guide_legend(nrow = 3, 
                                                    byrow = TRUE, 
                                                    title.position = "top", 
                                                    title.hjust = .5, 
                                                    title.theme = element_text(face = "bold", angle = 0))) +
                          scale_y_continuous(breaks = c(seq(0,30000, 5000))) +
                          labs(title = "Ticket Type by Week", y = "# of Tickets", x= "Month") +
                          theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
                          themeBrettrics

svg("plots/ticketsVolByViolation.svg", width = 8, height = 6, pointsize = 12)    
print(ticketsVolByViolation)
dev.off()

# --- heat map of day vs month ---

parkingDataDayHeatMap <- parkingData %>% 
  mutate(dayNumber = format(dateTime, "%d"), month = format(dateTime, "%B"), year = format(dateTime, "%Y")) %>% 
  group_by(year, month, dayNumber) %>%  
  dplyr::summarise(total = n()) %>%
  mutate(rescale = rescale(total)) %>%
  filter(year %in% 2012:2016)

monthOrder <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

#creating heatmap of day vs month
heatMapDayMonth <- ggplot(parkingDataDayHeatMap, aes(dayNumber, month)) + 
                        geom_tile(aes(fill = total), color = "black") +
                        scale_fill_gradient(low = "white", high = "blue") +
                        #guides(colour = FALSE, fill = FALSE) +
                        ylim(rev(monthOrder)) +
                        scale_x_discrete(labels = c(1:31)) +
                        facet_wrap(~ year, ncol = 1) +
                        labs(title = "Number of Tickets by Day from 2012 to 2016", y = "Month", x = "Day") +
                        themeBrettrics +
                        theme(axis.text.x = element_text(angle = 0),
                              axis.text = element_text(size = 8),
                              legend.position = "bottom") +
                        guides(fill = guide_colorbar(title = "# of Tickets",
                                                  title.position = "top", 
                                                  title.hjust = .5, 
                                                  title.theme = element_text(face = "bold", angle = 0),
                                                  barwidth = 8,
                                                  barheight = .8))

#creating svg
svg("plots/heatMapDayMonth.svg", width = 6, height = 10, pointsize = 12)    
print(heatMapDayMonth)
dev.off()

#--- heatmap of time and weekday ----

parkingDataTimeHour <- parkingData %>% filter(newViolDescrip %in% top25Viol[1:9]) %>% 
  group_by(newViolDescrip, weekday, hour) %>% 
  dplyr::summarise(count = n()) %>% 
  arrange(-count) %>% 
  mutate(rescale = rescale(count)) %>%
  plyr::ddply(.(newViolDescrip), transform, rescaleP = rescale(count)) # from plyr

hourOrder <- c(paste0(1:12, 'am'), paste0(1:12, 'pm')) 

hourOrder <- c("", "2am", "", "4am", "", "6am", "", "8am", "", "10am", "", "12pm", "", "2pm", "", "4pm", "", "6pm", "", "8pm", "", "10pm", "", "12am")
dayOrder <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

#heatmap of hour by weekday
heatMapHourDay <- ggplot(parkingDataTimeHour, aes(hour, weekday)) + geom_tile(aes(fill = rescaleP), color = "black") +
                      scale_fill_gradient(low = "white", high = "red") +
                      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                      guides(colour = FALSE, fill = FALSE) +
                      ylim(rev(dayOrder)) +
                      scale_x_discrete(labels = hourOrder) +
                      facet_wrap(~ newViolDescrip) +
                      labs(title = "What is the mostly likely time to get a ticket?", y = "Weekday", x = "Hour") +
                      themeBrettrics +
                      theme(axis.text.x = element_text(size = 7, angle = 45),
                            axis.text = element_text(size = 10),
                            legend.position = "bottom") +
                      guides(fill = guide_colorbar(title = "Legend",
                                                   title.position = "top", 
                                                   title.hjust = .5, 
                                                   title.theme = element_text(face = "bold", angle = 0),
                                                   barwidth = 10))

#creating svg
svg("plots/heatMapHourDay.svg", width = 8, height = 7, pointsize = 12)    
print(heatMapHourDay)
dev.off()

#developing maps and such...

# nTable <- table(parkingData$name) %>% as.data.frame()
# 
# colnames(nTable) <- c("id", "freq")
# fortLA <- fortify(laCountyNeigh, region = "name")
# 
# NeiFort <- merge(fortLA, nTable, by = "id", all.x=TRUE)
# NeiFort <- filter(NeiFort, freq > 5000)
# 
# mapLAother <- get_map("los angeles", 
#                       zoom = 10,
#                       source = "google",
#                       maptype = "roadmap", 
#                       scale = 1,
#                       messaging = FALSE)
# 
# ggmap(mapLAother) + 
#   geom_polygon(data = NeiFort, aes(x=long, y=lat, group=group, fill = freq), size=.2, color = "black", alpha = 1) +
#   scale_fill_gradient(low = "#efedf5", high = "#756bb1") +
#   coord_map()
# 
# carInCity <- parkingData %>%
#   filter(newMake %in% carModel$newMake[1:20], newViolDescrip %in% top25Viol[1:20]) %>%
#   group_by(name, newMake) %>%
#   dplyr::summarise(total = n()) 
  
#--- map plotting + heatmap of day and time
#loop through queries for multiply graphs
# 
# #retreiving map 
# mapLA <- get_map(location = "los angeles", 
#                  zoom = 10,
#                  source = "stamen", maptype = 'toner',
#                  #maptype = "terrain", 
#                  #scale = 1,
#                  messaging = FALSE)
# 
# queryParking <- filter(parkingData, parkingData$newViolDescrip %in% top25Viol[1:9], parkingData$Longitude >= -119)
# 
# ggplot(queryParkingDataTimeHour, aes(hour, weekday)) + 
#   geom_tile(aes(fill = rescale), color = "white") +
#   scale_fill_gradient(low = "#fffedb", high = "#1b0a56") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#   guides(colour = FALSE, fill = FALSE) +
#   ylim(rev(dayOrder)) +
#   scale_x_discrete(labels = hourOrder) +
#   facet_wrap(~ newViolDescrip) +
#   labs(y = "Weekday", x = "Hour") +
#   themeBrettrics +
#   theme(axis.text.x = element_text(size = 9, angle = 45),
#         axis.text = element_text(size = 10))
# 
# #kernal based density map for car make
# #queryParking <- filter(parkingData, parkingData$newMake == "Grumman")
# 
# parkingMapLA <- ggmap(mapLA) + 
#                   stat_density2d(aes(y = Latitude, x = Longitude, fill = ..level.., alpha = ..level..), 
#                                  size = 10, bins = 75, data = queryParking, geom = "polygon") +
#                   scale_fill_gradient(low = "orange", high = "#8B0000", guide = FALSE) + 
#                   guides(fill = FALSE, alpha = FALSE) +
#                   labs(title = "Parketing Tickets of Los Angeles") +
#                   facet_wrap(~ newViolDescrip, ncol = 3) +
#                   themeBrettrics +
#                   theme(axis.title.x=element_blank(), 
#                         axis.text.x=element_blank(), 
#                         axis.ticks.x=element_blank(), 
#                         axis.title.y=element_blank(), 
#                         axis.text.y=element_blank(), 
#                         axis.ticks.y=element_blank())
#   
# svg("plots/parkingMapLA.svg", width = 13, height = 13, pointsize = 12)    
# print(parkingMapLA)
# dev.off()


#creats empty variables for plotting
mapLATemp <- list()
heatMapTemp <- list()
tableCityTemp <- list()
heatMapMapTable <- list()

for (i in 1:10) {
  
  #creating dataset for map and also filters out any of the rows that are geo coded incorrectly
  queryParkingMap <- filter(parkingData, parkingData$newViolDescrip %in% 
                           top25Viol[i], parkingData$Longitude >= -119)
  
  #builds map for plotting
  mapLATemp[[i]] <- ggmap(mapLA) + 
                        stat_density2d(aes(y = Latitude, x = Longitude, fill = ..level.., alpha = ..level..), 
                                       size = 10, bins = 75, data = queryParkingMap, geom = "polygon") +
                        scale_fill_gradient(low = "orange", high = "#8B0000") + 
                        guides(alpha = FALSE) +
                        themeBrettrics +
                        labs(title = "Hotspots in Los Angeles") +
                        theme(axis.title.x=element_blank(), 
                              axis.text.x=element_blank(), 
                              axis.ticks.x=element_blank(), 
                              axis.title.y=element_blank(), 
                              axis.text.y=element_blank(), 
                              axis.ticks.y=element_blank(),
                              legend.position = c(1.005, 1.005), 
                              legend.justification = c(1.005, 1.005),
                              legend.background = element_rect(fill = alpha("grey", .8), color = "black")) +
                        guides(fill = guide_colorbar(title = "Less Tickets     More Tickets",
                                 title.position = "bottom", 
                                 label = FALSE,
                                 title.hjust = .5,
                                 direction = "horizontal",
                                 title.theme = element_text(size = 7, face = "bold", angle = 0),
                                 barwidth = 7,
                                 barheight = 1)) 
  
  #building dataset for heatmap
  queryParkingDataTimeHour <- parkingData %>% filter(newViolDescrip %in% top25Viol[i]) %>% 
    group_by(newViolDescrip, weekday, hour) %>% 
    dplyr::summarise(count = n()) %>% 
    arrange(-count) %>% 
    mutate(rescale = rescale(count)) #%>%
    #plyr::ddply(.(newViolDescrip), transform, rescale = rescale(count)) 
  
  #creating plot for heatmap
  heatMapTemp[[i]] <- ggplot(queryParkingDataTimeHour, aes(hour, weekday)) + 
                          geom_tile(aes(fill = rescale), color = "white") +
                          scale_fill_gradient(low = "#fffedb", high = "#1b0a56") +
                          theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
                          guides(colour = FALSE, fill = FALSE) +
                          ylim(rev(dayOrder)) +
                          scale_x_discrete(labels = hourOrder) +
                          labs(y = "Weekday", x = "Hour", title = "Ticket Times") +
                          themeBrettrics +
                          theme(axis.text.x = element_text(size = 9, angle = 45),
                                axis.text = element_text(size = 10), 
                                legend.position = "bottom") +
                          coord_fixed(2.5) +
                          guides(fill = guide_colorbar(title = "Less Tickets       More Tickets",
                                                        title.position = "top", 
                                                        label = FALSE,
                                                        ticks = FALSE,
                                                        #title.hjust = .5, 
                                                        title.theme = element_text(size = 9, face = "bold", angle = 0),
                                                        barwidth = 9))
  
  #building query for table
  queryCityTable <- parkingData %>% 
    filter(newViolDescrip %in% top25Viol[i]) %>% 
    group_by(name) %>% 
    filter(!is.na(name)) %>%
    dplyr::summarise(count = n()) %>% 
    arrange(-count) %>%
    top_n(10) %>%
    data.frame()
  
  #re-adjusting the coloum names
  colnames(queryCityTable) <- c("Neighborhood", "# of Tickets")
  queryCityTable$`# of Tickets` <-  prettyNum(queryCityTable$`# of Tickets`, big.mark = ",")
  
  #creating the grob for display
  tableCityTemp[[i]] <- tableGrob(queryCityTable, rows = NULL, theme = myt)
  
  #create the title grob for each set of plots
  textTitle <- textGrob(top25Viol[i], gp = gpar(fontsize = 16, font = 8, col = "white"))
  textTop <- ggplot(data = data.frame(1:10)) + 
    annotation_custom(grob = textTitle, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    theme(panel.background = element_rect(fill = "darkslategrey"))
  textTop <- ggplotGrob(textTop)
  #setting the height of the text grob
  panels <- textTop$layout$t[grep("panel", textTop$layout$name)]
  textTop$heights[panels] <- unit(c(1.2), "cm")
  
  heatMapMapTable[[i]] <- arrangeGrob(tableCityTemp[[i]], mapLATemp[[i]], heatMapTemp[[i]], nrow=1, top = textTop, widths=c(.9, 1.2, 1))
  print(i)
}

#exporting SVG
svg("plots/parkingHeatMapMapTable.svg", width = 11, height = 48, pointsize = 12)    
do.call(grid.arrange, list(grobs = heatMapMapTable, ncol=1))
dev.off()



svg("testGrob.svg", width = 11, height = 4.8, pointsize = 12)
print(grid.arrange(tableCityTemp[[i]], mapLATemp[[i]], heatMapTemp[[i]], ncol=3, 
                    top = textTop, 
                   widths=c(.9, 1.2, 1)))
                   #height = c(1,1,.8),
                   #padding = unit(.5, "cm")))
dev.off()

#---- stacked bar chart of ticket and car make ---

carModel <- parkingData %>% group_by(newMake) %>% dplyr::summarise(total = n()) %>% arrange(desc(total))

#transformation of data for bar chart 
parkingDataTicketCarMake <- parkingData %>% filter(newMake %in% carModel$newMake[1:10], newViolDescrip == top25Viol[1:10]) %>% 
  group_by(newViolDescrip, newMake) %>%  
  dplyr::summarise(total = n()) 

#creating plot of 100% stacked bar chart of 
carMakeBar <- ggplot(data = parkingDataTicketCarMake, aes(x = newMake, y = total)) + 
                 geom_bar(aes(fill = newViolDescrip), position = "fill", stat = "identity") +
                 scale_fill_brewer(palette = "Paired") +
                 coord_flip() +
                 labs(title = "Ticket Type by Car Make", x = "Car Make", y = NULL) +
                 scale_y_continuous(labels = percent_format(), breaks = c(seq(0, 1, .1))) +
                 themeBrettrics +
                 theme(axis.text.x = element_text(angle = 0),
                      legend.position = "bottom") +
                 guides(fill = guide_legend(nrow = 3,
                                             title = "Ticket Type",
                                             title.position = "top", 
                                             title.hjust = .5, 
                                             title.theme = element_text(face = "bold", angle = 0)))

svg("plots/carMakeBar.svg", width = 8, height = 6, pointsize = 12)    
print(carMakeBar)
dev.off()

