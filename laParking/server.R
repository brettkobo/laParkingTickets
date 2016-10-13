#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library("RMySQL")
library("shiny")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  #input for latitude
  #output$lat <- renderPrint({ input$numLat })
  #input for longitude
  #output$lon <- renderPrint({ input$numLon })
  
  
  parkingDBconnect = dbConnect(RMySQL::MySQL(),
                       user='root',
                       password='root', 
                       unix.sock="/Applications/MAMP/tmp/mysql/mysql.sock",
                       port = 3306,
                       dbname = "parking_tickets")
  
  parkQuery <- paste0("SELECT * FROM la_parking_tickets LIMIT", input$limitNum)
  
  subParkingData.q <- reactive({
    subParkingData.q <- dbGetQuery(mydb, parkQuery) 
    output$lengthSubParking.q <- renderPrint({ subParkingData.q })
  })
      
})
