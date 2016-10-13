#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DBI)
library("rCharts")

ui <- fluidPage(
  #side panel 
  sidebarPanel(
    numericInput("centerPointLat", "Latitude:", 34.048670),
    numericInput("centerPointLon", "Longitude:", -118.251751),
    actionButton(inputId = "submit_loc", label = "Submit")
  ),
  
  mainPanel(
    plotOutput(outputId = "mapOut", width = 400, height = 400),
    showOutput(outputId = "barPlotLA", "polycharts"), #, width = 400, height = 400, click = "plot_click"),
    verbatimTextOutput("x_value")
  )
)

server <- function(input, output, session) {
  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {
      
  #retreiving parking ticket data based on square parametered longitude and latitude  
  query_output <- reactive({
    #establishing the connection
    conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "parking_tickets",
      unix.sock = "/Applications/MAMP/tmp/mysql/mysql.sock",
      username = "root",
      password = "root")
    
    on.exit(dbDisconnect(conn), add = TRUE)
    
    lonDiff <- .0065
    latDiff <- .0055
    
    #parkingQuery <- paste0("SELECT * FROM la_parking_tickets LIMIT ", input$ID, ";")
    subParkingData <- paste("SELECT * FROM la_parking_tickets2016 WHERE Longitude >=", 
                                             input$centerPointLon - lonDiff, "AND Longitude <=",
                                             input$centerPointLon + lonDiff, "AND Latitude >=",
                                             input$centerPointLat - latDiff, "AND Latitude <=",
                                             input$centerPointLat + latDiff, sep = " ")
    
    # query <- sqlInterpolate(conn, parkingQuery, id1 = input$ID)
    
    query_output <- dbGetQuery(conn, subParkingData)
    })
  
    #output$tbl <- renderTable({
    #  query_output()
    #})
    
    mapLAsqaure <- reactive({
      mapLAsqaure <- get_map(location = c(lon = input$centerPointLon, lat = input$centerPointLat), 
                           zoom = 16, 
                           maptype = "roadmap", 
                           scale = 1,
                           messaging = FALSE)
    })
    
    output$mapOut <- renderPlot({
      mapPoints <-  ggmap(mapLAsqaure()) + 
        geom_point(data = query_output(), aes(x = Longitude, y = Latitude, color = "#ff6666"), alpha = .05) + 
        ggtitle("Map of Past Tickets Around You") + 
        guides(colour=FALSE)
      
      mapPoints + coord_cartesian()
    })
    
    # #ggplot for barChart
    # output$barOut <- renderPlot({
    #   setParkingData <- query_output()
    #   setParkingData.violTable <- table(setParkingData$Violation.Description) %>% as.data.frame()
    #   setParkingData.violTable <- setParkingData.violTable[order(-setParkingData.violTable$Freq),]
    #   setParkingData.violTable$Var1 <- factor(setParkingData.violTable$Var1, levels = setParkingData.violTable$Var1[order(setParkingData.violTable$Freq)])
    #   
    #   barPlotLA <- ggplot(data = head(setParkingData.violTable, n = 7)) + 
    #     geom_bar(aes(x = Var1, y = Freq), stat = "identity",fill = "#a3a3ff") +
    #     #coord_flip() +
    #     guides(colour = FALSE, fill = FALSE) +
    #     ggtitle("Most Common Ticket in this Area") 
    #   
    #   print(barPlotLA)
    # })

    output$barPlotLA <- renderChart({
      setParkingData <- query_output()
      setParkingData.violTable <- table(setParkingData$Violation.Description) %>% as.data.frame()
      setParkingData.violTable <- setParkingData.violTable[order(-setParkingData.violTable$Freq),]
      setParkingData.violTable$Var1 <- factor(setParkingData.violTable$Var1, levels = setParkingData.violTable$Var1[order(setParkingData.violTable$Freq)])
      
      p1 <- rPlot(Freq ~ Var1, data = head(setParkingData.violTable, n = 7))
      p1$addParams(dom = 'barPlotLA')
      # barPlotLA <- (data = head(setParkingData.violTable, n = 7)) + 
      #   geom_bar(aes(x = Var1, y = Freq), stat = "identity",fill = "#a3a3ff") +
      #   #coord_flip() +
      #   guides(colour = FALSE, fill = FALSE) +
      #   ggtitle("Most Common Ticket in this Area") 
      
      return(p1)
    })
        
    output$x_value <- renderText({
      if (is.null(input$plot_click$x)) return("")
      else {
        setParkingData <- query_output()
        setParkingData.violTable <- table(setParkingData$Violation.Description) %>% as.data.frame()
        setParkingData.violTable <- setParkingData.violTable[order(-setParkingData.violTable$Freq),]
        setParkingData.violTable$Var1 <- factor(setParkingData.violTable$Var1, levels = setParkingData.violTable$Var1[order(setParkingData.violTable$Freq)])

        lvls <- setParkingData.violTable$Var1[1:7]
        name <- as.character(lvls[round(input$plot_click$y*10)])
        HTML("You've selected <code>", name, " ", round(input$plot_click$x*10), "</code>", input$plot_click$x)
      }
    })
      
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)



