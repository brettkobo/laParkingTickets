#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pick a number"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       #numericInput("numLat", label = h3("Latitude"), value = 34.5),
       #numericInput("numLon", label = h3("Longitude"), value = -118.5)
       
       numericInput("limitNum", label = h3("Limit of Database"), value = 50), 
       submitButton("Update View")
    ),
    

    # Show a plot of the generated distribution
    mainPanel(
       #hr(),
       #fluidRow(column(3, verbatimTextOutput("lat"))),
       #hr(),
       #fluidRow(column(3, verbatimTextOutput("lon"))),
       hr(),
       verbatimTextOutput("lengthSubParking.q")
    )
  )
))
