library(shiny)
library(leaflet)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)


# Define UI for application that draws a histogram
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  leafletOutput("mymap", width = "100%", height = "100%"), 
  
  absolutePanel(bottom = 10, left = 10,
    actionButton("countryAction", "Reset to Country Map"),
  #   actionButton("stateAction", "State Layer"),
    p(),
  #   leafletOutput("mymap", height = 600),
  
  #   textOutput("click"),
  #   textOutput("mouseover"),
  #   textOutput("mouseout"),
  
    textOutput("indTest1"),
    textOutput("indTest2"),
    textOutput("indTest3"),
    textOutput("indTest4"),
  #   textOutput("boolean"),
  #   textOutput("env"),
  #   textOutput("message"),
     
    
    p()
  
  )
  
))