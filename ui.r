library(shiny)
library(leaflet)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  p(),
  actionButton("countryAction", "Reset to Country Map"),
#   actionButton("stateAction", "State Layer"),
  p(),
#   leafletOutput("mymap", height = 600),

#   textOutput("click"),
#   textOutput("mouseover"),
#   textOutput("mouseout"),

#   textOutput("indTest"),
#   textOutput("boolean"),
#   textOutput("env"),
#   textOutput("message"),
  
  leafletOutput("mymap"),  
  
  p()

))