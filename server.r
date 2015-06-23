library(shiny)
library(leaflet)
library(rgdal) #for reading/writing geo files
library(rgeos) #for simplification
library(sp)


# set the working directory
#setwd("<SET TO YOUR WORKING DIRECTORY>")


# load country and state data; convert NA's to "Unknown"
countries <- readOGR(dsn = "ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp", 
                     layer = "ne_10m_admin_0_countries") 
countries@data$NAME_LONG <- factor(countries@data$NAME_LONG, levels = c(levels(countries@data$NAME_LONG), "Unknown"))
countries@data$NAME_LONG[is.na(countries@data$NAME_LONG)] <- "Unknown"
countriesDF <- countries@data

states <- readOGR(dsn = "ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp", 
                  layer = "ne_10m_admin_1_states_provinces")
states@data$geonunit <- factor(states@data$geonunit, levels = c(levels(states@data$geonunit), "Unknown"))
states@data$geonunit[is.na(states@data$geonunit)] <- "Unknown"
states@data$name <- factor(states@data$name, levels = c(levels(states@data$name), "Unknown"))
states@data$name[is.na(states@data$name)] <- "Unknown"
statesSub <- states
statesSubDF <- states@data

# Create the country map at app initialization
paletteCountry <- colorQuantile("YlGnBu", countries$POP_EST, n = 10)
info <- paste0("Hover over a country to view information.")

map <- leaflet(countries) %>%
  addProviderTiles(provider = "Stamen.TonerLite") %>%
  addPolygons(layerId = ~NAME_LONG, 
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
              color = ~paletteCountry(POP_EST)
  ) %>%
  addLegend(title = c("Population"), position = "bottomright", pal = paletteCountry, values = ~POP_EST) %>%
  addControl(layerId = "infoControl", html = info, position = "topright", classes = "info")


############################################################################
# Define server logic
shinyServer(function(input, output, session) {
  
  ### see what these lists look like
#   output$click <- renderPrint({input$mymap_shape_click$id})
#   output$mouseover <- renderPrint({input$mymap_shape_mouseover})
#   output$mouseout <- renderPrint({input$mymap_shape_mouseout})
  

  #output$indTest <- renderPrint({ind})
  
  # Initialize ind variable in the global environment
  ind <- "c"
  
  ### initialize map at country level
  output$mymap <- renderLeaflet({
    map
  })

  # if the map is at the country level, then display country information
  observeEvent(input$mymap_shape_mouseover, {
    
    
    #output$boolean <- renderPrint({"NONE"})
    
    if(ind == "c"){
      

      #output$boolean <- renderPrint({"IF"})
      infoCountry <- paste0("<b>Country: </b>", countriesDF[countriesDF$NAME_LONG == input$mymap_shape_mouseover$id,]$NAME_LONG, "<br><b>Population: </b>", format(x = countries[countriesDF$NAME_LONG == input$mymap_shape_mouseover$id,]$POP_EST, format = "d", big.mark = ","))
      
      leafletProxy("mymap") %>%
        removeControl(layerId = "infoControl") %>%
        addControl(layerId = "infoControl", html = infoCountry, position = "topright", classes = "info")
  
      #output$env <- renderPrint({parent.frame()})
      
    } else{
        
      #output$boolean <- renderPrint({"ELSE"})
  
      #statesSub <- states[factor(states$geonunit) == input$mymap_shape_click$id,]
      
      infoState <- paste0("<b>State: </b>", statesSubDF[statesSubDF$name == input$mymap_shape_mouseover$id,]$name, "<br><b>GN Level: </b>", statesSubDF[statesSubDF$name == input$mymap_shape_mouseover$id,]$gn_level)
      output$statesSub <- renderPrint({print(infoState)})
      leafletProxy("mymap") %>%
        removeControl(layerId = "infoControl") %>%
        addControl(layerId = "infoControl", html = infoState, position = "topright", classes = "info")
      }
    
      #output$env <- renderPrint({parent.frame()})
    
  }, ignoreNULL = TRUE)


  ### if click on country, render state map for just that country
  if(ind == "c"){
    
    observeEvent(input$mymap_shape_click, {
      
    ### State

      statesSub <<- states[grepl(input$mymap_shape_click$id, factor(states$geonunit)),]
      statesSubDF <<- statesSub@data
      paletteState <- colorFactor(palette = "YlGnBu", domain = statesSub$gn_level)
      info <- paste0("Hover over a state to view information.")
      
      leafletProxy(mapId = "mymap", data = statesSub) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(layerId = ~name,
                    stroke = FALSE, 
                    fillOpacity = 0.5, 
                    smoothFactor = 0,
                    color = ~paletteState(gn_level)
        ) %>%
        addLegend(title = c("GN Level"), position = "bottomright", pal = paletteState, values = ~gn_level) %>%
        addControl(layerId = "infoControl", html = info, position = "topright", classes = "info")

      ind <<- "s"
      
      #output$indTest <- renderPrint({ind})
      
      #output$env <- renderPrint({parent.frame()})
    
    }, ignoreNULL = TRUE)
  
  }

   
  ### if select state action button, render state map
  observeEvent(input$countryAction, {
    
    output$mymap <- renderLeaflet({
      map
      
    })
    
    ind <<- "c"
    
  }, ignoreNULL = TRUE)

  
})
