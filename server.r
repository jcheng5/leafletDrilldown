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
states <- readOGR(dsn = "ne_10m_admin_1_states_provinces/ne_10m_admin_1_states_provinces.shp", 
                  layer = "ne_10m_admin_1_states_provinces")

# convert NA's to "Unknown" in columns we are using
countries@data$NAME_LONG <- factor(countries@data$NAME_LONG, levels = c(levels(countries@data$NAME_LONG), "Unknown"))
countries@data$NAME_LONG[is.na(countries@data$NAME_LONG)] <- "Unknown"
countriesDF <- countries@data


states@data$geonunit <- factor(states@data$geonunit, levels = c(levels(states@data$geonunit), "Unknown"))
states@data$geonunit[is.na(states@data$geonunit)] <- "Unknown"
states@data$name <- factor(states@data$name, levels = c(levels(states@data$name), "Unknown"))
states@data$name[is.na(states@data$name)] <- "Unknown"
statesSub <- states
statesSubDF <- states@data


# simplify shapefile for improved performance
tol <- 0.0001

countriesSimplified <- gSimplify(countries, tol, topologyPreserve = TRUE)
countries <- SpatialPolygonsDataFrame(countriesSimplified, data = countriesDF) 

statesSimplified <- gSimplify(statesSub, tol, topologyPreserve = TRUE)
states <- SpatialPolygonsDataFrame(statesSimplified, data = statesSubDF) 


# Create the country map at app initialization
paletteCountry <- colorQuantile("YlGnBu", countries$POP_EST, n = 10)
info <- paste0("Hover over a country to view information.")

map <- leaflet(countries) %>%
  addTiles(group = "OSM") %>%
  addProviderTiles(provider = "Stamen.TonerLite", group = "Stamen Toner Lite") %>%
  setView(lng = 0, lat = 0, zoom = 2) %>%
  addPolygons(layerId = ~NAME_LONG, 
              stroke = FALSE, fillOpacity = 0.5, smoothFactor = 0.5,
              color = ~paletteCountry(POP_EST)
  ) %>%
  addLegend(layerId = "legend", title = c("Population"), position = "bottomright", pal = paletteCountry, values = ~POP_EST) %>%
  addLayersControl(baseGroups = c("OSM", "Stamen Toner Lite"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  addControl(layerId = "infoControl", html = info, position = "topright", className = "info") %>% 
  mapOptions(zoomToLimits="always")
  


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
      
      output$indTest1 <- renderPrint({paste0("one = ", ind)})
      #output$boolean <- renderPrint({"IF"})
      infoCountry <- paste0("<b>Country: </b>", countriesDF[countriesDF$NAME_LONG == input$mymap_shape_mouseover$id,]$NAME_LONG, "<br><b>Population: </b>", format(x = countries[countriesDF$NAME_LONG == input$mymap_shape_mouseover$id,]$POP_EST, format = "d", big.mark = ","))
      
      leafletProxy("mymap") %>%
        removeControl(layerId = "infoControl") %>%
        addControl(layerId = "infoControl", html = infoCountry, position = "topright", className = "info")
  
      #output$env <- renderPrint({parent.frame()})
      
    } else{
      output$indTest1 <- renderPrint({paste0("one = ", ind)})
      #output$boolean <- renderPrint({"ELSE"})
  
      #statesSub <- states[factor(states$geonunit) == input$mymap_shape_click$id,]
      
      infoState <- paste0("<b>State: </b>", statesSubDF[statesSubDF$name == input$mymap_shape_mouseover$id,]$name, "<br><b>Name Length: </b>", statesSubDF[statesSubDF$name == input$mymap_shape_mouseover$id,]$name_len)

      leafletProxy("mymap") %>%
        removeControl(layerId = "infoControl") %>%
        addControl(layerId = "infoControl", html = infoState, position = "topright", className = "info")
      }
    
      #output$env <- renderPrint({parent.frame()})
    
  }, ignoreNULL = TRUE)


  ### if click on country, render state map for just that country
  tryCatch({
  if(ind == "c"){
    output$indTest2 <- renderPrint({paste0("two = ", ind)})
    observeEvent(input$mymap_shape_click, {
      
    ### State

      statesSub <<- states[grepl(input$mymap_shape_click$id, factor(states$geonunit)),]
      statesSubDF <<- statesSub@data
      paletteState <- colorFactor(palette = "YlGnBu", domain = sort(statesSub$name_len))
      info <- paste0("Hover over a state to view information.")
      
      leafletProxy(mapId = "mymap", data = statesSub) %>%
        clearShapes() %>%
        addPolygons(layerId = ~name,
                    stroke = FALSE, 
                    fillOpacity = 0.5, 
                    smoothFactor = 0,
                    color = ~paletteState(name_len)
        ) %>%
        removeControl(layerId = "legend") %>%
        addLegend(title = c("Name Length"), position = "bottomright", pal = paletteState, values = ~sort(name_len)) %>%
        removeControl(layerId = "infoControl") %>%
        addControl(layerId = "infoControl", html = info, position = "topright", className = "info")
      
      output$indTest2 <- renderPrint({paste0("two = ", ind)})
      
      #output$env <- renderPrint({parent.frame()})
      
      ind <- "s"
      ind <<- "s"
      
    }, ignoreNULL = TRUE)
  
  }
  
  }, error = function(e) return())

   
  ### if select state action button, render state map
  observeEvent(input$countryAction, {
    
    output$mymap <- renderLeaflet({
      map
      
    })
    
    ind <<- "c"
    
  }, ignoreNULL = TRUE)

  
})
