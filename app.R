library(tidyverse)
library(shiny)

library(sf) #load spatial data library
library(mapboxapi) #load mapboxapi library, allows us to make interactive maps
library(leaflet) #this is also required to use the mapbox interactive map
library(leaflet.extras) # used for polygon drawing
library(sp) #this is required for more spatial analysis
library(rgdal) #this helps read kmz/kml files
library(osmdata)
library(purrr)

library(sfnetworks) # for network analysis
library(tidygraph) # for network cleaning
library(dbscan) # for ML algorithm: clustering
library(magrittr) # for extracting data

library(mapview) # for Leaflet map download

#if (is.null(suppressMessages(webshot:::find_phantom()))) { webshot::install_phantomjs() }
################### Algorithm ##################################################

#Set the working directory
#setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #set the working directory to the file save location

# Map box Map Setup #

my_token <- "Please enter token here" #load mapbox token
mb_access_token(my_token, install = TRUE, overwrite = TRUE) #install = True installs your token so you dont have to keep loading your token
readRenviron("~/.Renviron") #refresh environment

mapbox_map <- leaflet() %>% #establish mapbox basemap
  addMapboxTiles(style_id = "light-v10", #AddMapboxTiles converts vector tiles into raster tiles
                 username = "mapbox", #load light-v10 from mabbox username
                 access_token = my_token) #username is not your username, but "mapbox"

# Function: Geo-code Project Location #

project_sf_function <- function(project_address){
  project_coordinates_df <- data.frame(lat = c(""), lon = c(""))
  project_coordinates_df$lon <- mb_geocode (project_address)[1]
  project_coordinates_df$lat <- mb_geocode (project_address)[2]
  
  project_sf <- st_as_sf(project_coordinates_df, coords = c("lon", "lat"), crs = 4326) %>%
    dplyr::select(geometry)
  
  return (project_sf)
}

# Function: Download OSM Street within the Boundary #

streets_function <- function(buffer){
  proj_bb <- st_bbox(buffer)
  q_osm <- opq(bbox = proj_bb) %>% #define bounding box for overpass query 
    add_osm_feature(key = 'highway') %>% # Map features are available in key-value pairs. See potential key-value pairs here: https://wiki.openstreetmap.org/wiki/Map_features
    add_osm_feature(key = 'name') %>%
    osmdata_sf()

  return (q_osm)
}

# Function: Filter and Clip the Streets #

streets_join_function <- function(q_osm, buffer){
  streets <- q_osm$osm_lines %>%
    dplyr::filter( highway %in% c("tertiary", "residential", "unclassified", "secondary", "primary"))
  
  #clip the streets file by the buffer
  streets <- st_intersection(buffer %>%
                               st_transform(crs = 4326), 
                             streets %>%
                               st_transform(crs = 4326))
  
  return (streets)
}

# Function: Transfer to Network and Clean DB #

net_function <- function(streets){
  streets_net <- streets %>% st_cast("LINESTRING") %>% st_as_sf() %>% as_sfnetwork(directed = FALSE)
  
  # Omit the situation of: overpasses and underpasses
  streets_net <- tidygraph::convert(streets_net, to_spatial_subdivision)
  
  # Delete the pseudo nodes with degree = 2
  streets_net <- tidygraph::convert(streets_net, to_spatial_smooth)
  
  return (streets_net)
}

# Function: Delete nodes with degree = 1 on the boundary of the buffer #

delete_board_point_function <- function(streets_net){
  clean_net <- streets_net %>% 
    mutate(deg = igraph::degree(.)) %>%
    filter(deg>=2) 
  
  return (clean_net)
}

# Function: Clustering #
# This is for the duplicated intersections for multiple lane streets
cluster_function <- function(clean_net){
  nodes <- clean_net %>%
    st_as_sf()
  
  nodes$cluster <- nodes %>%
    st_coordinates() %>%
    dbscan::dbscan(eps = 0.00035, minPts = 1) %>%
    extract("cluster") %>%
    extract2(1)
  
  centroids <- nodes %>%
    group_by(cluster) %>%
    summarise() %>%
    ungroup %>%
    st_centroid()
  
  return (centroids)
}


# Function: Calculate Intersection Density #

density_calculation <- function(intersections, buffer){
  area_buffer_sqm <- st_area(buffer) #area in sq meters
  area_buffer_sqkm <- area_buffer_sqm/ (1 * 10^6) #area in sqkm
  
  intersection_density <- as.vector(nrow(intersections)/(area_buffer_sqkm))
  
  return (intersection_density)
}

# Function: Output the Final Result of the Project

intersections_result_function <- function(intersection_density){
  
  intersection_result <- if(intersection_density > 35) TRUE else FALSE
  
  return (intersection_result)
}




##################### UI #######################################################
ui <- bootstrapPage(
  
  # Navbar main page #
  navbarPage('LEED Automation Web App',
             
             # # BD+C #
             # navbarMenu('LEED BD+C v4',
             #            tabPanel('Location and Transportation')
             #            ),
             
             # ND #
             navbarMenu('LEED ND v4',
                        tabPanel('Smart Location and Linkage',
                                 
                                 # Sidebar
                                 sidebarPanel(
                                   width = 3,
                                   p(strong('Criteria Selection')),
                                   tabsetPanel(
                                     id = 'SLL_criteria',
                                     tabPanel(id = 'smart_location', 'Smart Location', 
                                              textInput(inputId = 'street', label = h6('Street'), placeholder = 'project address'),
                                              textInput(inputId = 'city', label = h6('City'), placeholder = 'city'),
                                              textInput(inputId = 'state', label = h6('State'), placeholder = 'state/ province'),
                                              textInput(inputId = 'zipcode', label = h6('Zipcode'), placeholder = 'zipcode'),
                                              actionButton('enter_location', '1: Enter Address'),
                                              actionButton('intersection_analysis', '2: Intersection Analysis'),
                                              downloadButton('download_leaflet', '3: Download Map'),
                                              verbatimTextOutput('intersection_result'),
                                              verbatimTextOutput('intersection_detail'),
                                              verbatimTextOutput('smart_instruction')
                                              
                                              )
                                   )
                                 ),
                                 
                                 # Main page
                                 mainPanel(
                                   conditionalPanel(condition = "input.SLL_criteria == 'Smart Location'",                                    
                                                    div(tags$head(includeCSS('style.css'))),
                                                    leafletOutput('intersection_map', width = '100vw', height = '100vh')
                                                                  )
                                                    
                                           )
                                 ) #tab panel Smart Location
                      ) #, #navbar menu ND
             # Methodology and Algorithm
             # @Al-Jalil: Un-comment when finish this part
             #navbarMenu('Methodology and Algorithm',
             #verbatimTextOutput('method'))
             
             )
)

######################### Server ###############################################
server <- function(input, output, session){
  
  
### Server for LEED ND v4 Smart Location:
  
  # Smart Location Instruction:
  output$smart_instruction <- renderText({
    paste('Step1: Please enter the project address and click button "1"',
          'Step2: Please use the toolbar to draw the boundary of the project',
          'Step3: Please click button "2" and check the result',
          'Step4: Please click button "3" to download the map',
          sep = '\n')}
  )
  
  # Map shown #
  output$intersection_map <- renderLeaflet({
    mapbox_map
    })
  
  # Reactive values storage: project location, boundary and buffer #
  reactiveV <- reactiveValues(project_sf = NULL, boundary_project = NULL, buffer_project = NULL,
                              streets_to_draw = NULL, centroids_to_draw = NULL, output_map = NULL,
                              message = '')
  
  # click - enter location to draw the buffer
  observeEvent(input$enter_location, {
    
    # Location geo-coding
    sl_data$address <- reactive({paste(input$street, input$city, input$state, input$zipcode, sep = ', ')})
    sl_data$address <- as.character(sl_data$address())

    # test: if no information in the street input: then send a note
    if (input$street == "") {
      reactiveV$message <- 'Please enter the project address' 
      output$intersection_result <- renderText({
        reactiveV$message
      })
    } else {    
      reactiveV$message <- 'Please draw the project boundary on the map'
      output$intersection_result <- renderText({
        reactiveV$message
      })  
      
    reactiveV$project_sf <- project_sf_function(sl_data$address)

    # zoom in to location and show the boundary drawing tool
    output$intersection_map <- renderLeaflet({
      mapbox_map %>%
        addCircleMarkers(
        data = reactiveV$project_sf,
        color = "red"
      ) %>%
      # Boundary drawing tool - only polygon and rectangle
        addDrawToolbar(editOptions = editToolbarOptions(),
                       circleOptions = FALSE,
                       markerOptions = FALSE,
                       polylineOptions = FALSE,
                       circleMarkerOptions = FALSE)
    })
    }
  })
  
  observeEvent(input$intersection_map_draw_new_feature, {
    
    # define the layer of the boundary - for future data extraction
    boundary_layer <- input$intersection_map_draw_new_feature
    
    coordinates <- boundary_layer$geometry$coordinates[[1]]
    lat <- c()
    lon <- c()
    
    for (i in c(1: length(coordinates))){
      lat <- c(lat, coordinates[[i]][[1]])
      lon <- c(lon, coordinates[[i]][[2]])
    }
    
    reactiveV$boundary_project <- data.frame(lat = lat, lon = lon) %>%
      st_as_sf(., coords = c("lat", "lon"), crs = 4326)%>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON") %>%
      st_as_sfc() %>%
      st_sf()
    
    
    reactiveV$buffer_project <- st_buffer(reactiveV$boundary_project %>% st_transform(crs = 26919), 804.672 * 1, endCapStyle = 'SQUARE') %>%
      st_transform(crs = 4326)
    
    output$intersection_map <- renderLeaflet({
      mapbox_map %>%
        addPolygons(
          data = reactiveV$boundary_project,
          weight = 1,
          color = '#000000',
          opacity = 0.5,
          fillColor = '#000000',
          fillOpacity = 0.5
        )  %>%
        addPolygons(
          data = reactiveV$buffer_project,
          color = '#A9A9A9',
          fill = FALSE
        )
    
  })
  })

  # click - analysis
  observeEvent(input$intersection_analysis, {
    
    # check: if there is no buffer drawn on the map:
    if (is.null(reactiveV$buffer_project)){
      reactiveV$message <- 'Please draw the project boundary on the map'
      output$intersection_result <- renderText({
        reactiveV$message})
    } else {
      
      # progress bar
      progress <- shiny::Progress$new(style = 'notification', min =1, max = 5)
      progress$set(message = 'Start Analysis', value = 0)
      on.exit(progress$close())
      
      for (i in 1:5){
        progress$set(value = i)
        if (i==1){
          progress$set(message = 'Downloading Streets Data')
          q_osm <- streets_function(reactiveV$buffer_project)
        }
        if (i==2){
          progress$set(message = 'Clipping Streets Data')
          streets <- streets_join_function(q_osm, reactiveV$buffer_project)
        }
        if (i==3){
          progress$set(message = 'Cleaning and Clustering Data')
          streets_net <- net_function(streets)
          clean_net <- delete_board_point_function(streets_net)
        }
        if (i==4){
          progress$set(message = 'Analyzing Intersections')
          centroids <- cluster_function(clean_net)
        }
        if (i==5){
          progress$set(message = 'Generating Intersection Map')
          intersection_density <- density_calculation(centroids, reactiveV$buffer_project)
          intersection_result <- intersections_result_function(intersection_density)
          
          reactiveV$streets_to_draw <- streets_net %>%
            activate('edges') %>%
            as_tibble() %>%
            st_as_sf() %>%
            st_set_crs(4326)
          
          reactiveV$centroids_to_draw <- centroids %>%
            st_as_sf() %>%
            st_set_crs(4326)
        }
        
      }
      
      # output text
      if (intersection_result == 'TRUE'){
        reactiveV$message <- paste('This project meets the requirements of', 'Smart Location Option2', sep='\n')
      }
      else {
        reactiveV$message <- paste('This project does not meet' ,'the requirements of Smart Location Option2', sep='\n')
      }
      
      output$intersection_meet <- renderText({
        reactiveV$message
      })
      
      output$intersection_detail <- renderText({
        paste('The requirments of Smart Location Option2 is:', '35 intersections per square kilometer', ' ',
              'Intersection Density of this project is:', floor(intersection_density), 'per square kilometers', sep='\n')
      })
      
      # output map
      # create leaflet map and store it as reactive map
      reactiveV$output_map <- reactive({
        mapbox_map %>%
          addPolygons(
            data = reactiveV$boundary_project,
            weight = 1,
            color = '#000000',
            opacity = 0.5,
            fillColor = '#000000',
            fillOpacity = 0.5
          ) %>%
          addPolygons(
            data = reactiveV$buffer_project,
            color = '#A9A9A9',
            fill = FALSE
          ) %>%
          addPolylines(
            data = reactiveV$streets_to_draw,
            label = reactiveV$streets_to_draw$name,
            stroke = TRUE,
            color = '#696969',
            weight = 3
          ) %>%
          addCircleMarkers(
            data = reactiveV$centroids_to_draw,
            weight = 5,
            color = '#708090',
            opacity = 0.6,
            fillColor = '#778899',
            fillOpacity = 0.8
          )
      })
      
      output$intersection_map <- renderLeaflet({
        
        reactiveV$output_map()
          
        })
      
    }
    
    })
  
  # Download JPEG
  output$download_leaflet <- downloadHandler(
    filename <- "IntersectionAnalysis.html",
    content <- function(file) {
      mapshot(
        x = reactiveV$output_map(), url = file
      )
    }
  )
  # observeEvent(input$download_leaflet, {
  #   mapshot(reactiveV$output_map(), file = 'intersection.jpeg')
  # })
  # output$download_leaflet <- downloadHandler(
  #   filename = 'intersection.jpeg',
  #   content = function(file){
  #     mapshot(reactiveV$output_map(), file = file)
  #   }
  # )
  
  # Reactive data storage
  sl_data <- reactiveValues( text = 'No Address Input')
    
}

########################### Run App ############################################
shinyApp(ui, server)