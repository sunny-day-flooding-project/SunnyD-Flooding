# Packages to load
library(dplyr)
library(dbplyr)
library(lubridate)
library(shiny)
library(shinydashboard)
library(colourvalues)
library(waiter)
library(sf)
library(leaflet)
library(DT)
library(htmltools)
library(RColorBrewer)
library(dygraphs)
library(magick)
library(xts)
library(RPostgres)
library(DBI)
library(pool)
library(shinyalert)


# Source env variables if working on desktop
# source("C:/Users/Adam Gold/Desktop/postgres_keys.R")

# City names to sho<- on initial load map
place_names <- c("Beaufort, North Carolina") #, "Carolina Beach, North Carolina"

# Boundaries of locations listed in "place_names"
suppressMessages(
  urban_boundaries <- sf::st_read("gis_data/merged_boundaries_coast.shp") %>%
    sf::st_make_valid() %>%
    sf::st_cast("POLYGON") %>%
    mutate(place = paste0(Place_Name,", ",State_Name)) %>% 
    filter(place %in% place_names) %>% 
    group_by(place) %>% 
    summarise() %>% 
    sf::st_as_sf() %>% 
    st_centroid() 
)

# HTML waiting screen for initial load
waiting_screen <- tagList(
    spin_wave(),
    h4("Loading...")
)

# Color palette for water level on site map
pal <- colorNumeric(
    palette = rev(brewer.pal(10,"RdBu")),
    domain = c(-2,2))

# Connect to database
con <- dbPool(
    drv =RPostgres::Postgres(),
    dbname = Sys.getenv("POSTGRESQL_DATABASE"),
    host = Sys.getenv("POSTGRESQL_HOST"),
    port = Sys.getenv("POSTGRESQL_PORT"),
    password = Sys.getenv("POSTGRESQL_PASSWORD"),
    user = Sys.getenv("POSTGRESQL_USER")
)

onStop(function() {
  poolClose(con)
})

database <- con %>% 
  tbl("sensor_data_processed")

#------------------------ Define UI ---------------------------------------
ui <- dashboardPage(
    title = "Data Viewer - Sunny Day Flooding Project", 
    skin = "black",
    header = dashboardHeader(
        title =  HTML('
                <div width="300px">
                      <i class="fas fa-sun" role="presentation" aria-label="sun icon" style="color:#fbb040;position:absolute;left:15px;top:15px"></i><p style="color:white">Sunny Day Flooding Project</p>
                </div>
                      '),
        titleWidth = 350
    ),
    sidebar = dashboardSidebar(
        width = 350,
        sidebarMenu(
            id = "nav",
            menuItem("Map", tabName = "Map", icon = icon("map")),
            
            conditionalPanel(
                condition = "input.nav === 'Map'",
                helpText("Select city from menu", style = "color:white;font-size:12pt;text-align:center"),
                helpText("or click the location on the map",
                         style = "color:white;font-size:12pt;text-align:center"),
                
                #Select city, either Beaufort or Carolina Beach
                selectInput(
                    'city_name',
                    label = h3("Location"),
                    choices = c("",place_names),
                    selected = NULL,
                    multiple = F
                ),
                
                radioButtons("map_layers", label = h3("Map Layers"),
                             choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                             selected = 1)
            ), 
            menuItem("Data", tabName = "Data", icon = icon("database")),
            
            conditionalPanel(
                condition = "input.nav === 'Data'",
                uiOutput("firstSelection", align = "center"),
                uiOutput("secondSelection", align = "center"),
                
                # filter by date, auto is the last three days of data
                # uiOutput("date_filter", align="center"),
                
                # div(actionButton("get_plot_data", "Plot New Dates"), align="center")
                
            ), 
            
            menuItem("Flood Cam", tabName = "Pictures", icon = icon("camera")),
            conditionalPanel(
              condition = "input.nav === 'Pictures'",
              uiOutput("firstCameraSelection", align = "center"),
              uiOutput("secondCameraSelection", align = "center")
              
              # filter by date, auto is the last three days of data
              # uiOutput("date_filter", align="center"),
              
              # div(actionButton("get_plot_data", "Plot New Dates"), align="center")
              
            ),
            menuItem("About", tabName = "About", icon = icon("info-circle"))
            )
        ),
    dashboardBody(
        fluidPage(
            useShinyalert(),
            use_waiter(),
            
        tags$head(tags$link(rel = "shortcut icon", href = "https://tarheels.live/sunnydayflood/wp-content/uploads/sites/1319/2021/02/sunny_d_icon-01-2.png"),
        tags$style(HTML('
        .skin-black .main-header .logo {
          background-color: #13294B;
          border-right: 1px solid #13294B;
        }
        .skin-black .main-header .logo:hover {
          background-color: #13294B;
        }
        
        .skin-black .main-header .navbar {
          background-color: #13294B;
        }
        
        .skin-black .main-header .navbar>.sidebar-toggle {
          color: #fff;
          border-right: 1px solid #13294B;
        }
        
        .skin-black .main-header .navbar .sidebar-toggle:hover {
          color: #fff;
          background: #000;
        }
        
        .main-header .sidebar-toggle {
          font-weight: 200; 
        }
        
        .download_button {
          background-color:#13294b;
          color:white !important;
          font-size: 12pt;
          border:3px;
        }
        
        .download_button:hover {
          background-color:#000000;
          color:white !important;
          font-size: 12pt;
          border: 3px;
        }
        
        #get_plot_data {
          background-color: #fbb040;
          color: white;
          font-size: 12pt;
          font-weight: bold;
          border: 3px;
        }
        
        #get_plot_data:hover {
          background-color: #f1a93e;
          color: white;
          font-size: 12pt;
          font-weight: bold;
          border: 3px;
        }
        
        #get_picture {
          background-color: #fbb040;
          color: white;
          font-size: 12pt;
          font-weight: bold;
          border: 3px;
        }
        
        #get_picture:hover {
          background-color: #f1a93e;
          color: white;
          font-size: 12pt;
          font-weight: bold;
          border: 3px;
        }
        
        .content-wrapper, .right-side {
                                background-color: #ffffff;
        }
                                
        #camera img {max-width: 100%; width: 500px; height: auto}
        
      '))),
        tabItems(
          
    # Mapping tab for overview and site map
    tabItem(tabName = "Map",
        # waiter::waiter_show_on_load(),
            fluidRow(
            leafletOutput(outputId = "m", width="100%",height="calc(100vh - 80px)"),
            style = "z-index: 5;" ## z-index modification
                    )
            ), 
    
    # Tab showing selected data and time series graphs
    tabItem(tabName = "Data",
            
            # Time series interactive plot or data table in separate tabs
            fluidRow(
                     tabsetPanel(
                     id = "data_tabs",
                     tabPanel(
                         "Plot",
                         dygraphOutput("site_level_ts", height = "40vh"),

                         # uiOutput("site_data_panel"),
                         br(),
                         br(),
                         wellPanel(fluidRow(column(4,
                                                   
                                                   uiOutput("date_filter", align = "center"),
                                                   div(actionButton("get_plot_data", "Plot New Dates"), align =
                                                         "center")
                         ),
                         column(4, 
                                selectInput(inputId = "elev_datum", label ="Elevation Datum", choices = c("Road","NAVD88"), selected = "Road")),
                         column(4))) #leafletOutput(outputId = "m2", height = "30vh")
                            ),
                     # tabPanel(
                     #     "Table",
                     #      DT::dataTableOutput("site_data")
                     #        ),
                     tabPanel(
                        "Download",
                        h3("Click the button below to download the selected data.", align="center"),
                        p("Data will be downloaded as a .csv file named",strong("site name_minimum date_maximum date.csv"),align="center"),
                        div(downloadButton("downloadData", "Download Selected Data", class = "download_button"), align="center")
                     ),
                     tabPanel(
                       "Site Description",
                       h3("[Site description goes here]")
                     )
                                )
                    )
            ),
    tabItem(tabName = "Pictures",
            fluidRow(
              tabsetPanel(
                tabPanel(
                  "Pictures",
                     imageOutput(outputId = "camera")
                    ,
            
              wellPanel(fluidRow(column(4
                                        # ,
                                        # uiOutput("date_filter_photos", align = "center"),
                                        # div(actionButton("get_picture", "View Picture"), align =
                                              # "center")
              ),
              column(4, 
                     h3("Additional controls coming soon!")),
              column(4))))))
            
            ),
    tabItem(tabName = "About",
            fluidRow(
              h1("About the project"),
              p("Learn about the project by visiting the", a("Project Website", href = "https://tarheels.live/sunnydayflood/"))
            ))
                )
                )
                )
                )



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    # Initialize wait screen for plotting data
    w <- Waiter$new(id="site_level_ts",
                    html = spin_3k(),
                    color = transparent(.75))   
    
    # Initialize wait screen for image rendering
    w2 <- Waiter$new(id="camera",
                    html = spin_3k(),
                    color = transparent(.75))   
    
    # Setup date filter for Data tab sidebar UI
    output$date_filter <- renderUI(
      dateRangeInput(
        'dateRange',
        label = 'Date range input: yyyy-mm-dd',
        start = Sys.Date() - 2,
        end = Sys.Date() + 1
      )
    )
    
    # Setup date filter for Data tab sidebar UI
    output$date_filter_photos <- renderUI(
      dateRangeInput(
        'dateRangePhotos',
        label = 'Date range input: yyyy-mm-dd',
        start = Sys.Date() - 2,
        end = Sys.Date() + 1
      )
    )
    
    # Popup on load to display info
    shinyalert(title = "Welcome to the data viewer! (beta)",
               text = "Here you can view real-time data from Sunny Day Flooding Project monitoring sites.",
               closeOnClickOutside = FALSE,
               showConfirmButton = T,
               confirmButtonCol = "#fbb040",
               type = "info",
               animation=F,
               size = "s")
    
    # Load Data
    # Update sensor locations with most recent data from database
    sensor_locations <- con %>% 
      tbl("sensor_locations") %>%
      collect() %>%
      left_join(database %>%
                  group_by(sensor_ID) %>%
                  filter(date == max(date, na.rm=T)) %>% 
                  collect(),
                by=c("place","sensor_ID", "sensor_elevation","road_elevation")) %>%
      sf::st_as_sf(coords = c("lng", "lat"), crs = 4269) %>%
      mutate(
        html_popups = paste0( 
          '<div>',
              '<h3 align="center"><strong>Site ',sensor_ID,'</h3></strong>',
          '<h4 align="center">Last level:</h4>',
          '<h3 align="center">',round(road_water_level, digits = 2),'</h3>',
          '<p align="center">',date,'</p>',
          '<p align="center">Click to view data at this site</p>'
        )
      )
      
    camera_locations <- con %>% 
      tbl("camera_locations") %>%
      collect() %>% 
      sf::st_as_sf(coords = c("lng", "lat"), crs = 4269)
    
    # Labels for sensor map 1
    sensor_locations_labels <- as.list(sensor_locations$html_popups)
    
    # Set the reactive values that will be updated by user inputs
    reactive_selection <- reactiveValues(overall_data_sensor = sensor_locations$sensor_ID[1],
                                         overall_data_location = unique(sensor_locations$place)[1],
                                         overall_data_sensor_choices = sensor_locations$sensor_ID[sensor_locations$place == unique(sensor_locations$place)[1]],
                                         
                                         overall_camera_ID = camera_locations$camera_ID[1],
                                         overall_camera_location = unique(camera_locations$place)[1],
                                         overall_camera_choices = camera_locations$camera_ID[camera_locations$place == unique(camera_locations$place)[1]]
                                         )
    
   reactive_min_date <- reactiveVal()
   reactive_min_date(Sys.Date() - 2)
   reactive_max_date <- reactiveVal()
   reactive_max_date(Sys.Date()+1)
    
    observeEvent(input$get_plot_data,{
      data_n <- 10 * 24 * (input$dateRange[2] - input$dateRange[1])
      
      if(data_n > 7440){
        shinyalert::shinyalert(type="error", 
                               title="Data request is too large",
                               text="Please select a shorter time span. Maximum request is 30 days .")
      }
      if(data_n <= 7440){
        reactive_min_date(input$dateRange[1])
        reactive_max_date(input$dateRange[2])
      }
    })
       
    # Reactive value that stores the data from the selected sensor site, filtered by date range
    sensor_data <- reactive({
      req(input$data_sensor)
      database %>%
        filter(sensor_ID %in% !!input$data_sensor) %>%
        filter(date >= !!reactive_min_date() & date < !!reactive_max_date()) %>% # 
        collect()
    })
    

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$data_sensor, reactive_min_date(),reactive_max_date(), ".csv", sep="_")
      },
      content = function(file) {
        write.csv(sensor_data(), file)
      }
    )
    
    # Input drop-downs menus for data page
    output$firstSelection <- renderUI({
        selectInput("data_location", "Select a location", choices = unique(sensor_locations$place), selected = reactive_selection$overall_data_location, selectize = T)
    })

    output$secondSelection <- renderUI({
        selectInput("data_sensor", "Select a sensor", choices = reactive_selection$overall_data_sensor_choices, selected = reactive_selection$overall_data_sensor, selectize = T,multiple = F)
    })

    output$firstCameraSelection <- renderUI({
      selectInput("camera_location", "Select a location", choices = unique(camera_locations$place), selected = reactive_selection$overall_camera_location, selectize = T)
    })
    
    output$secondCameraSelection <- renderUI({
      selectInput("camera_ID", "Select a camera", choices = reactive_selection$overall_camera_choices, selected = reactive_selection$overall_camera_ID, selectize = T,multiple = F)
    })
    
    # Create initial map for "map" tab 
    output$m <-renderLeaflet(leaflet() %>%
                                 addProviderTiles(group = "Positron (default)",provider = providers$CartoDB.Positron) %>%
                                 addProviderTiles(group = "Dark Matter",provider = providers$CartoDB.DarkMatter) %>%
                                 addProviderTiles(group = "Imagery",provider = providers$Esri.WorldImagery) %>%
                                 addTiles(group = "OSM") %>%
                                 setView(lng = -77.360784, lat = 34.576053, zoom = 8) %>%
                                 addCircleMarkers(data = sensor_locations, group = "sensor_site",
                                                  # popup = ~html_popups,
                                                  label = lapply(sensor_locations_labels,HTML),
                                                  labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
                                                  clusterOptions = markerClusterOptions(),
                                                  clusterId = "place",
                                                  layerId = sensor_locations$sensor_ID,
                                                  color = "black", fillColor = ~pal(road_water_level), fillOpacity = 1) %>% 
                                 leaflet::addLegend('bottomright', pal = pal, values = c(-2,2),
                                           title = 'Water level<br>relative to<br>surface (ft)',
                                           opacity = 1) %>% 
                                # addAwesomeMarkers(data = camera_locations, icon=map_icon, label=~as.character(camera_ID)) %>% 
                                 addLayersControl(
                                     baseGroups = c("Positron (default)", "Dark Matter","Imagery", "OSM"),
                                     options = layersControlOptions(collapsed = FALSE)) %>% 
                                 addEasyButton(easyButton(
                                     icon="fa-globe", title="Zoom out",
                                     onClick=JS("function(btn, map){ map.setView({lng:-77.360784, lat:34.576053}, 8); }"),
                                     position = "topright")) 
    )
    
    # reactive value storing the mean coordinates of the sensors from the city selected from the map tab
    map1_selected_location <- reactive({
        req(input$city_name)
        reactive_selection$overall_data_location <- input$city_name
        reactive_selection$overall_camera_location <- input$city_name
        
        sensor_locations %>% 
            filter(place == input$city_name) %>% 
            sf::st_coordinates() %>% 
            as_tibble() %>% 
            summarise(lng = mean(X, na.rm=T),
                      lat = mean(Y, na.rm=T))
    })
    
    # If city name is selected on the map tab, fly to the location
    observeEvent(input$city_name,{
        leafletProxy(mapId = "m") %>% 
            setView(lng = map1_selected_location()[1], lat = map1_selected_location()[2], zoom=16)
    })
    
    # Make camera icon for camera layer. Can be moved somewhere better
    map_icon <- awesomeIcons(
      icon = 'fa-camera',
      iconColor = '#FFFFFF',
      library = 'fa'
    )
    
    # Toggle layers on map
    observeEvent(input$map_layers,{
      if(input$map_layers == 1){
        leafletProxy(mapId = "m") %>% 
          clearGroup("camera_site") %>% 
          addCircleMarkers(data = sensor_locations, group = "sensor_site",
                           # popup = ~html_popups,
                           label = lapply(sensor_locations_labels,HTML),
                           labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
                           clusterOptions = markerClusterOptions(),
                           clusterId = "place",
                           layerId = sensor_locations$sensor_ID,
                           color = "black", fillColor = ~pal(road_water_level), fillOpacity = 1)
      }
      
      if(input$map_layers == 2){
        leafletProxy(mapId = "m") %>% 
          clearGroup("sensor_site") %>% 
          addAwesomeMarkers(data = camera_locations, icon=map_icon, label=~as.character(camera_ID), group = "camera_site",
                            clusterOptions = markerClusterOptions(),
                            clusterId = "place",
                            layerId = sensor_locations$sensor_ID,
          )
          
      }
    })
    
    # # Sensor selected from map tab. Uses map click to update reactive value that controls dropdown menus on Data tab
    # map1_clicked_sensor <- reactive({
    #     req(input$m_marker_click)
    #     input$m_marker_click
    #     }) #Unused
    
    # Updates the choices for the sensor ID on the data tab
    observe({
        reactive_selection$overall_data_sensor_choices <- sensor_locations$sensor_ID[sensor_locations$place == input$data_location]
    })
    
    # Updates the choices for the sensor ID on the data tab
    observe({
      reactive_selection$overall_camera_choices <- camera_locations$camera_ID[camera_locations$place == input$camera_location]
    })
    
    # Updates the reactive values for data sensor and location on the Data tab using click on Map tab
    observe({
      if(input$map_layers == 1){
          reactive_selection$overall_data_sensor <- input$m_marker_click
          reactive_selection$overall_data_location <- unique(sensor_locations$place[sensor_locations$sensor_ID == input$m_marker_click$id])
      }
      
      if(input$map_layers == 2){
        reactive_selection$overall_camera <- input$m_marker_click
        reactive_selection$overall_camera_location <- unique(camera_locations$place[camera_locations$camera_ID == input$m_marker_click$id])
      }
    })

        observeEvent(input$m_marker_click,{
          if(input$map_layers == 1){ 
            updateNavbarPage(session, 
                             inputId = "nav",
                             selected = "Data")
          }
          
          if(input$map_layers == 2){ 
            updateNavbarPage(session, 
                             inputId = "nav",
                             selected = "Pictures")
          }
        })
        
        
        
        # Render plot with selected data
        observe({
          
          # Set min and max date using reactive values from date_filter
          min_date_plot <- as.POSIXct.Date(reactive_min_date())
          max_date_plot <- as.POSIXct.Date(reactive_max_date())
          
          # use spinner for load
          w$show()
          
          # Render the plot
          output$site_level_ts <-  renderDygraph({
            
            # Assign reactive data to a normal object for dygraphs
            plot_sensor_data <- sensor_data()
            
            plot_sensor_stats <- sensor_locations %>% 
              filter(sensor_ID %in% plot_sensor_data$sensor_ID)
            
            # Plot Road datum
            if(input$elev_datum == "Road") {
              x <- xts::xts(plot_sensor_data$road_water_level,
                            order.by = plot_sensor_data$date,
                            tzone = "UTC")
              
              tzone(x) <- "America/New_York"
              road_elevation_limit <- 0
              sensor_elevation_limit <- plot_sensor_stats$sensor_elevation - plot_sensor_stats$road_elevation
              y_axis_range <- c(sensor_elevation_limit-0.5, ifelse(max(x[1], na.rm=T) > road_elevation_limit, max(x[1], na.rm=T) + 0.5, road_elevation_limit+0.5))
            }
            
            # Plot NAVD88 Datum
            if(input$elev_datum == "NAVD88") {
              x <- xts::xts(plot_sensor_data$sensor_water_level,
                            order.by = plot_sensor_data$date,
                            tzone = "UTC")
              
              tzone(x) <- "America/New_York"
              road_elevation_limit <- plot_sensor_stats$road_elevation
              sensor_elevation_limit <- plot_sensor_stats$sensor_elevation
              y_axis_range <- c(sensor_elevation_limit-0.5, ifelse(max(x[1], na.rm=T) > road_elevation_limit, max(x[1], na.rm=T) + 0.5, road_elevation_limit + 0.5))
              
            }
            
            dygraph(x, main = plot_sensor_stats$sensor_ID) %>%
              dyAxis("y", label = "Water level (ft)", valueRange = y_axis_range) %>%
              dyAxis("x", label = "Date (EST/EDT)") %>%
              dyOptions(colors = "#3266a8",
                        gridLineColor = "#e0e0e0",
                        strokeWidth = 2) %>%
              dyLimit(road_elevation_limit, color = "red", label = "Road Elevation") %>%
              dyLimit(sensor_elevation_limit, color = "black", label = "Sensor Elevation") %>%
              dyUnzoom() %>%
              dyCrosshair(direction = "vertical") %>%
              dySeries("V1", label = "Level") 
            
          })
          

        # # Render table with selected data
        #     output$site_data <- renderDataTable(
        #       sensor_data() %>% 
        #         mutate(date_EST_EDT = lubridate::with_tz(date, tzone = "America/New_York")) %>% 
        #         dplyr::select(date, road_water_level, sensor_water_level) %>% 
        #       mutate_if(.predicate = is.numeric, .funs = "round", digits = 2)
        #     )
            
          # Hide spinner after load
            w$hide()
        })
        
        observe({
          req(input$camera_ID)
          w2$show()
            output$camera <- renderImage({
              # A temp file to save the output. It will be deleted after renderImage
              # sends it, because deleteFile=TRUE.
              outfile <- tempfile(fileext='.jpg')

              magick::image_read(paste0("https://photos-sunnydayflood.cloudapps.unc.edu/public/",input$camera_ID,".jpg")) %>% 
                magick::image_write(path = outfile)
              
              # Return a list
              list(src = outfile,
                   alt = paste0("Latest picture from",input$camera_ID),
                   height = "100%")
              
            }, deleteFile = T)
            w2$hide()
        })
}

# Run the application
shinyApp(ui = ui, server = server)
