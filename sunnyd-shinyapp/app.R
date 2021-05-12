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
library(highcharter)
library(magick)
library(xts)
library(RPostgres)
library(DBI)
library(pool)
library(shinyalert)
library(stringr)
library(shinydisconnect)
library(tippy)
library(httr)
library(shinyWidgets)


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
    palette = rev(brewer.pal(10,"RdYlBu")),
    domain = c(-2,0))

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

global <- getOption("highcharter.global")
global$useUTC <- FALSE
global$timezoneOffset <- -300
options(highcharter.global = global)

get_thirdparty_metadata <- function(location){
  if(location == "Beaufort, North Carolina"){
    return(tibble("entity"= "NOAA",
                  "url" = "https://tidesandcurrents.noaa.gov/waterlevels.html?id=8656483",
                  "types"=list(c("obs","pred"))))
  }
}

# Define functions to get local water levels from each location
get_thirdparty_wl <- function(location, type, min_date, max_date) {
  # Beaufort uses NOAA for water levels and predictions
  if(location == "Beaufort, North Carolina"){
      request <-
        httr::GET(
          url = "https://api.tidesandcurrents.noaa.gov/api/prod/datagetter/",
          query = list(
            "station" = "8656483",
            "begin_date" = min_date,
            "end_date" = max_date,
            "product" = ifelse(type == "obs","water_level","predictions"),
            "units" = "english",
            "datum" = "NAVD",
            "time_zone" = "gmt",
            "format" = "json",
            "application" = "UNC_Institute_for_the_Environment, https://github.com/acgold"
          )
        )
      
      if(type == "obs") {
        wl <-
          tibble::as_tibble(jsonlite::fromJSON(rawToChar(request$content))$data)
        colnames(wl)[1:2] <- c("date", "level_ft_navd88")
      }
      
      if(type == "pred") {
        wl <-
          tibble::as_tibble(jsonlite::fromJSON(rawToChar(request$content))$predictions)
        colnames(wl)[1:2] <- c("date", "level_ft_navd88")
      }
      
      if(nrow(wl) == 0){
        stop("No data available")
      }
      
      wl <- wl %>%
        transmute(
          location = "Beaufort, North Carolina",
          date = ymd_hm(date),
          level = as.numeric(level_ft_navd88),
          entity = ifelse(type == "obs", "NOAA Observed","NOAA Predictions"),
          notes = ifelse(type == "obs", "observation","prediction")
        )
      return(wl)
    }
}

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
                div(style="border-left-style: solid; border-left-width: medium; border-left-color: white;",
                  p("Use the controls on the sidebar to navigate to a study site and select a map layer", style = "color:white;font-size:12pt;width:250px;white-space: break-spaces;margin-left: auto;margin-right: auto; font-style:italic"),
                  br(),
                  p("Hover and click on points explore live data and pictures", style = "color:white;font-size:12pt;width:250px;white-space: break-spaces;margin-left: auto;margin-right: auto; font-style:italic"),

                #Select city, either Beaufort or Carolina Beach
                selectInput(
                    'city_name',
                    label = h4("Location"),
                    choices = c("",place_names),
                    selected = NULL,
                    multiple = F,
                    selectize = F
                ),
                
                radioButtons("map_layers", label = h4("Map Layers"),
                             choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                             selected = 1)
            )
            ), 
            menuItem("Data", tabName = "Data", icon = icon("database")),
            
            conditionalPanel(
                condition = "input.nav === 'Data'",
                div(style="border-left-style: solid; border-left-width: medium; border-left-color: white;",
                    
                uiOutput("firstSelection", align = "center"),
                uiOutput("secondSelection", align = "center"),
                )
            ), 
            
            menuItem("Flood Cam", tabName = "Pictures", icon = icon("camera")),
            conditionalPanel(
              condition = "input.nav === 'Pictures'",
              div(style="border-left-style: solid; border-left-width: medium; border-left-color: white;",
                  
              uiOutput("firstCameraSelection", align = "center"),
              uiOutput("secondCameraSelection", align = "center")
              )

            ),
            menuItem("About", tabName = "About", icon = icon("info-circle"))
            )
        ),
    dashboardBody(
        fluidPage(
          disconnectMessage(
            text = "Your session has timed out! Try refreshing the page.",
            refresh = "Refresh",
            background = "#FFFFFF",
            colour = "#000000",##000000
            refreshColour = "#337AB7",
            overlayColour = "#000000",
            overlayOpacity = 0.25,
            width = 450,
            top = "center",
            size = 24,
            css = ""
          ),
            useShinyalert(),
            use_waiter(),
            waiter::waiter_show_on_load(html = spin_3k(),
                                        color = transparent(0)),
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
        
        .nav-tabs-custom .nav-tabs li.active {
          border-top-color: black;
        }
          
      '))),
        tabItems(
          
    # Mapping tab for overview and site map
    tabItem(tabName = "Map",
        # waiter::waiter_show_on_load(),
            fluidRow(
              leafletOutput(outputId = "m", width="100%",height="calc(100vh - 80px)"),
            )
            ), 
    
    # Tab showing selected data and time series graphs
    tabItem(tabName = "Data",
            # Time series interactive plot or data table in separate tabs
            fluidRow(uiOutput("flood_status", align="center")),
            fluidRow(
                     tabBox(width="100%",
                     id = "data_tabs",
                     tabPanel(
                         "Plot",
                         highchartOutput("site_level_ts", height = "50vh", width = "100%"),
                         wellPanel(fluidRow(column(4,
                                                   
                                                   uiOutput("date_filter", align = "center"),
                                                   div(actionButton("get_plot_data", "Plot New Dates"), align =
                                                         "center")
                         ),
                         column(4, 
                                selectInput(inputId = "elev_datum", label =h4("Elevation Datum"), selectize = F,choices = c("Road","NAVD88"), selected = "Road")),
                         column(4,
                                h4("Local water levels",tippy(icon("info-circle",style="font-size:14px"), h5("Click the button below to add nearby downstream water levels to the plot.",br(),br(),"Adding these data can help visualize when flooding may occur.",br(),br(),"Turning this option on may slow down the drawing of the plot.",style = "text-align:left"))),
                                switchInput(inputId = "view_3rdparty_data", value = F, size = "mini"),
                                # checkboxInput(inputId = "view_3rdparty_data", label = "Check this box to add local water level data to the plot", value = FALSE),
                                uiOutput(outputId="thirdparty_info")
                            )))),
                     tabPanel(
                       "Site Description",
                       h3("Site description coming soon"),
                       actionButton("view_on_map_wl", label = "View site on map", icon = icon("map"))
                     ),
                     tabPanel(
                        "Download",
                        h3("Click the button below to download the selected data.", align="center"),
                        p("Data will be downloaded as a .csv file named",strong("site name_minimum date_maximum date.csv"),align="center"),
                        div(downloadButton("downloadData", "Download Selected Data", class = "download_button"), align="center")
                     )
                                )
                    )
            ),
    tabItem(tabName = "Pictures",
            fluidRow(
              tabsetPanel(
                tabPanel(
                  "Pictures",
                  fluidRow(column(6,
                     imageOutput(outputId = "camera")),
                  column(6,
                         h1("Water detected?",tippy(icon("info-circle",style="font-size:16px"), tooltip = div(h5("We are using machine learning to detect water on the road surface using our flood cams.",br(),br(),"The model used here is trained to distinguish pictures that have water in them from those that do not. Cars, salt water stains, sun glare, and other features in the photos can decrease the accuracy of the model.",br(),br(),"This model is still in development and is for informational purposes only."), style = "text-align:left"))),
                         div(p("In Development",style="color:white;text-align: center"),style="background-color:#fbb040;width:125px;border-radius: 20px"),
                         p("The chart below shows the probability that water has been detected in this picture with our machine learning model"),
                  highchartOutput(outputId ="tf_predict")))
                  ),
              tabPanel(
                "Site Description",
                h3("Site description coming soon"),
                actionButton("view_on_map_camera", label = "View site on map", icon = icon("map"))
                
              )
              
              ))
            
            ),
    tabItem(tabName = "About",
            fluidRow(
              h1("The Sunny Day Flooding Project"),
              p("We are a group of researchers from",strong("UNC Chapel Hill"), "and", strong("NC State"), "that work with NC communities to measure, model, and better understand the causes and impacts of chronic flooding."),
              strong("Visit our", a("Project Website", href = "https://tarheels.live/sunnydayflood/"), "to learn more about the project"),
              
              h1("Data Viewer"),
              p("This data viewer shows real-time water level data and pictures from our study sites."),
              p("We are constantly optimizing the data viewer and would love to hear your feedback about the site."),
              strong("Send us an ", a("email",href = "mailto:sunnydayflood@gmail.com"), "with any constructive comments!")
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
        label = h4('Date range'),
        start = Sys.Date() - 2,
        end = Sys.Date() + 1,
        max = Sys.Date() + 3
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
               text = "Here you can view real-time data from Sunny Day Flooding Project monitoring sites.\n\n Data and images are preliminary and for informational purposes only",
               closeOnClickOutside = FALSE,
               showConfirmButton = T,
               confirmButtonCol = "#fbb040",
               confirmButtonText = "OK",
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
                  filter(qa_qc_flag == F) %>% 
                  filter(date == max(date, na.rm=T)) %>% 
                  collect(),
                by=c("place","sensor_ID", "sensor_elevation","road_elevation")) %>%
      mutate(date_lst = lubridate::with_tz(date, tzone = "America/New_York")) %>% 
      sf::st_as_sf(coords = c("lng", "lat"), crs = 4269) %>%
      mutate(
        html_popups = paste0( 
          '<div>',
              '<h3 align="center"><strong>Site ',sensor_ID,'</h3></strong>',
          '<h4 align="center">Last level:</h4>',
          '<h3 align="center">',round(road_water_level, digits = 2),'</h3>',
          '<p align="center">',paste0(format(date_lst, "%I:%M %p", usetz = T)," - ",format(date_lst, "%b %d, %Y")),'</p>',
          '<p align="center">Click to view data at this site</p>'
        )
      ) %>% 
      mutate(flood_status = road_water_level > (road_elevation - alert_threshold))
      
    camera_locations <- con %>% 
      tbl("camera_locations") %>%
      collect() %>% 
      left_join(con %>% 
                  tbl("photo_info") %>% 
                  filter(DateTimeOriginalUTC == max(DateTimeOriginalUTC, na.rm=T)) %>% 
                  collect(), by = c("camera_ID")) %>% 
      mutate(date_lst = lubridate::with_tz(DateTimeOriginalUTC , tzone = "America/New_York")) %>% 
      sf::st_as_sf(coords = c("lng", "lat"), crs = 4269) %>% 
      mutate(
        html_popups = paste0( 
          '<div>',
          '<h3 align="center"><strong>Camera ',camera_ID,'</h3></strong>',
          '<h4 align="center">Last picture:</h4>',
          '<h4 align="center">',paste0(format(date_lst, "%I:%M %p", usetz = T)," - ",format(date_lst, "%b %d, %Y")),'</h4>',
          '<p align="center">Click to view this camera</p>'
        )
      )
    
    # Labels for sensor map 1
    sensor_locations_labels <- as.list(sensor_locations$html_popups)
    
    # Labels for sensor map 1 camera
    camera_locations_labels <- as.list(camera_locations$html_popups)
      
    # Set the reactive values that will be updated by user inputs
    reactive_selection <- reactiveValues(overall_data_sensor = sensor_locations$sensor_ID[1],
                                         overall_data_location = unique(sensor_locations$place)[1],
                                         overall_data_sensor_choices = sensor_locations$sensor_ID[sensor_locations$place == unique(sensor_locations$place)[1]],
                                         
                                         overall_camera_ID = camera_locations$camera_ID[1],
                                         overall_camera_location = unique(camera_locations$place)[1],
                                         overall_camera_choices = camera_locations$camera_ID[camera_locations$place == unique(camera_locations$place)[1]]
                                         )
    
   reactive_min_date <- reactiveVal()
   reactive_min_date((Sys.Date() - 2) %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
   reactive_max_date <- reactiveVal()
   reactive_max_date((Sys.Date() + 1) %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
    
    observeEvent(input$get_plot_data,{
      data_n <- 10 * 24 * (input$dateRange[2] - input$dateRange[1])
      
      if(data_n > 7440){
        shinyalert::shinyalert(type="error", 
                               title="Data request is too large",
                               text="Please select a shorter time span. Maximum request is 30 days .")
      }
      if(data_n <= 7440){
        reactive_min_date(input$dateRange[1] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
        reactive_max_date(input$dateRange[2] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
        }
    })
       
    # Reactive value that stores the data from the selected sensor site, filtered by date range
    sensor_data <- reactive({
      req(input$data_sensor)
      database %>%
        filter(sensor_ID %in% !!input$data_sensor) %>%
        filter(date >= !!reactive_min_date() & date < !!reactive_max_date(),
               qa_qc_flag == F) %>% # 
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
        selectInput("data_location", "Select a location", choices = unique(sensor_locations$place), selected = reactive_selection$overall_data_location, selectize = F)
    })

    output$secondSelection <- renderUI({
        selectInput("data_sensor", "Select a sensor", choices = reactive_selection$overall_data_sensor_choices, selected = reactive_selection$overall_data_sensor, selectize = F,multiple = F)
    })

    output$firstCameraSelection <- renderUI({
      selectInput("camera_location", "Select a location", choices = unique(camera_locations$place), selected = reactive_selection$overall_camera_location, selectize = F)
    })
    
    output$secondCameraSelection <- renderUI({
      selectInput("camera_ID", "Select a camera", choices = reactive_selection$overall_camera_choices, selected = reactive_selection$overall_camera_ID, selectize = F,multiple = F)
    })
    
    flood_status_reactive <- reactive({
      req(input$data_sensor)
      status <- sensor_locations %>% filter(sensor_ID == input$data_sensor) %>% pull(flood_status)
      # print(status)
      return(status)
    })
    
    time_since_last_measurement <- reactive({
      req(input$data_sensor)
      last_datetime <- sensor_locations %>% filter(sensor_ID == input$data_sensor) %>% pull(date)
      time_difference <- time_length(((Sys.time() %>% with_tz("UTC"))-last_datetime), unit="minute")
      # print(time_difference)
      return(round(time_difference))
    })
    
    
    # showing the banner of flood alert status
    output$flood_status <- renderUI({
      if(flood_status_reactive()){
        if(time_since_last_measurement() < 120){
          div(width = "100%", style="background-color:#e1142c;height:25px;padding:2.5px 2.5px;margin-bottom:5px", 
              p("Status: ",strong("FLOODING",style="color:white;"),tippy(icon("info-circle"), h5("Water level measurements within this storm drain indicate that water is", strong("likely on or near the road surface."), align = "left"),animation = "scale"),", last observation was ",strong(time_since_last_measurement())," minutes ago", style = "color:white"))
        }
        if(time_since_last_measurement() >= 120){
          div(width = "100%", style="background-color:#838386;height:25px;padding:2.5px 2.5px;margin-bottom:5px", 
              p("Status: ",strong("UNKNOWN",style="color:white;"),tippy(icon("info-circle"), h5("The latest water level measurements within this storm drain indicate that water was", strong("likely on or near the road surface"), ", but the sensor has not reported water level for about ", strong(round(time_since_last_measurement()/60, digits = 0)), " hours", align = "left"),animation = "scale"),", last observation was about ",strong(round(time_since_last_measurement()/60,digits = 0))," hours ago", style = "color:white"))
        }
      }
      
      if(!flood_status_reactive()){
        if(time_since_last_measurement() <= 32){
          div(width = "100%", style="background-color:#48bf84;height:25px;padding:2.5px 2.5px;margin-bottom:5px", 
              p("Status: ",strong("NOT FLOODING",style="color:white;"),tippy(icon("info-circle"), h5("Water level measurements within this storm drain indicate that water is", strong("likely not near the road surface."), align = "left"),animation = "scale"),", last observation was ",strong(time_since_last_measurement())," minutes ago", style = "color:white"))
        }
        if(time_since_last_measurement() > 32){
          div(width = "100%", style="background-color:#838386;height:25px;padding:2.5px 2.5px;margin-bottom:5px",
              p("Status: ",strong("UNKNOWN",style="color:white;"),tippy(icon("info-circle"), h5("The latest water level measurements within this storm drain indicate that water was", strong("likely not near the road surface"), ", but the sensor has not reported water level for about ", strong(round(time_since_last_measurement()/60, digits = 0)), " hours", align = "left"),animation = "scale"),", last observation was about ",strong(round(time_since_last_measurement()/60, digits = 0))," hour(s) ago", style = "color:white"))
        }
      }
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
                                 leaflet::addLegend('bottomright', pal = pal, values = c(-2,0),
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
      
      if(input$city_name == ""){
        leafletProxy(mapId = "m") %>% 
          setView(lng = -77.360784, lat = 34.576053, zoom = 8)
      }
      
      if(input$city_name != ""){
        leafletProxy(mapId = "m") %>% 
            setView(lng = map1_selected_location()[1], lat = map1_selected_location()[2], zoom=16)
      }
    })
    
    
    observeEvent(input$view_on_map_wl,{
      updateNavbarPage(session, 
                       inputId = "nav",
                       selected = "Map")
      
      sensor_location_selected <- sensor_locations %>% 
        filter(sensor_ID == input$data_sensor) 
      
      sensor_location_selected_coords <- sensor_location_selected %>% 
        sf::st_coordinates()
      
      sensor_location_selected_label <-  as.list(str_remove(sensor_location_selected$html_popups,"<p align=\"center\">Click to view data at this site</p>"))
      
      leafletProxy(mapId = "m") %>%
        setView(lng = sensor_location_selected_coords[1], lat = sensor_location_selected_coords[2], zoom = 18) %>% 
        addPopups(lng = sensor_location_selected_coords[1], lat = sensor_location_selected_coords[2], lapply(sensor_location_selected_label,HTML),
                  options = popupOptions(closeButton = T, closeOnClick = T)
        )
      
      updateRadioButtons(session, 
                         inputId = "map_layers", 
                         choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                         selected = 1)
        
    })
    
    # Use this for cameras
    observeEvent(input$view_on_map_camera,{
      updateNavbarPage(session,
                       inputId = "nav",
                       selected = "Map")
      
      camera_location_selected <- camera_locations %>%
        filter(camera_ID == input$camera_ID) 
      
      camera_location_selected_coords <- camera_location_selected %>% 
        sf::st_coordinates()
      
      camera_location_selected_label <-  as.list(str_remove(camera_location_selected$html_popups,"<p align=\"center\">Click to view this camera</p>"))
      
      leafletProxy(mapId = "m") %>%
        setView(lng = camera_location_selected_coords[1], lat = camera_location_selected_coords[2], zoom = 18) %>%
        addPopups(lng = camera_location_selected_coords[1], lat = camera_location_selected_coords[2], camera_location_selected_label,
                  options = popupOptions(closeButton = T, closeOnClick = T)
        )
      
      updateRadioButtons(session, 
                         inputId = "map_layers", 
                         choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                         selected = 2)
      
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
          addAwesomeMarkers(data = camera_locations, icon=map_icon, group = "camera_site",
                            label = lapply(camera_locations_labels,HTML),
                            labelOptions = labelOptions(direction = "top", style=list("border-radius" = "10px")),
                            clusterOptions = markerClusterOptions(),
                            clusterId = "place",
                            layerId = sensor_locations$sensor_ID,
          )
          
      }
    })
    
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
          reactive_selection$overall_camera_location <- unique(sensor_locations$place[sensor_locations$sensor_ID == input$m_marker_click$id])
          
      }
      
      if(input$map_layers == 2){
        reactive_selection$overall_camera <- input$m_marker_click
        reactive_selection$overall_camera_location <- unique(camera_locations$place[camera_locations$camera_ID == input$m_marker_click$id])
        reactive_selection$overall_data_location <- unique(camera_locations$place[camera_locations$camera_ID == input$m_marker_click$id])
        
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
        
        thirdparty_metadata <- reactive({
          req(input$data_location)
          get_thirdparty_metadata(location = input$data_location)
        })
        
        output$thirdparty_info <- renderUI({
          helpText("Local water level data at this station are from",a(href=thirdparty_metadata()$url,thirdparty_metadata()$entity, target = "_blank"))
        })
        
        plot_missing_data_shading <- reactive({
          req(input$data_sensor, time_since_last_measurement())
          if(time_since_last_measurement() >= 32){
            return(datetime_to_timestamp(sensor_locations %>% filter(sensor_ID == input$data_sensor) %>% pull(date)))
          }
          else(return(NULL))
        })
        
        observeEvent(c(input$get_plot_data, input$view_3rdparty_data),{
          req(input$view_3rdparty_data == T,
              "obs" %in% unlist(thirdparty_metadata()$types),
              abs(input$dateRange[2] - input$dateRange[1]) <=30,
              nrow(sensor_data()!=0))

            min_date = min(input$dateRange)
            max_date = max(input$dateRange)

            plot_sensor_stats <- sensor_locations %>%
              filter(sensor_ID %in% input$data_sensor)

            plot_3rd_party_data_obs <<- get_thirdparty_wl(location = input$data_location,
                              type = "obs",
                              min_date = min_date %>% str_remove_all("-"),
                              max_date = max_date %>% str_remove_all("-")) %>%
              mutate(date = datetime_to_timestamp(date),
                     road_water_level= level-plot_sensor_stats$road_elevation,
                     sensor_water_level=level)

        })
        
        observeEvent(c(input$get_plot_data, input$view_3rdparty_data),{
          req(input$view_3rdparty_data == T,
              "pred" %in% unlist(thirdparty_metadata()$types),
              abs(input$dateRange[2] - input$dateRange[1]) <=30,
              nrow(sensor_data()!=0))
          
            min_date = min(input$dateRange)
            max_date = max(input$dateRange)
            
          plot_sensor_stats <- sensor_locations %>% 
            filter(sensor_ID %in% input$data_sensor)
          
          plot_3rd_party_data_predict <<- get_thirdparty_wl(location = input$data_location,
                            type = "pred",
                            min_date = min_date %>% str_remove_all("-"),
                            max_date = max_date %>% str_remove_all("-")) %>% 
            mutate(date = datetime_to_timestamp(date),
                   road_water_level= level-plot_sensor_stats$road_elevation,
                   sensor_water_level=level)
          
        })
            

        # Render plot with selected data
        observe({
          if(nrow(sensor_data()) == 0){
            # Popup on load to display info
            shinyalert::shinyalert(type="error",
                                   title="No data during the selected time range!",
                                   text="Please select a different time span.",
                                   animation = T)
          }
          
          # Set min and max date using reactive values from date_filter
          min_date_plot <- reactive_min_date() 
          max_date_plot <- reactive_max_date() 

          # use spinner for load
          w$show()

          # Render the plot
          output$site_level_ts <-  renderHighchart({
            req(nrow(sensor_data()>0))
            
            # Get all plot data
            plot_sensor_data <- sensor_data() 
            
            plot_sensor_stats <- sensor_locations %>% 
              filter(sensor_ID %in% input$data_sensor)
            
            x <- plot_sensor_data %>% 
              arrange(date) %>% 
              mutate(date = datetime_to_timestamp(date))
            
            # Plot Road datum
            if(input$elev_datum == "Road") {
              
              road_elevation_limit <- 0
              sensor_elevation_limit <- plot_sensor_stats$sensor_elevation - plot_sensor_stats$road_elevation
              y_axis_max <- ifelse(nrow(x) != 0,
                                     c(ifelse(max(x$road_water_level, na.rm=T) > road_elevation_limit, max(x$road_water_level, na.rm=T) , road_elevation_limit+0.25)),
                                     c(NA))
            }
            
            # Plot NAVD88 Datum
            if(input$elev_datum == "NAVD88") {

              road_elevation_limit <- plot_sensor_stats$road_elevation
              sensor_elevation_limit <- plot_sensor_stats$sensor_elevation
              y_axis_max <- ifelse(nrow(x)!=0,
                                     c(ifelse(max(x$sensor_water_level, na.rm=T) > road_elevation_limit, max(x$sensor_water_level, na.rm=T) , road_elevation_limit +0.25 )),
              c(NA))
              
            }
            
            hc <- highchart() %>%
              hc_add_series(data = x %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                            hcaes(x=date,
                                  y = wl),
                            type="line",
                            name="Water Level",
                            color="#1d1d75") %>%
              hc_chart(zoomType= "x",
                       backgroundColor = "#FFFFFF"
                       )%>%
              hc_plotOptions(series = list(lineWidth = 2,
                                           allowPointSelect = TRUE,
                                           states = list(hover = list(lineWidth = 2.5)),
                                           gapSize = 2160000,
                                           gapUnit = "value")) %>%
              hc_tooltip(crosshairs = TRUE,
                         valueDecimals = 2,
                         xDateFormat = "%I:%M %p, %b %e, %Y",
                         shared= TRUE
                         # borderWidth = 2
                         # sort = TRUE,
                         # table = TRUE
                         ) %>%
              hc_xAxis(type = "datetime",
                       max = max_date_plot %>% datetime_to_timestamp(),
                       min = min_date_plot %>% datetime_to_timestamp(),
                       dateTimeLabelFormats = list(
                         day = "%b %e",
                         minute = "%I:%M %p",
                         hour = "%I:%M %p"
                       ),
                       plotBands = list(
                         list(
                           from = plot_missing_data_shading(),
                           to = datetime_to_timestamp(Sys.time() %>% with_tz("UTC")),
                           color = hex_to_rgba("black", 0.1),
                           label = list(text = "Missing data",
                                        style = list(color = 'grey', fontWeight = 'bold', fontSize = 14),
                                        y = -5),
                           # the zIndex is used to put the label text over the grid lines
                           zIndex = 1

                         )
                       ),
                       plotLines = list(list(value = datetime_to_timestamp(Sys.time() %>% with_tz("UTC")),
                            # dashStyle = "longdash",
                            color="black",
                            width = 1,
                            zIndex = 4,
                            label = list(text = "Current time",
                                         style = list( color = 'black', fontWeight = 'bold'))))) %>%
              hc_yAxis(max = y_axis_max,
                       title = list(text = "Water Level (ft)"),
                       # min = y_axis_range[1],
                       plotLines = list(
                list(value =road_elevation_limit,
                     dashStyle = "longdash",
                     color="red",
                     width = 1,
                     zIndex = 4,
                     label = list(text = "Road Elevation",
                                  style = list( color = 'red', fontWeight = 'bold'))),
                list(value = sensor_elevation_limit,
                     dashStyle = "longdash",
                     color="black",
                     width = 1,
                     zIndex = 4,
                     label = list(text = "Sensor Elevation",
                                  style = list( color = 'black', fontWeight = 'bold'))))) %>%
              hc_exporting(enabled = TRUE,
                           filename = paste0(plot_sensor_stats$sensor_ID,"_",min_date_plot %>% with_tz("America/New_York"),"_to_",max_date_plot %>% with_tz("America/New_York")),
                           showTable = F,
                           buttons = list(contextButton = list(symbolSize = 20,
                                                               x= -20,
                                                               menuItems = list("viewFullscreen", "printChart", "separator", "downloadPNG")))) %>%
              hc_title(text =plot_sensor_stats$sensor_ID,
                       floating = F)
            
            if(input$view_3rdparty_data == T){
                
                plot_3rd_party_data_stats <- thirdparty_metadata()

              if("obs" %in% unlist(plot_3rd_party_data_stats$types)){
                hc <- hc %>% hc_add_series(plot_3rd_party_data_obs %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                                           hcaes(x=date,
                                                 y=wl),
                                           name = unique(plot_3rd_party_data_obs$entity),
                                           type="line",
                                           color="#DC018A",
                                           visible = T)
              }
  
              if("pred" %in% unlist(plot_3rd_party_data_stats$types)){
                hc <- hc %>% hc_add_series(plot_3rd_party_data_predict %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                                           hcaes(x=date,
                                                 y=wl),
                                           name = unique(plot_3rd_party_data_predict$entity),
                                           type="line",
                                           color="#01CB4D",
                                           visible = T)
              }
            }
            
            hc

          })
          
        })
        
        observe({
          req(input$camera_ID)
          
          
          w2$show()
          
            output$camera <- renderImage({
              
              outfile <- tempfile(fileext='.jpg')

              time <- con %>% 
                tbl("photo_info") %>% 
                filter(DateTimeOriginalUTC == max(DateTimeOriginalUTC, na.rm=T)) %>% 
                pull(DateTimeOriginalUTC) %>% 
                lubridate::with_tz(tzone="America/New_York")
              
              realtime_img <- magick::image_read(paste0("https://photos-sunnydayflood.cloudapps.unc.edu/public/",input$camera_ID,".jpg")) 
              
              realtime_img %>% 
                image_annotate(paste0(format(time, "%I:%M %p", usetz = T)," - ",format(time, "%b %d, %Y")), size = 40, color = "white", boxcolor = "black",
                               degrees = 0, gravity = "north") %>% 
                magick::image_write(path = outfile)
              
              # Return a list
              list(src = outfile,
                   alt = paste0("Latest picture from",input$camera_ID),
                   height = "100%")
              
            }, deleteFile = T)
            
            output$tf_predict <- renderHighchart({

              req <- httr::POST("https://ml-sunnydayflood.apps.cloudapps.unc.edu/detect_flooding_latest",
                                body = list(camera_ID=input$camera_ID),
                                encode = "json")

              prediction <- round(unlist(httr::content(req)),digits = 2)


              col_stops <- data.frame(
                q = c(0.4, 0.6, .8),
                c = c('#48bf84','#f5bf0f','#e1142c'),
                stringsAsFactors = FALSE
              )

              highchart() %>%
                hc_chart(type = "solidgauge") %>%
                hc_pane(
                  startAngle = -90,
                  endAngle = 90,
                  background = list(
                    outerRadius = '100%',
                    innerRadius = '60%',
                    shape = "arc"
                  )
                ) %>%
                hc_tooltip(enabled = FALSE) %>%
                hc_yAxis(
                  stops = list_parse2(col_stops),
                  lineWidth = 0,
                  minorTickWidth = 0,
                  tickAmount = 2,
                  min = 0,
                  max = 100,
                  labels = list(y = 26, style = list(fontSize = "22px"))
                ) %>%
                hc_add_series(
                  data = prediction*100,
                  dataLabels = list(
                    y = -50,
                    borderWidth = 0,
                    format= "{y} %",
                    useHTML = TRUE,
                    style = list(fontSize = "40px")
                  )
                ) %>%
                hc_size(height = 300)

            })
            w2$hide()
        })
        waiter::waiter_hide()
}

# Run the application
shinyApp(ui = ui, server = server)
