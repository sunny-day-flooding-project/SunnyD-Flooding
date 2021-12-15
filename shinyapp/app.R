# Packages to load
library(dplyr)
library(dbplyr)
library(lubridate)
library(shiny)
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
library(shinyjs)
library(bs4Dash)


# Source env variables if working on desktop
# source("/Users/adam/Documents/SunnyD/sunnyday_postgres_keys.R")

# HTML waiting screen for initial load
waiting_screen <- tagList(
  spin_wave(),
  h4("Loading...")
)

# Color palette for water level on site map
pal <- colorNumeric(
  palette = rev(brewer.pal(10,"RdYlBu")),
  domain = c(-3.5,0.5))

pal_rev <- colorNumeric(
  palette = rev(brewer.pal(10,"RdYlBu")),
  domain = c(-3.5,0.5),
  reverse = T)

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

drift_corrected_database <- con %>% 
  tbl("sensor_data_drift_corrected")

global <- getOption("highcharter.global")
global$useUTC <- FALSE
global$timezoneOffset <- -300
options(highcharter.global = global)

get_thirdparty_metadata <- function(location){
  switch(location,
         "Beaufort, North Carolina" = tibble("entity"= "NOAA",
                                             "url" = "https://tidesandcurrents.noaa.gov/waterlevels.html?id=8656483",
                                             "types"=list(c("obs","pred")))
  )
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

time_converter <- function(x){
  if(x < 60){
    return(paste0(strong(x)," minutes ago"))
  }
  else if(x >= 60 & x < 1440){
    return(paste0("about ", strong(round(x/60))," hour(s) ago"))
  }
  else(
    return(paste0("about ", strong(round(x/1440)), " day(s) ago"))
  )
}

jsCode <- "shinyjs.init = function() {
  $(document).on('shiny:sessioninitialized', function (e) {
  var mobile = window.matchMedia('only screen and (max-width: 768px)').matches;
  Shiny.onInputChange('is_mobile_device', mobile);
});
}"


#------------------------ Define UI ---------------------------------------
ui <- bs4Dash::dashboardPage(
  title = "Data Viewer", 
  header = bs4Dash::dashboardHeader(
    border = F,
    .list = list(span(HTML('<i class="fas fa-sun" style="color:#fbb040;display:inline;"></i>'),p("Sunny Day Flooding Project", style = "color:white;display:inline;"))
    )
  ),
  sidebar = bs4Dash::dashboardSidebar(
    width = 400,
    skin = "light",
    status = "primary",
    elevation = 2,
    sidebarMenu(
      id = "nav",
      
      menuItem("Map", tabName = "Map", icon = icon("map")),
      menuItem("Data", tabName = "Data", icon = icon("database")),
      menuItem("Flood Cam", tabName = "Pictures", icon = icon("camera")),
      menuItem("About", tabName = "About", icon = icon("info-circle"))
    )
  ),
  controlbar = bs4Dash::dashboardControlbar(
    controlbarItem( 
      br(),
      h2("Admin Login"),
      passwordInput(inputId = "admin_pswd", label = "Password"),
      actionButton(inputId = "admin_pswd_submit", label = "Log In", width = "100px"), 
      actionButton(inputId = "admin_logout", label = "Log Out", status = "secondary"), 
      br(),
      uiOutput(outputId = "admin_status")
    )
  ),
  body = bs4Dash::dashboardBody(
    fluidPage(
      disconnectMessage(
        text = "Your session has timed out! Try refreshing the page.",
        refresh = "Refresh",
        background = "#FFFFFF",
        colour = "#000000",
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.25,
        width = 450,
        top = "center",
        size = 24,
        css = ""
      ),
      useShinyalert(),
      useShinyjs(),
      extendShinyjs(text = jsCode, functions = c()),
      use_waiter(),
      waiter::waiter_show_on_load(html = spin_3k(),
                                  color = transparent(0)),
      tags$head(tags$link(rel = "shortcut icon", href = "https://tarheels.live/sunnydayflood/wp-content/uploads/sites/1319/2021/02/sunny_d_icon-01-2.png"),
                includeCSS("sunnyd-theme.css"),
                tags$style(HTML('
        
        .content-wrapper>.content {
          padding-top:15px !important;
          padding-bottom:15px !important;
          padding-left:0px !important;
          padding-right:0px !important;
        }
        
        .pretty-link {
          color: #7bafff;
        }
        
        .pretty-link:hover {
          color: #638ecf;
        }
        
        .navbar-white {
          background-color: #23272b !important; 
        }
        
        .navbar-dark {
          background-color: #23272b !important; 
        }

        .brand-link {
          background-color: #13294b !important;
          color: #fff !important;
          border-bottom: 1px solid black !important;
        }

        p {
          margin-bottom: 0px !important;
        }
        
        .dark-theme-icon fa fa-sun {
          color:white !important;
        }
        
        .fa {
          color:white !important;
        }
        
        #downloadData {
          background-color:#13294b;
          color:white !important;
          //font-size: 12pt;
          border:3px;
        }
        
        #downloadData:hover {
          background-color:#000000;
          color:white !important;
          //font-size: 12pt;
          border: 3px;
        }
        
        #get_plot_data {
          background-color: #fbb040;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
        }
        
        #get_plot_data:hover {
          background-color: #f1a93e;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
        }
        
        #refresh_button {
          background-color: #616C78;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
        }
        
        #refresh_button:hover {
          background-color: #40474f;
          color: white;
          //font-size: 12pt;
          font-weight: bold;
          //border: 3px;
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
        
        #view_on_map_wl, #view_on_map_camera {
          background-color: #fbb040;
          color: white;
          font-weight: bold;
          border: 3px;
        }
        
        #view_on_map_wl:hover, #view_on_map_camera:hover {
          background-color: #f1a93e;
          color: white;
          font-weight: bold;
          border: 3px;
        }
        
        #camera img {max-width: 100%; width: 500px; height: auto}
        
        .input-group-text {
          background-color: #ffffff00!important;
          border: none;
        }
        
      '))),
      tabItems(
        
        # Mapping tab for overview and site map
        tabItem(tabName = "Map",
                fluidRow(
                  column(width = 3,
                         div(class="card",
                             div(class = "card-body",
                                 id = "instructions_box",
                                 p("Navigate to a study location and select a map layer. Hover and click on points explore live data and pictures"),
                                 br(),
                                 selectInput(
                                   'city_name',
                                   label = h4("Location"),
                                   choices = c(""),
                                   selected = NULL,
                                   multiple = F,
                                   selectize = F
                                 ),
                                 radioButtons("map_layers", label = h4("Map Layers"),
                                              choices = list("Water Level Sensors" = 1, "Flood Cams" = 2), 
                                              selected = 1)
                             )
                         )
                  ),
                  column(width = 9,
                         div(class="card",
                             div(class = "card-body",
                                 style="padding:0px;",
                                 uiOutput(outputId = "leaf")
                             )
                         )
                  )
                )
                
        ), 
        
        # Tab showing selected data and time series graphs
        tabItem(tabName = "Data",
                # Time series interactive plot or data table in separate tabs
                box(width=12, 
                    id = "flood_status", 
                    collapsed = T,
                    #   p(
                    #   icon("info-circle"),
                    #   "  Flood Status: ",
                    #   strong("Loading, ", style = "color:white;"),
                    #   style = "margin-bottom: 0px;display:inline;"
                    # ),
                    # status = "gray",
                    solidHeader = T,
                    p(strong("NOT FLOODING:"), " Water level measurements within this storm drain indicate that water is", em("likely not near the road surface."), align = "left",style="margin-bottom:0px;"),
                    p(strong("WARNING:"), " Water level measurements within this storm drain indicate that water is", em("likely near the road surface."), align = "left",style="margin-bottom:0px;"),
                    p(strong("FLOODING:"), " Water level measurements within this storm drain indicate that water is", em("likely on the road surface."),"This status is triggered by a loss of communications with our underground sensor (meaning that our sensor is completely under water and can't transmit data)", align = "left",style="margin-bottom:0px;"),
                    p(strong("UNKNOWN:"), " Water level within this storm drain is unknown because the sensor has not reported water level recently.", align = "left",style="margin-bottom:0px;")
                ),
                
                tabBox(width=12,
                       id = "data_tabs",
                       type = "tabs",
                       collapsible = F,
                       tabPanel(
                         "Plot",
                         highchartOutput("site_level_ts", width = "100%"),
                         hr(),
                         h4("Plot options"),
                         fluidRow(
                           column(width = 3,
                                  uiOutput("firstSelection"),
                                  uiOutput("secondSelection")
                           ),
                           column(width=1),
                           column(width = 3,
                                  selectInput(inputId = "elev_datum", label = "Elevation Datum", selectize = F,choices = c("Road","NAVD88"), selected = "Road"),
                                  p(strong("Local water levels"),tippy(icon("info-circle",style="font-size:14px"), h5("Click the button below to add nearby downstream water levels to the plot.",br(),br(),"Adding these data can help visualize when flooding may occur.",br(),br(),"Turning this option on may slow down the drawing of the plot.",style = "text-align:left;"))),
                                  switchInput(inputId = "view_3rdparty_data", label = ,value = F, size = "mini",inline=T),
                                  uiOutput(outputId="thirdparty_info", style="display:inline;")
                           ),
                           column(width=1),
                           column(width = 3,
                                  uiOutput("dateRange"),
                                  br(),
                                  div(
                                    actionButton("get_plot_data", "Plot new dates"),
                                    actionButton("refresh_button", "Refresh", status = "primary"),
                                    style="text-align:center", size = "lg")
                           ),
                           column(width=1)
                           
                         ),
                         hr(),
                         fluidRow(
                           column(width=12,
                            uiOutput("site_notes")
                           )
                         )
                       ),
                       tabPanel(
                         "Site Info",
                         uiOutput(outputId = "site_description")
                       ),
                       tabPanel(
                         "Download",
                         h3("Click the button below to download the selected data.", align="center"),
                         br(),
                         p("Data will be downloaded as a .csv file named",strong("site name_minimum date_maximum date.csv"),align="center"),
                         br(),
                         div(downloadButton(outputId = "downloadData", label = "Download Selected Data", class = "download_button", icon = icon("download")), align="center")
                       )
                )
                
        ),
        tabItem(tabName = "Pictures",
                tabBox(
                  collapsible = F,
                  type = "tabs",
                  tabPanel(
                    "Pictures",
                    imageOutput(outputId = "camera"),
                    uiOutput("firstCameraSelection", align = "center"),
                    uiOutput("secondCameraSelection", align = "center")
                  ),
                  # column(6,
                  #        h1("Water detected?",tippy(icon("info-circle",style="font-size:16px"), tooltip = div(h5("We are using machine learning to detect water on the road surface using our flood cams.",br(),br(),"The model used here is trained to distinguish pictures that have water in them from those that do not. Cars, salt water stains, sun glare, and other features in the photos can decrease the accuracy of the model.",br(),br(),"This model is still in development and is for informational purposes only."), style = "text-align:left"))),
                  #        div(p("In Development",style="color:white;text-align: center"),style="background-color:#fbb040;width:125px;border-radius: 20px"),
                  #        # p("The chart below shows the probability that water has been detected in this picture with our machine learning model"),
                  #         uiOutput(outputId = "camml")))
                  #        # highchartOutput(outputId ="tf_predict")))
                  # ),
                  tabPanel(
                    "Site Info",
                    h3("Site description coming soon"),
                    actionButton("view_on_map_camera", label = "View site on map", icon = icon("map"))
                    
                  )
                  
                )
                
        ),
        tabItem(tabName = "About",
                div(class = "card",
                    div(class = "card-body",
                        includeMarkdown("about.md")
                    )
                )
        )
      )
    )
  ),
  footer = dashboardFooter(
    left = p(em(strong("Disclaimer: "), "Data are preliminary and for informational use only.")),
    right = "Copyright 2021 Sunny Day Flooding"
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
  
  w_admin <- Waiter$new(id="admin_pswd_submit",
                   html = spin_3k(),
                   color = transparent(.75)
                   ) 
  
  output$dateRange <- renderUI({
    dateRangeInput(
      inputId = 'dateRange',
      label = 'Date range',
      start = Sys.Date() - 2,
      end = Sys.Date() + 1,
      max = Sys.Date() + 14
      )
  })

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
  shinyalert(text = includeMarkdown("landing_text.md"),
             html = T,
             closeOnClickOutside = FALSE,
             showConfirmButton = T,
             confirmButtonCol = "#fbb040",
             confirmButtonText = "OK",
             type = "info",
             animation=F,
             size = "s")
  
  
  output$leaf = renderUI({
    req(!is.null(input$is_mobile_device))
    
    
    if (input$is_mobile_device == F) {
      leafletOutput(outputId = "m",
                    width = "100%",
                    height = "calc(100vh - 160px)")
    }
    
    else if (input$is_mobile_device == T) {
      leafletOutput(outputId = "m",
                    width = "100%",
                    height = "calc(100vh - 130px)")
    }
    
  })
  
  observeEvent(input$nav,
               {
                 req(input$is_mobile_device == T)
                 # for desktop browsers
                 addClass(selector = "body", class = "sidebar-collapse")
                 # for mobile browsers
                 removeClass(selector = "body", class = "sidebar-open")
               })
  
  admin_login_status <- reactiveVal(value = F)
  
  output$admin_status <- renderUI(
    span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged out", style="display:inline-block;")) 
  )
  
  observeEvent(input$admin_pswd_submit, {
    req(input$admin_pswd, admin_login_status() == F)
    
    w_admin$show()
    
    if(isolate(input$admin_pswd) == Sys.getenv("ADMIN_PSWD")){
      
      admin_login_status(T)
      
      updateActionButton(inputId = "admin_pswd_submit",
                         icon = tags$i(
                           class = "fas fa-check-circle", 
                           style = "color: rgb(0,166,90)"
                         ))
      
      toast(
        title = "🎉 Logged in! 🎉",
        options = list(
          autohide = TRUE,
          # icon = "fas fa-check",
          close = FALSE,
          delay = 3000,
          position = "topRight"
        )
      )
      
      output$admin_status <- renderUI(
        span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged in", style="display:inline-block;")) 
      )
      
      w_admin$hide()
    }
    
    if(isolate(input$admin_pswd) != Sys.getenv("ADMIN_PSWD")){
      
      w_admin$show()
      
      updateActionButton(inputId = "admin_pswd_submit",
                         icon = tags$i(
                           class = "fas fa-times-circle", 
                           style = "color: red"
                         ))
      
      toast(
        title = "Incorrect password!",
        options = list(
          autohide = TRUE,
          icon = "fas fa-times-circle",
          close = FALSE,
          delay = 3000,
          position = "topRight"
        )
      )
      
      output$admin_status <- renderUI(
        span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged out", style="display:inline-block;")) 
      )
      
      w_admin$hide()
    }
  })
  
  observeEvent(input$admin_logout, {
    req(isolate(admin_login_status() == T))
    
    w_admin$show()
    
    admin_login_status(F)
    
    updateActionButton(inputId = "admin_pswd_submit",
                       icon = tags$i(
                         class = "fas fa-times-circle", 
                         style = "color: red"
                       ))
    
    toast(
      title = "Logged out!",
      options = list(
        autohide = TRUE,
        icon = "fas fa-times-circle",
        close = FALSE,
        delay = 3000,
        position = "topRight"
      )
    )
    
    output$admin_status <- renderUI(
      span(p("Status: ",style="display:inline-block;font-weight: bold;"), p("Logged out", style="display:inline-block;")) 
    )
    
    w_admin$hide()
  }
  )
  
  # Load Data
  # Update sensor locations with most recent data from database
  sensor_locations <- con %>% 
    tbl("sensor_locations") %>%
    collect() %>%
    left_join(drift_corrected_database %>%
                group_by(sensor_ID) %>%
                filter(date == max(date, na.rm=T)) %>% 
                collect() %>% 
                mutate(sensor_water_level = sensor_water_level_adj,
                       road_water_level = road_water_level_adj),
              by=c("place", "sensor_ID", "sensor_elevation", "road_elevation")) %>% 
    mutate(date_lst = lubridate::with_tz(date, tzone = "America/New_York")) %>% 
    sf::st_as_sf(coords = c("lng", "lat"), crs = 4269) %>%
    mutate(
      html_popups = paste0( 
        '<div>',
        '<h4 align="center"><strong>Site ',sensor_ID,'</h4></strong>',
        '<h5 align="center">Last level:</h5>',
        '<h4 align="center">',round(road_water_level, digits = 2),'</h4>',
        '<p align="center">',paste0(format(date_lst, "%I:%M %p", usetz = T)," - ",format(date_lst, "%b %d, %Y")),'</p>',
        '<p align="center">Click to view data at this site</p>'
      )
    ) %>% 
    mutate(flood_status = sensor_water_level > alert_threshold)
  
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
        '<h4 align="center"><strong>Camera ',camera_ID,'</h4></strong>',
        '<h5 align="center">Last picture:</h5>',
        '<p align="center">',paste0(format(date_lst, "%I:%M %p", usetz = T)," - ",format(date_lst, "%b %d, %Y")),'</p>',
        '<p align="center">Click to view this camera</p>'
      )
    )
  
  # Labels for sensor map 1
  sensor_locations_labels <- as.list(sensor_locations$html_popups)
  
  # Labels for sensor map 1 camera
  camera_locations_labels <- as.list(camera_locations$html_popups)
  
  
  updateSelectInput(session = session,
                    inputId = "city_name",
                    # label = h4("Location"),
                    choices = c("",unique(sensor_locations$place)),
                    selected = NULL)
  
  
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
    admin_login_nonreactive <- admin_login_status()
    
    if(data_n > 7440){
      
      if(admin_login_nonreactive == F){
      shinyalert::shinyalert(type="error", 
                             title="Data request is too large",
                             text="Please select a shorter time span. Maximum request is 30 days .")
      }
      
      if(admin_login_nonreactive == T){
        reactive_min_date(input$dateRange[1] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
        reactive_max_date(input$dateRange[2] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
      }
    }
    if(data_n <= 7440){
      reactive_min_date(input$dateRange[1] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
      reactive_max_date(input$dateRange[2] %>% as_datetime() %>% force_tz("America/New_York") %>% with_tz("UTC"))
    }
  })
  

  # Reactive value that stores the data from the selected sensor site, filtered by date range
  sensor_data <- reactive({
    req(input$data_sensor)
    
    input$refresh_button
    
    drift_corrected_database %>%
      filter(sensor_ID %in% !!input$data_sensor) %>%
      filter(date >= !!reactive_min_date() & date < !!reactive_max_date(),
             qa_qc_flag == F) %>% 
      collect() 
    
  })
  
  
  observeEvent(input$refresh_button,{
    toast(
      title = "Refreshed!",
      options = list(
        autohide = TRUE,
        icon = "fas fa-check",
        close = FALSE,
        delay = 3000,
        position = "bottomRight"
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(input$data_sensor, "_",format(reactive_min_date(),"%Y%m%d"), "_",format(reactive_max_date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(sensor_data(), file)
    }
  )
  
  # Input drop-downs menus for data page
  output$firstSelection <- renderUI({
    selectInput("data_location", "Location", choices = unique(sensor_locations$place), selected = reactive_selection$overall_data_location, selectize = F)
  })
  
  output$secondSelection <- renderUI({
    selectInput("data_sensor", "Sensor", choices = reactive_selection$overall_data_sensor_choices, selected = reactive_selection$overall_data_sensor, selectize = F,multiple = F)
  })
  
  output$firstCameraSelection <- renderUI({
    selectInput("camera_location", "Location", choices = unique(camera_locations$place), selected = reactive_selection$overall_camera_location, selectize = F)
  })
  
  output$secondCameraSelection <- renderUI({
    selectInput("camera_ID", "Camera", choices = reactive_selection$overall_camera_choices, selected = reactive_selection$overall_camera_ID, selectize = F,multiple = F)
  })
  
  flood_status_reactive <- reactive({
    req(input$data_sensor)
    status <- sensor_locations %>% filter(sensor_ID == input$data_sensor) %>% pull(flood_status)
    return(status)
  })
  
  time_since_last_measurement <- reactive({
    req(input$data_sensor)
    last_datetime <- sensor_locations %>% filter(sensor_ID == input$data_sensor) %>% pull(date)
    time_difference <- time_length(((Sys.time() %>% with_tz("UTC"))-last_datetime), unit="minute")
    return(round(time_difference))
  })
  
  # showing the banner of flood alert status
  observe({
    time_since_last_measurement_value <- time_since_last_measurement()
    
    time_since_last_measurement_text <- time_converter(time_since_last_measurement_value)
    
    if(flood_status_reactive() == F){
      if(time_since_last_measurement_value <= 45){
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("NOT FLOODING, ", style = "color:white;"),
                      HTML(time_since_last_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "success",
                    solidHeader = T
                  )
        )
      }
      
      else(
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("UNKNOWN, ", style = "color:white;"),
                      HTML(time_since_last_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "gray",
                    solidHeader = T
                  )
        )
      )
    }
    
    else if(flood_status_reactive() == T){
      if(time_since_last_measurement_value <= 45){
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("WARNING", style = "color:white;"),
                      HTML(time_since_last_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "warning",
                    solidHeader = T
                  )
        )
      }
      
      else(
        updateBox(id = "flood_status",
                  action="update",
                  session = session,
                  options = list(
                    title = p(
                      icon("info-circle"),
                      "  Roadway Flood Status: ",
                      strong("FLOODING, ", style = "color:white;"),
                      HTML(time_since_last_measurement_text),
                      style = "margin-bottom: 0px;display:inline;"
                    ),
                    status = "danger",
                    solidHeader = T
                  )
        )
      )
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
                                              color = "black", 
                                              fillColor = ~pal(road_water_level), 
                                              fillOpacity = 1) %>% 
                             leaflet::addLegend('bottomright', pal = pal_rev, values = c(-3.5,0.5),
                                                title = 'Water level<br>relative to<br>surface (ft)',
                                                opacity = 1,
                                                labFormat = labelFormat(transform = function(x) sort(x, decreasing = TRUE))) %>% 
                             addLayersControl(
                               baseGroups = c("Positron (default)", "Dark Matter","Imagery", "OSM"),
                               options = layersControlOptions(collapsed = T)) %>% 
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
  
  # Updates the choices for the camera ID on the data tab
  observe({
    reactive_selection$overall_camera_choices <- camera_locations$camera_ID[camera_locations$place == input$camera_location]
  })
  

  observeEvent(input$m_marker_click,{
    reactive_selection$overall_data_sensor <- input$m_marker_click
    reactive_selection$overall_data_location <- unique(sensor_locations$place[sensor_locations$sensor_ID == input$m_marker_click$id])
    reactive_selection$overall_camera_location <- unique(sensor_locations$place[sensor_locations$sensor_ID == input$m_marker_click$id])
    
    reactive_selection$overall_camera <- input$m_marker_click

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
    helpText("  Data source: ",a(href=thirdparty_metadata()$url,thirdparty_metadata()$entity, target = "_blank", class = "pretty-link"))
  })
  
  plot_missing_data_shading <- reactive({
    req(input$data_sensor, time_since_last_measurement())
    if(time_since_last_measurement() >= 45){
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
  
  site_info <- reactive({
    req(input$data_sensor)
    return(sensor_locations %>% 
             filter(sensor_ID == input$data_sensor))
  })
  
  output$site_notes <- renderUI({
    p(strong("Site notes: "),site_info()$notes.x)
  })
  
  output$site_description <- renderUI({
    includeMarkdown(paste0("site_descriptions/",site_info()$sensor_ID,"/",site_info()$sensor_ID,".md"))
    
  })
  
  observeEvent(input$view_3rdparty_data, ignoreInit = T,{
    w$show()
  })
  
  # Render plot with selected data
  observe({
    if(nrow(sensor_data()) == 0 & input$view_3rdparty_data == F){
      # Popup on load to display info
      shinyalert::shinyalert(type="error",
                             title="Whoops, no data!",
                             text="We don't have any data during the selected time range. 
                             Please select a different time span or turn on 'Local Water Levels' to view data from other sources.",
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
                             c(ifelse(max(x$road_water_level_adj, na.rm=T) > road_elevation_limit, max(x$road_water_level_adj, na.rm=T) , road_elevation_limit+0.25)),
                             c(NA))
      }
      
      # Plot NAVD88 Datum
      if(input$elev_datum == "NAVD88") {
        
        road_elevation_limit <- plot_sensor_stats$road_elevation
        sensor_elevation_limit <- plot_sensor_stats$sensor_elevation
        y_axis_max <- ifelse(nrow(x)!=0,
                             c(ifelse(max(x$sensor_water_level_adj, na.rm=T) > road_elevation_limit, max(x$sensor_water_level_adj, na.rm=T) , road_elevation_limit +0.25 )),
                             c(NA))
        
      }
      
      hc <- highchart() %>%
        hc_add_series(data = x %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level_adj","sensor_water_level_adj")),
                      hcaes(x=date,
                            y = wl),
                      type="line",
                      name="Water Level",
                      color="#1d1d75",
                      # Controls when points are shown on plot (only on zoom)
                      marker = list(
                        enabledThreshold = 0.25
                      )) %>%
        hc_chart(zoomType= "x",
                 backgroundColor = "#FFF"
        )%>%
        hc_plotOptions(series = list(lineWidth = 2,
                                     # Shows points on line when hovered
                                     allowPointSelect = TRUE,
                                     # Makes line thicker on hover
                                     states = list(hover = list(lineWidth = 2.5)),
                                     # Defines gap size to not connect points with line
                                     gapSize = 2160000,
                                     gapUnit = "value"
                                     
        )) %>%
        hc_tooltip(crosshairs = TRUE,
                   valueDecimals = 2,
                   xDateFormat = "%I:%M %p, %b %e, %Y",
                   shared= TRUE
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
      
      if(admin_login_status() == T){
        hc <- hc %>% 
          hc_add_series(data = x %>% dplyr::select(date,"wl" = ifelse(input$elev_datum == "Road","road_water_level","sensor_water_level")),
                        hcaes(x=date,
                              y = wl),
                        type="line",
                        name="Water Level (unadjusted)",
                        color="#4da9f0",
                        visible = T,
                        # Controls when points are shown on plot (only on zoom)
                        marker = list(
                          enabledThreshold = 0.25
                        )) %>% 
          hc_add_series(data = x %>% dplyr::select(date,voltage),
                        hcaes(x=date,
                              y = voltage/10),
                        type="line",
                        name="Battery (Voltage/10)",
                        color="#ffde05",
                        visible = F,
                        # Controls when points are shown on plot (only on zoom)
                        marker = list(
                          enabledThreshold = 0.25
                        ))
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
      
      realtime_img <- magick::image_read(paste0("https://photos-sunnydayflood.apps.cloudapps.unc.edu/public/",input$camera_ID,".jpg")) 
      
      realtime_img %>% 
        image_annotate(paste0(format(time, "%I:%M %p", usetz = T)," - ",format(time, "%b %d, %Y")), size = 40, color = "white", boxcolor = "black",
                       degrees = 0, gravity = "north") %>% 
        magick::image_write(path = outfile)
      
      # Return a list
      list(src = outfile,
           alt = paste0("Latest picture from",input$camera_ID),
           height = "100%")
      
    }, deleteFile = T)
    
    # output$camml <- renderUI({
    #   req <- httr::POST("https://ml-acgold.apps.cloudapps.unc.edu/detect_flooding_latest",
    #                     body = list(camera_ID=input$camera_ID),
    #                     encode = "json")
    # })
    
    #             output$tf_predict <- renderHighchart({
    
    # req <- httr::POST("https://ml-sunnydayflood.apps.cloudapps.unc.edu/detect_flooding_latest",
    #                   body = list(camera_ID=input$camera_ID),
    #                   encode = "json")
    
    #               prediction <- round(unlist(httr::content(req)),digits = 2)
    
    
    #               col_stops <- data.frame(
    #                 q = c(0.4, 0.6, .8),
    #                 c = c('#48bf84','#f5bf0f','#e1142c'),
    #                 stringsAsFactors = FALSE
    #               )
    
    #               highchart() %>%
    #                 hc_chart(type = "solidgauge") %>%
    #                 hc_pane(
    #                   startAngle = -90,
    #                   endAngle = 90,
    #                   background = list(
    #                     outerRadius = '100%',
    #                     innerRadius = '60%',
    #                     shape = "arc"
    #                   )
    #                 ) %>%
    #                 hc_tooltip(enabled = FALSE) %>%
    #                 hc_yAxis(
    #                   stops = list_parse2(col_stops),
    #                   lineWidth = 0,
    #                   minorTickWidth = 0,
    #                   tickAmount = 2,
    #                   min = 0,
    #                   max = 100,
    #                   labels = list(y = 26, style = list(fontSize = "22px"))
    #                 ) %>%
    #                 hc_add_series(
    #                   data = prediction*100,
    #                   dataLabels = list(
    #                     y = -50,
    #                     borderWidth = 0,
    #                     format= "{y} %",
    #                     useHTML = TRUE,
    #                     style = list(fontSize = "40px")
    #                   )
    #                 ) %>%
    #                 hc_size(height = 300)
    
    #             })
    w2$hide()
  })
  waiter::waiter_hide()
}

# Run the application
shinyApp(ui = ui, server = server)
