FROM rocker/shiny:4.1.0

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libglpk-dev \
    libjq-dev \
    libv8-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libcairo2-dev \
    libsqlite3-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libudunits2-dev \
    libgdal-dev \
    odbc-postgresql \
    libmagick++-dev

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean

# install packages
RUN install2.r dplyr lubridate bs4Dash colourvalues waiter sf leaflet DT htmltools RColorBrewer highcharter xts shinyalert RPostgres DBI pool dbplyr magick stringr shinydisconnect tippy httr shinyWidgets shinyjs

# expose ports
EXPOSE 3838
EXPOSE 5432

# create new user so it doesn't run as root
RUN groupadd -r shinyapp && useradd --no-log-init -r -g shinyapp shinyapp

# copy necessary files
ADD shinyapp /home/shinyapp/app

# change working directory
WORKDIR /home/shinyapp

# change to new 'shinyapp' user
USER shinyapp

# run app on container start
CMD ["R", "-e", "shiny::runApp('app', host = '0.0.0.0', port = 3838)"]