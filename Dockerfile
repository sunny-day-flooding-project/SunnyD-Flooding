# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:latest

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
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
RUN install2.r dplyr lubridate shinydashboard colourvalues waiter sf leaflet DT htmltools RColorBrewer dygraphs xts shinyalert RPostgres DBI pool dbplyr magick

# expose ports
EXPOSE 3838
EXPOSE 5432

# create new user so it doesn't run as root
RUN groupadd -r shinyapp && useradd --no-log-init -r -g shinyapp shinyapp

# copy necessary files
ADD sunnyd-shinyapp /home/shinyapp/app

# change working directory
WORKDIR /home/shinyapp

# change to new 'shinyapp' user
USER shinyapp

# run app on container start
CMD ["R", "-e", "shiny::runApp('app', host = '0.0.0.0', port = 3838)"]
