FROM rocker/shiny:latest

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev libssl-dev libxml2-dev \
 && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('shiny','httr','jsonlite','plotly','bslib'))"

WORKDIR /srv/shiny-server
COPY App.R /srv/shiny-server/app.R

EXPOSE 3838
