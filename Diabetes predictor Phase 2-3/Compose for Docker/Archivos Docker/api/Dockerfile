FROM rocker/r-ver:4.5.2

RUN apt-get update && apt-get install -y --no-install-recommends \
  build-essential \
  libcurl4-openssl-dev \
  libssl-dev \
  libxml2-dev \
  libsodium-dev \
  libicu-dev \
  ca-certificates \
  && rm -rf /var/lib/apt/lists/*

RUN R -q -e "install.packages(c('plumber','jsonlite'))"

WORKDIR /app
COPY plumber.R /app/plumber.R
COPY artifacts /app/artifacts

EXPOSE 8000

CMD ["R","-q","-e","plumber::plumb('plumber.R')$run(host='0.0.0.0', port=8000)"]
