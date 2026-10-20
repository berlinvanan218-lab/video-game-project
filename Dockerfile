FROM rocker/r-ver:4.4.1

RUN apt-get update && apt-get install -y \
    libssl-dev libcurl4-openssl-dev libsodium-dev libxml2-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c('plumber','dplyr','readr','tidyr','jsonlite'))"

WORKDIR /app
COPY . /app

EXPOSE 8000
CMD ["Rscript", "run.R"]
