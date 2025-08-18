# Simplified Dockerfile - Direct R Shiny approach

FROM rocker/r-base:4.3.2

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
RUN R -e "install.packages(c( \
    'shiny', 'dplyr', 'readr', 'purrr', 'stringr', \
    'httr', 'jsonlite', 'bslib', 'commonmark', \
    'shinybusy', 'ggplot2', 'htmltools', 'digest', \
    'tidyverse', 'baseballr', 'stringi', 'janitor' \
    ), repos='https://cran.rstudio.com/')"

# Create app directory
WORKDIR /app

# Copy your app
COPY app/app.R /app/app.R

EXPOSE 3838

# Run R directly to start the Shiny app
CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=3838)"]