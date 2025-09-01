# Performance-optimized Dockerfile with better caching + Database support

FROM rocker/r-base:4.3.2

# Install system dependencies (cached layer - rarely changes)
# Added postgresql-dev for RPostgreSQL package
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install core packages first (cached layer)
RUN R -e "install.packages(c('shiny', 'dplyr', 'readr', 'purrr', 'stringr'), repos='https://cran.rstudio.com/')"

# Install UI packages (cached layer)
RUN R -e "install.packages(c('httr', 'jsonlite', 'bslib', 'ggplot2', 'htmltools'), repos='https://cran.rstudio.com/')"

# Install utility packages (cached layer)  
RUN R -e "install.packages(c('commonmark', 'shinybusy', 'digest', 'shinyWidgets'), repos='https://cran.rstudio.com/')"

# Install database packages (new layer)
RUN R -e "install.packages(c('DBI', 'RSQLite', 'RPostgreSQL', 'uuid'), repos='https://cran.rstudio.com/')"

# Install heavy packages last (most likely to change/fail)
RUN R -e "install.packages(c('tidyverse', 'baseballr', 'stringi', 'janitor'), repos='https://cran.rstudio.com/')"

WORKDIR /app
COPY app/ /app/

EXPOSE 3838
CMD ["R", "-e", "shiny::runApp('/app/app.R', host='0.0.0.0', port=3838)"]
