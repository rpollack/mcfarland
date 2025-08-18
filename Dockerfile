FROM rocker/shiny:4.3.2

RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libgit2-dev \
    libfontconfig1-dev \
    && rm -rf /var/lib/apt/lists/*

RUN R -e "install.packages(c( \
    'shiny', \
    'dplyr', \
    'readr', \
    'purrr', \
    'stringr', \
    'httr', \
    'jsonlite', \
    'bslib', \
    'commonmark', \
    'shinybusy', \
    'ggplot2', \
    'htmltools', \
    'digest', \
    'tidyverse', \
    'baseballr', \
    'stringi', \
    'janitor' \
    ), repos='https://cran.rstudio.com/')"

RUN mkdir -p /srv/shiny-server/
COPY app/app.R /srv/shiny-server/app.R

RUN echo 'run_as shiny; \
server { \
  listen 3838 0.0.0.0; \
  location / { \
    site_dir /srv/shiny-server; \
    log_dir /var/log/shiny-server; \
    directory_index on; \
  } \
}' > /etc/shiny-server/shiny-server.conf

EXPOSE 3838
CMD ["/usr/bin/shiny-server"]
