# McFARLAND API Dockerfile
# Builds an R environment for the Plumber service that powers the Next.js front end.

FROM rocker/r-base:4.3.2

# Install system dependencies required by various R packages
RUN apt-get update && apt-get install -y \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libpq-dev \
    postgresql-client \
    && rm -rf /var/lib/apt/lists/*

# Install R package dependencies. Split across layers to improve build caching.
RUN R -e "install.packages(c('plumber', 'shiny', 'dplyr', 'readr', 'purrr', 'stringr'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('httr', 'jsonlite', 'bslib', 'ggplot2', 'htmltools'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('commonmark', 'shinybusy', 'digest', 'shinyWidgets', 'later'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('DBI', 'RSQLite', 'RPostgreSQL', 'uuid'), repos='https://cran.rstudio.com/')"
RUN R -e "install.packages(c('tidyverse', 'baseballr', 'stringi', 'janitor'), repos='https://cran.rstudio.com/')"

WORKDIR /srv/mcfarland

# Copy the entire repository so cached CSVs and R modules are available to the API.
COPY . .

# Default port for local development. Render will override PORT automatically.
ENV PORT=8000

EXPOSE 8000

CMD ["R", "-e", "pr <- plumber::pr('services/api/plumber.R'); pr$run(host = '0.0.0.0', port = as.numeric(Sys.getenv('PORT', '8000')))"]
