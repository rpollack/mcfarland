source(file.path("..", "..", "refresh", "data_sources.R"))
source(file.path("..", "..", "refresh", "pipeline.R"))
source("fixture_http.R")

make_temp_dir <- function() {
  dir <- tempfile("data-sources-test-")
  dir.create(dir)
  dir
}
