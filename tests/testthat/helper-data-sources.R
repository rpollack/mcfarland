source(file.path("..", "..", "data_sources.R"))
source(file.path("..", "..", "pipeline.R"))
source("fixture_http.R")

make_temp_dir <- function() {
  dir <- tempfile("data-sources-test-")
  dir.create(dir)
  dir
}
