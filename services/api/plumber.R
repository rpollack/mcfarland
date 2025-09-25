# McFARLAND Plumber API -----------------------------------------------------

# nolint start: line_length
#* @apiTitle McFARLAND Baseball Insights API
#* @apiDescription Exposes cached player data, stat lines, and persona-driven
#* AI analysis for the React front end.
# nolint end

get_script_dir <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[[1]]))))
  }

  if (!is.null(sys.frames()[[1]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1]]$ofile)))
  }

  normalizePath(".", winslash = "/", mustWork = FALSE)
}

script_dir <- get_script_dir()
root_dir <- normalizePath(file.path(script_dir, "..", ".."), winslash = "/", mustWork = TRUE)
app_dir <- file.path(root_dir, "app")

source_module <- function(path, env) {
  sys.source(path, envir = env, chdir = TRUE)
}

app_env <- new.env(parent = globalenv())
modules <- c(
  "R/setup.R",
  "R/cache.R",
  "R/data.R",
  "R/photos.R",
  "R/player_info.R",
  "R/analysis.R",
  "R/compare.R",
  "R/api_helpers.R"
)

invisible(lapply(file.path(app_dir, modules), source_module, env = app_env))

data_root <- Sys.getenv("MCFARLAND_DATA_URL")
if (nzchar(data_root)) {
  app_env$GITHUB_DATA_URL <- data_root
} else {
  app_env$GITHUB_DATA_URL <- file.path(root_dir, "")
}

api_origin <- Sys.getenv("MCFARLAND_CORS_ORIGIN", "*")

respond_with_error <- function(res, status, message) {
  res$status <- status
  list(error = list(status = status, message = message))
}

#* Handle CORS preflight and response headers
#* @filter cors
cors <- function(req, res) {
  res$setHeader("Access-Control-Allow-Origin", api_origin)
  res$setHeader("Access-Control-Allow-Methods", "GET,POST,OPTIONS")
  res$setHeader("Access-Control-Allow-Headers", "Content-Type, Authorization")

  if (identical(req$REQUEST_METHOD, "OPTIONS")) {
    res$status <- 204
    return(list())
  }

  plumber::forward()
}

#* Health check
#* @serializer json list(auto_unbox = TRUE, na = "null")
#* @get /health
function() {
  list(
    status = "ok",
    time = app_env$iso8601_utc(Sys.time()),
    cache = list(
      refreshed = app_env$iso8601_utc(app_env$.cache_timestamp),
      source = app_env$GITHUB_DATA_URL
    )
  )
}

#* List available players
#* @serializer json list(auto_unbox = TRUE, na = "null")
#* @param type Optional player type filter (hitter or pitcher)
#* @param search Optional search string
#* @get /players
function(type = NULL, search = NULL) {
  baseball_data <- app_env$load_baseball_data_cached()

  normalized_type <- tolower(trimws(type %||% ""))
  if (!normalized_type %in% c("", "hitter", "pitcher")) {
    normalized_type <- ""
  }

  search_term <- if (!is.null(search)) trimws(search) else NULL

  players <- app_env$build_players_index(
    baseball_data,
    type = if (normalized_type == "") NULL else normalized_type,
    search = search_term,
    include_photo = TRUE
  )

  list(
    players = players,
    type = if (normalized_type == "") "all" else normalized_type,
    count = length(players),
    updatedAt = app_env$iso8601_utc(Sys.time()),
    cache = list(refreshed = app_env$iso8601_utc(app_env$.cache_timestamp))
  )
}

#* Fetch a stat line for a player
#* @serializer json list(auto_unbox = TRUE, na = "null")
#* @param player_id Player identifier (compound id supported)
#* @get /players/<player_id>/statline
function(player_id, res) {
  baseball_data <- app_env$load_baseball_data_cached()
  payload <- app_env$build_statline_payload(player_id, baseball_data, include_photo = TRUE)

  if (is.null(payload)) {
    return(respond_with_error(res, 404, "Player not found"))
  }

  payload$updatedAt <- app_env$iso8601_utc(Sys.time())
  payload$cache <- list(refreshed = app_env$iso8601_utc(app_env$.cache_timestamp))
  payload
}

#* Generate persona analysis for a player
#* @serializer json list(auto_unbox = TRUE, na = "null")
#* @param player_id Player identifier
#* @param mode Persona/analysis mode (default when empty)
#* @get /players/<player_id>/analysis
function(player_id, mode = "default", res) {
  baseball_data <- app_env$load_baseball_data_cached()
  record <- app_env$resolve_player_record(player_id, baseball_data)

  if (is.null(record)) {
    return(respond_with_error(res, 404, "Player not found"))
  }

  analysis_mode <- if (is.null(mode) || trimws(mode) == "") "default" else mode
  analysis <- app_env$analyze_player_performance(player_id, analysis_mode, baseball_data)

  list(
    player = app_env$build_player_payload(record, baseball_data, include_photo = TRUE),
    mode = analysis_mode,
    html = as.character(analysis),
    updatedAt = app_env$iso8601_utc(Sys.time()),
    cache = list(refreshed = app_env$iso8601_utc(app_env$.cache_timestamp))
  )
}

#* Compare multiple players of the same type
#* @serializer json list(auto_unbox = TRUE, na = "null")
#* @post /compare
function(req, res) {
  body <- tryCatch(jsonlite::fromJSON(req$postBody), error = function(e) NULL)

  if (is.null(body) || is.null(body$playerIds)) {
    return(respond_with_error(res, 400, "playerIds are required"))
  }

  player_ids <- unique(as.character(unlist(body$playerIds)))
  player_ids <- player_ids[player_ids != ""]

  if (length(player_ids) == 0) {
    return(respond_with_error(res, 400, "At least one player id must be provided"))
  }

  analysis_mode <- if (is.null(body$mode) || trimws(body$mode) == "") "default" else body$mode

  baseball_data <- app_env$load_baseball_data_cached()
  statlines <- lapply(player_ids, function(id) {
    app_env$build_statline_payload(id, baseball_data, include_photo = TRUE)
  })

  if (any(vapply(statlines, is.null, logical(1)))) {
    return(respond_with_error(res, 404, "One or more players could not be found"))
  }

  player_types <- vapply(statlines, function(x) x$player$type, character(1))
  player_type <- player_types[[1]]

  if (length(unique(player_types)) > 1) {
    return(respond_with_error(res, 400, "Players must all be hitters or all pitchers"))
  }

  analysis <- app_env$analyze_player_comparison(player_ids, baseball_data, analysis_mode)
  recommendation_id <- app_env$recommend_best_player(player_ids, baseball_data)
  recommendation <- app_env$build_recommended_player(recommendation_id, baseball_data)

  list(
    playerType = player_type,
    mode = analysis_mode,
    players = statlines,
    analysisHtml = as.character(analysis),
    recommended = recommendation,
    updatedAt = app_env$iso8601_utc(Sys.time()),
    cache = list(refreshed = app_env$iso8601_utc(app_env$.cache_timestamp))
  )
}
