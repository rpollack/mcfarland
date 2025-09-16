# Admin utilities

# Check if session has admin privileges
is_admin <- function(session) {
  # Access the URL query string without triggering reactivity
  query <- parseQueryString(isolate(session$clientData$url_search))
  if (!is.null(query$admin) && query$admin == Sys.getenv("ADMIN_PASSWORD", "")) {
    session$userData$admin_mode <- TRUE
    return(TRUE)
  }
  return(isTRUE(session$userData$admin_mode))
}

# Log analysis unless user is admin
log_analysis_if_not_admin <- function(session, player_name, analysis_mode) {
  if (!is_admin(session)) {
    log_analysis(session, player_name, analysis_mode)
  } else {
    cat("🔧 Admin usage - not logged\n")
  }
}

log_share_if_not_admin <- function(session, player_name, analysis_mode, event_type) {
  if (!is_admin(session)) {
    log_share_event(session, player_name, analysis_mode, event_type)
  } else {
    cat("🔧 Admin share - not logged\n")
  }
}

log_session_if_not_admin <- function(session) {
  if (!is_admin(session)) {
    log_session_start(session)
  } else {
    cat("🔧 Admin session - not logged\n")
  }
}

