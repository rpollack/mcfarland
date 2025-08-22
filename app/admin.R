# app/admin.R - Fixed version

# Admin check function (keep your existing one - this is good!)
is_admin <- function(session) {
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query$admin) && query$admin == Sys.getenv("ADMIN_PASSWORD", "")) {
    session$userData$admin_mode <- TRUE
    return(TRUE)
  }
  return(isTRUE(session$userData$admin_mode))
}

# SIMPLIFIED: Much simpler logging function
log_if_not_admin <- function(session, player_name, analysis_mode) {
  if (!is_admin(session)) {
    log_analysis(session, player_name, analysis_mode)
  } else {
    cat("🔧 Admin usage - not logged\n")
  }
}