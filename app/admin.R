# Keep your existing admin check (this is good!)
is_admin <- function(session) {
  query <- parseQueryString(session$clientData$url_search)
  if (!is.null(query$admin) && query$admin == Sys.getenv("ADMIN_PASSWORD", "")) {
    session$userData$admin_mode <- TRUE
    return(TRUE)
  }
  return(isTRUE(session$userData$admin_mode))
}

# OPTION 1: Fixed log_if_not_admin - with default log_function parameter
log_if_not_admin <- function(session, log_function = cat, ...) {
  if (!is_admin(session)) {
    # Check if log_function is actually a function
    if (is.function(log_function)) {
      log_function(...)
    } else {
      # Fallback to cat if log_function is not a function
      cat("📝 LOG:", log_function, ..., "\n")
    }
  } else {
    cat("🔧 Admin usage - not logged\n")
  }
}