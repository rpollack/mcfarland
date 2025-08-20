# app/analytics.R
# Absolute minimal: just log "user X analyzed player Y with vibe Z at time T"

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================

get_db_connection <- function() {
  database_url <- Sys.getenv("DATABASE_URL")
  
  if (database_url == "" || Sys.getenv("RENDER") != "true") {
    # Local: SQLite
    return(dbConnect(RSQLite::SQLite(), "analytics.db"))
  } else {
    # Production: PostgreSQL
    tryCatch({
      url_parts <- regmatches(database_url, regexec("postgres://([^:]+):([^@]+)@([^:]+):(\\d+)/(.+)", database_url))[[1]]
      dbConnect(RPostgreSQL::PostgreSQL(),
                host = url_parts[4], port = as.integer(url_parts[5]),
                dbname = url_parts[6], user = url_parts[2], password = url_parts[3])
    }, error = function(e) {
      return(dbConnect(RSQLite::SQLite(), "analytics_fallback.db"))
    })
  }
}

# ==============================================================================
# DATABASE SETUP
# ==============================================================================

init_analytics_db <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  tryCatch({
    # ONE TABLE: just log every analysis
    dbExecute(con, "
      CREATE TABLE IF NOT EXISTS analyses (
        user_id TEXT,
        timestamp TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
        player_name TEXT,
        analysis_mode TEXT
      )
    ")
    
    # Simple index for date queries
    dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_analyses_date ON analyses(date(timestamp))")
    
    cat("✓ Analytics table ready\n")
  }, error = function(e) {
    cat("⚠ Database setup error:", e$message, "\n")
  })
}

# ==============================================================================
# USER ID GENERATION
# ==============================================================================

generate_user_id <- function(session) {
  user_id <- session$userData$user_id
  if (is.null(user_id)) {
    user_id <- UUIDgenerate()
    session$userData$user_id <- user_id
  }
  return(user_id)
}

# ==============================================================================
# SIMPLE LOGGING
# ==============================================================================

log_analysis <- function(session, player_name, analysis_mode) {
  user_id <- session$userData$user_id
  if (is.null(user_id)) return()
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  tryCatch({
    dbExecute(con, "
      INSERT INTO analyses (user_id, player_name, analysis_mode)
      VALUES (?, ?, ?)
    ", params = list(user_id, player_name, analysis_mode))
    
    cat("📊", player_name, "-", analysis_mode, "\n")
  }, error = function(e) {
    cat("⚠ Logging error:", e$message, "\n")
  })
}