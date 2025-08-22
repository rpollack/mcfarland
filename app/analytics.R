# app/analytics.R
# Absolute minimal: just log "user X analyzed player Y with vibe Z at time T"

# Load required libraries
library(DBI)
library(RSQLite)
library(uuid)

# Only load RPostgreSQL if available
postgres_available <- requireNamespace("RPostgreSQL", quietly = TRUE)

# ==============================================================================
# DATABASE CONNECTION
# ==============================================================================
# Replace your get_db_connection() function with this version
# that handles Render's internal URL format (missing port)

get_db_connection <- function() {
  database_url <- Sys.getenv("DATABASE_URL")
  
  if (database_url == "" || Sys.getenv("RENDER") != "true") {
    # Local: SQLite
    cat("🔗 Using SQLite database\n")
    return(dbConnect(RSQLite::SQLite(), "analytics.db"))
  } else {
    # Production: PostgreSQL
    if (!postgres_available) {
      cat("⚠ PostgreSQL not available, falling back to SQLite\n")
      return(dbConnect(RSQLite::SQLite(), "analytics_fallback.db"))
    }
    
    tryCatch({
      library(RPostgreSQL)
      cat("🔗 Connecting to PostgreSQL\n")
      
      # Robust URL parsing that handles missing port
      parse_db_url <- function(url) {
        # Remove protocol
        if (grepl("^postgres://", url)) {
          url <- sub("^postgres://", "", url)
        } else if (grepl("^postgresql://", url)) {
          url <- sub("^postgresql://", "", url)
        } else {
          stop("URL must start with postgres:// or postgresql://")
        }
        
        # Split into auth@host/db parts
        parts <- strsplit(url, "@")[[1]]
        if (length(parts) != 2) stop("Invalid URL format: missing @")
        
        auth_part <- parts[1]
        host_db_part <- parts[2]
        
        # Parse auth (user:password)
        auth_split <- strsplit(auth_part, ":")[[1]]
        if (length(auth_split) != 2) stop("Invalid auth format")
        user <- auth_split[1]
        password <- auth_split[2]
        
        # Parse host/database (may or may not have port)
        host_db_split <- strsplit(host_db_part, "/")[[1]]
        if (length(host_db_split) != 2) stop("Invalid host/database format")
        
        host_port <- host_db_split[1]
        database <- host_db_split[2]
        
        # Parse host:port (port might be missing)
        if (grepl(":", host_port)) {
          # Port is specified
          host_port_split <- strsplit(host_port, ":")[[1]]
          host <- host_port_split[1]
          port <- as.integer(host_port_split[2])
        } else {
          # No port specified, use default PostgreSQL port
          host <- host_port
          port <- 5432L
        }
        
        return(list(
          user = user,
          password = password,
          host = host,
          port = port,
          database = database
        ))
      }
      
      # Parse the URL
      db_params <- parse_db_url(database_url)
      
      cat("✓ Parsed connection details:\n")
      cat("  Host:", db_params$host, "\n")
      cat("  Port:", db_params$port, "\n")
      cat("  Database:", db_params$database, "\n")
      cat("  User:", db_params$user, "\n")
      
      dbConnect(RPostgreSQL::PostgreSQL(),
                host = db_params$host,
                port = db_params$port,
                dbname = db_params$database,
                user = db_params$user,
                password = db_params$password)
      
    }, error = function(e) {
      cat("⚠ PostgreSQL connection failed:", e$message, "\n")
      cat("📁 Falling back to SQLite\n")
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
    
    # Simple index for date queries - use different syntax for SQLite vs PostgreSQL
    db_type <- class(con)[1]
    if (db_type == "SQLiteConnection") {
      dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_analyses_date ON analyses(date(timestamp))")
    } else {
      # PostgreSQL
      tryCatch({
        dbExecute(con, "CREATE INDEX IF NOT EXISTS idx_analyses_date ON analyses(DATE(timestamp))")
      }, error = function(e) {
        # Index might already exist
        cat("📊 Index creation note:", e$message, "\n")
      })
    }
    
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
    cat("👤 New user ID generated:", substr(user_id, 1, 8), "...\n")
  }
  return(user_id)
}

# ==============================================================================
# SIMPLE LOGGING
# ==============================================================================

log_analysis <- function(session, player_name, analysis_mode) {
  user_id <- session$userData$user_id
  if (is.null(user_id)) {
    cat("⚠ No user ID available for logging\n")
    return()
  }
  
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  tryCatch({
    dbExecute(con, "
      INSERT INTO analyses (user_id, player_name, analysis_mode)
      VALUES (?, ?, ?)
    ", params = list(user_id, player_name, analysis_mode))
    
    cat("📊", player_name, "-", analysis_mode, "- User:", substr(user_id, 1, 8), "...\n")
  }, error = function(e) {
    cat("⚠ Logging error:", e$message, "\n")
  })
}
