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
      
      # Parse DATABASE_URL format: postgres://user:password@host:port/dbname
      url_parts <- regmatches(database_url, regexec("postgres://([^:]+):([^@]+)@([^:]+):(\\d+)/(.+)", database_url))[[1]]
      
      if (length(url_parts) < 6) {
        stop("Invalid DATABASE_URL format")
      }
      
      dbConnect(RPostgreSQL::PostgreSQL(),
                host = url_parts[4], 
                port = as.integer(url_parts[5]),
                dbname = url_parts[6], 
                user = url_parts[2], 
                password = url_parts[3])
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

# ==============================================================================
# ADMIN ANALYTICS FUNCTIONS
# ==============================================================================

get_analytics_summary <- function() {
  con <- get_db_connection()
  on.exit(dbDisconnect(con))
  
  tryCatch({
    # Total analyses today
    today_count <- dbGetQuery(con, "
      SELECT COUNT(*) as count 
      FROM analyses 
      WHERE DATE(timestamp) = DATE('now')
    ")
    
    # Total analyses this week  
    week_count <- dbGetQuery(con, "
      SELECT COUNT(*) as count 
      FROM analyses 
      WHERE timestamp >= datetime('now', '-7 days')
    ")
    
    # Top players analyzed today
    top_players <- dbGetQuery(con, "
      SELECT player_name, COUNT(*) as count
      FROM analyses 
      WHERE DATE(timestamp) = DATE('now')
      GROUP BY player_name 
      ORDER BY count DESC 
      LIMIT 5
    ")
    
    # Top analysis modes
    top_modes <- dbGetQuery(con, "
      SELECT analysis_mode, COUNT(*) as count
      FROM analyses 
      WHERE DATE(timestamp) = DATE('now')
      GROUP BY analysis_mode 
      ORDER BY count DESC
    ")
    
    list(
      today_total = today_count$count,
      week_total = week_count$count,
      top_players = top_players,
      top_modes = top_modes
    )
  }, error = function(e) {
    cat("⚠ Analytics query error:", e$message, "\n")
    list(today_total = 0, week_total = 0, top_players = data.frame(), top_modes = data.frame())
  })
}