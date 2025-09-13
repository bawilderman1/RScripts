library(duckdb)
library(dplyr)
library(lubridate)

#' Load and Prepare Time-Series Data for Backtesting
#'
#' This function connects to a duckdb database, loads price data,
#' optionally joins dividend data, and aggregates it to the desired timeframe.
#'
#' @param timeframe The desired timeframe for the data (e.g., "1d", "1w", "1mo").
#' @param db_path The file path to the duckdb database.
#' @param base_table The name of the primary price data table in the database.
#' @param dividend_table An optional string specifying the name of the dividend table.
#'   If NULL, dividends will not be processed.
#' @return A tibble with the prepared data, aggregated to the specified timeframe.
load_and_prepare_data <- function(timeframe, db_path, base_table, dividend_table = NULL) {
  
  # --- Branch for future Intraday timeframes ---
  if (timeframe %in% c("1min", "5min", "1h")) {
    # This section is a placeholder for future intraday implementation.
    # It would handle POSIXct timestamps and likely not process dividends.
    stop(paste("Intraday timeframe '", timeframe, "' not yet implemented.", sep=""))
  }
  
  con <- dbConnect(duckdb::duckdb(), dbdir = db_path, read_only = TRUE)
  
  # --- Build the initial SQL query dynamically ---
  if (!is.null(dividend_table)) {
    sql <- paste("
      SELECT 
        p.dt, p.open, p.high, p.low, p.close, 
        COALESCE(d.dividend, 0.0) as dividend
      FROM ", base_table, " p
      LEFT JOIN ", dividend_table, " d ON p.dt = d.dt
      ORDER BY p.dt
    ")
  } else {
    sql <- paste("
      SELECT 
        dt, open, high, low, close, 
        0.0 as dividend
      FROM ", base_table, "
      ORDER BY dt
    ")
  }
  
  # --- Execute the query in duckdb ---
  daily_master <- dbGetQuery(con, sql)
  dbDisconnect(con, shutdown = TRUE)
  
  daily_master <- as_tibble(daily_master) %>% mutate(dt = as.Date(dt))
  
  # --- If the timeframe is daily, return the pre-joined data ---
  if (timeframe == "1d") {
    return(daily_master)
  }
  
  # --- For weekly or monthly, aggregate using dplyr ---
  aggregated_data <- daily_master %>%
    mutate(period_bucket = floor_date(dt, unit = timeframe)) %>%
    group_by(period_bucket) %>%
    summarise(
      open = first(open),
      high = max(high),
      low = min(low),
      close = last(close),
      dividend = sum(dividend),
      dt = max(dt) # Get the latest actual trading day in the period
    ) %>%
    select(-period_bucket) %>%
    ungroup() %>%
    arrange(dt)
    
  return(aggregated_data)
}
