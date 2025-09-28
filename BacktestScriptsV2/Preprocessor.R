library(dplyr)
library(lubridate)
library(rlang)

# --- Strategy for Date Bucketing ---

#' @description Generic function to create a time-period bucket.
#' S3 dispatch will select the correct method based on the class of 'dt'.
get_period_bucket <- function(dt, from_timeframe, to_timeframe, time_ranks) {
  UseMethod("get_period_bucket")
}

#' @description Strategy for Date objects (interday).
get_period_bucket.Date <- function(dt, from_timeframe, to_timeframe, time_ranks) {
  allowed_units <- c("1d", "1w", "1mo")
  if (!(to_timeframe %in% allowed_units)) {
    stop(paste("Invalid 'to_timeframe' for Date objects:", to_timeframe))
  }
  if (time_ranks[to_timeframe] <= time_ranks[from_timeframe]) {
    stop("Target timeframe must be greater than source timeframe.")
  }
  floor_date(dt, unit = to_timeframe)
}

#' @description Strategy for POSIXct objects (intraday).
get_period_bucket.POSIXct <- function(dt, from_timeframe, to_timeframe, time_ranks) {
  allowed_units <- c("1m", "3m", "5m", "15m", "30m", "1h", "1d", "1w", "1mo")
   if (!(to_timeframe %in% allowed_units)) {
    stop(paste("Invalid 'to_timeframe' for POSIXct objects:", to_timeframe))
  }
  if (time_ranks[to_timeframe] <= time_ranks[from_timeframe]) {
    stop("Target timeframe must be greater than source timeframe.")
  }
  floor_date(dt, unit = to_timeframe)
}


#' Aggregate time-series data to a higher timeframe.
#'
#' @param from_timeframe The source timeframe (e.g., "1d").
#' @param to_timeframe The target timeframe (e.g., "1w").
#' @param dt A vector of Date or POSIXct objects.
#' @param open,high,low,close Required numeric vectors for OHLC data.
#' @param volume,adjClose,dividend Optional numeric vectors.
#' @param other_cols A named list of optional vectors to be aggregated by taking the last value in the period.
#' @return A tibble with the aggregated data.
to_agg_timeframe <- function(from_timeframe, to_timeframe, dt, open, high, low, close, 
                             volume = NULL, adjClose = NULL, dividend = NULL, 
                             other_cols = list()) {

  # --- 1. Timeframe Validation ---
  time_ranks <- c("1m"=1, "3m"=2, "5m"=3, "15m"=4, "30m"=5, "1h"=6, 
                  "1d"=7, "1w"=8, "1mo"=9)
  
  if (!(from_timeframe %in% names(time_ranks)) || !(to_timeframe %in% names(time_ranks))) {
    stop("Invalid timeframe provided.")
  }

  # --- 2. Assemble Input Vectors into a Tibble ---
  df <- tibble(dt, open, high, low, close)
  if (!is.null(volume)) df$volume <- volume
  if (!is.null(adjClose)) df$adjClose <- adjClose
  if (!is.null(dividend)) df$dividend <- dividend
  
  if (length(other_cols) > 0) {
    for (col_name in names(other_cols)) {
      df[[col_name]] <- other_cols[[col_name]]
    }
  }

  # --- 3. Build Dynamic Aggregation Logic ---
  summary_expressions <- list(
    open = expr(first(open)),
    high = expr(max(high)),
    low = expr(min(low)),
    close = expr(last(close)),
    dt = expr(max(dt))
  )
  
  if ("volume" %in% names(df)) summary_expressions$volume <- expr(sum(volume))
  if ("adjClose" %in% names(df)) summary_expressions$adjClose <- expr(last(adjClose))
  if ("dividend" %in% names(df)) summary_expressions$dividend <- expr(sum(dividend))
  
  if (length(other_cols) > 0) {
    for (col_name in names(other_cols)) {
      summary_expressions[[col_name]] <- expr(last(!!sym(col_name)))
    }
  }

  # --- 4. Perform Aggregation ---
  aggregated_data <- df %>%
    mutate(period_bucket = get_period_bucket(dt, from_timeframe, to_timeframe, time_ranks)) %>%
    group_by(period_bucket) %>%
    summarise(!!!summary_expressions) %>%
    select(-period_bucket) %>%
    ungroup() %>%
    mutate(period_start_dt = floor_date(dt, unit = to_timeframe), period_end_dt = dt) %>%
    arrange(dt)
    
  return(aggregated_data)
}