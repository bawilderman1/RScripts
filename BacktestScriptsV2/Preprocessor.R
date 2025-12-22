library(dplyr)
library(lubridate)
library(rlang)

# --- Helper function to parse timeframe units for lubridate ---
.parse_timeframe_unit <- function(to_timeframe) {
  num_part <- as.numeric(gsub("[^0-9.]", "", to_timeframe))
  unit_part <- gsub("[0-9.]", "", to_timeframe)

  lubridate_unit <- switch(unit_part,
    "m" = paste(num_part, "minutes"),
    "h" = paste(num_part, "hours"),
    "d" = paste(num_part, "days"),
    "w" = paste(num_part, "weeks"),
    "mo" = paste(num_part, "months"),
    stop(paste("Unsupported timeframe unit for lubridate:", unit_part))
  )
  return(lubridate_unit)
}

# --- Strategy for Date Bucketing ---

#' @description Generic function to create a time-period bucket.
#' S3 dispatch will select the correct method based on the class of 'dt'.
get_period_bucket <- function(time_vec, from_timeframe, to_timeframe, time_ranks) {
  UseMethod("get_period_bucket")
}

#' @description Strategy for Date objects (interday).
get_period_bucket.Date <- function(time_vec, from_timeframe, to_timeframe, time_ranks) {
  allowed_units <- c("1d", "1w", "1mo")
  if (!(to_timeframe %in% allowed_units)) {
    stop(paste("Invalid 'to_timeframe' for Date objects:", to_timeframe))
  }
  if (time_ranks[to_timeframe] <= time_ranks[from_timeframe]) {
    stop("Target timeframe must be greater than source timeframe.")
  }
  floor_date(time_vec, unit = to_timeframe)
}

#' @description Strategy for POSIXct objects (intraday).
get_period_bucket.POSIXct <- function(time_vec, from_timeframe, to_timeframe, time_ranks) {
  allowed_units <- c("1m", "3m", "5m", "15m", "30m", "1h", "1d", "1w", "1mo")
   if (!(to_timeframe %in% allowed_units)) {
    stop(paste("Invalid 'to_timeframe' for POSIXct objects:", to_timeframe))
  }
  if (time_ranks[to_timeframe] <= time_ranks[from_timeframe]) {
    stop("Target timeframe must be greater than source timeframe.")
  }

  lubridate_unit <- .parse_timeframe_unit(to_timeframe) # Use helper
  
  floor_date(time_vec, unit = lubridate_unit)
}


#' Aggregate time-series data to a higher timeframe.
to_agg_timeframe <- function(from_timeframe, to_timeframe, time_vec, open, high, low, close, 
                             volume = NULL, adjClose = NULL, dividend = NULL, 
                             other_cols = list(), timestamp_priority = "first") {

  # --- 1. Timeframe Validation ---
  time_ranks <- c("1m"=1, "3m"=2, "5m"=3, "15m"=4, "30m"=5, "1h"=6, 
                  "1d"=7, "1w"=8, "1mo"=9)
  
  if (!(from_timeframe %in% names(time_ranks)) || !(to_timeframe %in% names(time_ranks))) {
    stop("Invalid timeframe provided.")
  }

  # --- 2. Assemble Input Vectors into a Tibble ---
  df <- tibble(dttm = time_vec, open, high, low, close)
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
    # Use min() and max() on the actual data for the period boundaries
    period_start = expr(min(dttm)),
    period_end = expr(max(dttm))
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
    mutate(period_bucket = get_period_bucket(time_vec, from_timeframe, to_timeframe, time_ranks)) %>%
    group_by(period_bucket) %>%
    summarise(!!!summary_expressions) %>%
    select(-period_bucket) %>%
    ungroup()

  # Set the main time column based on priority
  if (timestamp_priority == "last") {
    aggregated_data$dttm <- aggregated_data$period_end
  } else {
    aggregated_data$dttm <- aggregated_data$period_start
  }
  
  # --- 5. Conditionally Format Output Time Column & Add Period Start/End ---
  is_interday <- time_ranks[to_timeframe] >= time_ranks["1d"]
  
  if (is_interday) {
    aggregated_data <- aggregated_data %>%
      mutate(dt = as.Date(dttm)) %>%
      select(-dttm) %>%
      rename(period_start_dt = period_start, period_end_dt = period_end) %>%
      mutate(period_start_dt = as.Date(period_start_dt), period_end_dt = as.Date(period_end_dt)) %>%
      select(dt, period_start_dt, period_end_dt, everything())
  } else {
    aggregated_data <- aggregated_data %>% 
      rename(period_start_dttm = period_start, period_end_dttm = period_end) %>%
      select(dttm, period_start_dttm, period_end_dttm, everything())
  }
    
  return(aggregated_data %>% arrange(1))
}
