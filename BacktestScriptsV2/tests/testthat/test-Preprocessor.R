library(testthat)
library(lubridate)
library(dplyr)

# Source the function to be tested
source("../../Preprocessor.R")

# --- New, Realistic Mock Data ---
# 1-minute data for a few days, respecting market hours (9:30 start) and days (Mon-Wed)
dttm_1m_realistic <- as.POSIXct(c(
  # Monday, Jan 6th
  "2025-01-06 09:30:00", "2025-01-06 09:31:00", "2025-01-06 15:59:00",
  # Tuesday, Jan 7th
  "2025-01-07 09:30:00", "2025-01-07 09:31:00",
  # Wednesday, Jan 8th
  "2025-01-08 09:30:00", "2025-01-08 15:59:00"
))

ohlc_1m_realistic <- list(
  open = c(100, 101, 102, 103, 104, 105, 106),
  high = c(100.5, 101.5, 102.5, 103.5, 104.5, 105.5, 106.5),
  low = c(99.5, 100.5, 101.5, 102.5, 103.5, 104.5, 105.5),
  close = c(100.1, 101.1, 102.1, 103.1, 104.1, 105.1, 106.1)
)

# Daily data for weekly/monthly aggregation
dttm_daily_realistic <- as.POSIXct(c(
  # Week 1 (Jan 6-10)
  "2025-01-06 16:00:00", "2025-01-07 16:00:00", "2025-01-08 16:00:00", "2025-01-09 16:00:00", "2025-01-10 16:00:00",
  # Week 2 (Jan 13-17)
  "2025-01-13 16:00:00", "2025-01-14 16:00:00", "2025-01-17 16:00:00", # Note: Jan 15, 16 are holidays
  # Feb data
  "2025-02-03 16:00:00"
))
ohlc_daily_realistic <- list(
  open = c(100, 102, 104, 106, 108, 110, 112, 114, 116),
  high = c(101, 103, 105, 107, 109, 111, 113, 115, 117),
  low = c(99, 101, 103, 105, 107, 109, 111, 113, 115),
  close = c(100.5, 102.5, 104.5, 106.5, 108.5, 110.5, 112.5, 114.5, 116.5)
)


# --- Test Suite ---
test_that("Aggregates 1m to 1d using min/max of actual data", {
  agg_data <- to_agg_timeframe(
    from_timeframe = "1m", to_timeframe = "1d", time_vec = dttm_1m_realistic,
    open = ohlc_1m_realistic$open, high = ohlc_1m_realistic$high,
    low = ohlc_1m_realistic$low, close = ohlc_1m_realistic$close,
    timestamp_priority = "first"
  )
  
  expect_equal(nrow(agg_data), 3) # Jan 6, 7, 8
  
  # Test Jan 6th bar
  bar1 <- agg_data[1, ]
  expect_equal(bar1$dt, as.Date("2025-01-06")) # Should be the date of the first data point
  expect_equal(bar1$open, 100) # First open of the day
  expect_equal(bar1$high, 102.5) # Max high of the day
  expect_equal(bar1$low, 99.5) # Min low of the day
  expect_equal(bar1$close, 102.1) # Last close of the day
  
  # Test period start/end dates
  expect_equal(bar1$period_start_dt, as.Date("2025-01-06")) # Min date in group
  expect_equal(bar1$period_end_dt, as.Date("2025-01-06")) # Max date in group
})

test_that("Aggregates daily to weekly using min/max of actual data", {
  agg_data <- to_agg_timeframe(
    from_timeframe = "1d", to_timeframe = "1w", time_vec = dttm_daily_realistic,
    open = ohlc_daily_realistic$open, high = ohlc_daily_realistic$high,
    low = ohlc_daily_realistic$low, close = ohlc_daily_realistic$close,
    timestamp_priority = "last" # Your special case for weekly
  )
  
  expect_equal(nrow(agg_data), 3) # Three distinct weeks
  
  # Test Week 2 data (with holidays)
  week2 <- agg_data[2, ]
  expect_equal(week2$dt, as.Date("2025-01-17")) # 'last' priority means it's the last day
  expect_equal(week2$open, 110) # Open of Jan 13
  expect_equal(week2$high, 115) # High of Jan 17
  expect_equal(week2$low, 109) # Low of Jan 13
  expect_equal(week2$close, 114.5) # Close of Jan 17
  
  # Test period start/end dates
  expect_equal(week2$period_start_dt, as.Date("2025-01-13")) # Min date in group
  expect_equal(week2$period_end_dt, as.Date("2025-01-17")) # Max date in group
})

test_that("Aggregates daily to monthly using min/max of actual data", {
  agg_data <- to_agg_timeframe(
    from_timeframe = "1d", to_timeframe = "1mo", time_vec = dttm_daily_realistic,
    open = ohlc_daily_realistic$open, high = ohlc_daily_realistic$high,
    low = ohlc_daily_realistic$low, close = ohlc_daily_realistic$close
    # Using default timestamp_priority = 'first'
  )

  expect_equal(nrow(agg_data), 2) # Jan and Feb
  
  # Test Jan bar
  jan_bar <- agg_data[1, ]
  expect_equal(jan_bar$dt, as.Date("2025-01-06")) # 'first' priority means it's the first day
  expect_equal(jan_bar$open, 100) # Open of Jan 6
  expect_equal(jan_bar$high, 115) # Max high in Jan
  expect_equal(jan_bar$low, 99) # Min low in Jan
  expect_equal(jan_bar$close, 114.5) # Close of Jan 17
  
  # Test period start/end dates
  expect_equal(jan_bar$period_start_dt, as.Date("2025-01-06")) # Min date in group
  expect_equal(jan_bar$period_end_dt, as.Date("2025-01-17")) # Max date in group
})

test_that("Aggregates 1m to 15m with both timestamp priorities", {
  dttm_1m <- as.POSIXct(c(
    "2025-01-06 09:30:00", "2025-01-06 09:31:00", "2025-01-06 09:44:00", # First bucket
    "2025-01-06 09:45:00", "2025-01-06 09:59:00"  # Second bucket
  ))
  ohlc_1m <- list(
    open = c(100, 101, 102, 103, 104),
    high = c(100.5, 101.5, 102.5, 103.5, 104.5),
    low = c(99.5, 100.5, 101.5, 102.5, 103.5),
    close = c(100.1, 101.1, 102.1, 103.1, 104.1)
  )
  
  # Test with timestamp_priority = 'first'
  agg_first <- to_agg_timeframe(
    from_timeframe = "1m", to_timeframe = "15m", time_vec = dttm_1m,
    open = ohlc_1m$open, high = ohlc_1m$high, low = ohlc_1m$low, close = ohlc_1m$close,
    timestamp_priority = "first"
  )
  
  expect_equal(nrow(agg_first), 2)
  expect_equal(agg_first[1, ]$dttm, as.POSIXct("2025-01-06 09:30:00")) # Min time
  expect_equal(agg_first[1, ]$period_start_dttm, as.POSIXct("2025-01-06 09:30:00"))
  expect_equal(agg_first[1, ]$period_end_dttm, as.POSIXct("2025-01-06 09:44:00"))

  # Test with timestamp_priority = 'last'
  agg_last <- to_agg_timeframe(
    from_timeframe = "1m", to_timeframe = "15m", time_vec = dttm_1m,
    open = ohlc_1m$open, high = ohlc_1m$high, low = ohlc_1m$low, close = ohlc_1m$close,
    timestamp_priority = "last"
  )
  
  expect_equal(nrow(agg_last), 2)
  expect_equal(agg_last[1, ]$dttm, as.POSIXct("2025-01-06 09:44:00")) # Max time
  expect_equal(agg_last[1, ]$period_start_dttm, as.POSIXct("2025-01-06 09:30:00"))
  expect_equal(agg_last[1, ]$period_end_dttm, as.POSIXct("2025-01-06 09:44:00"))
})
