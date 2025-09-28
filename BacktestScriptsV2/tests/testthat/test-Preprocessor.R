library(testthat)
library(lubridate)

# Source the function to be tested (it will not exist yet)
# We use a tryCatch to prevent an error during the initial "Red" phase of TDD
tryCatch({
  source("../../Preprocessor.R")
}, error = function(e) {
  # Function doesn't exist yet, which is expected.
})

# --- Mock Data --- 
# Daily data spanning two weeks. Friday of week 2 is a holiday.
dt_daily <- as.Date(c("2025-01-03", "2025-01-06", "2025-01-07", "2025-01-08", "2025-01-09"))
open_daily <- c(100, 102, 104, 106, 108)
high_daily <- c(101, 103, 105, 107, 109)
low_daily  <- c(99, 101, 103, 105, 107)
close_daily <- c(100.5, 102.5, 104.5, 106.5, 108.5)
vol_daily <- c(1000, 1100, 1200, 1300, 1400)
div_daily <- c(0, 0, 0.5, 0, 0.25)
other_vec <- c(1, 2, 3, 4, 5)

# Intraday data
dt_intraday <- as.POSIXct(c("2025-01-01 09:30:00", "2025-01-01 09:31:00", "2025-01-01 10:00:00"))
open_intraday <- c(100, 101, 102)
high_intraday <- c(100.5, 101.5, 102.5)
low_intraday <- c(99.5, 100.5, 101.5)
close_intraday <- c(100.2, 101.2, 102.2)


# --- Test Suite --- 
test_that("Correctly aggregates daily data to weekly", {
  agg_data <- to_agg_timeframe(
    from_timeframe = "1d", to_timeframe = "1w", dt = dt_daily,
    open = open_daily, high = high_daily, low = low_daily, close = close_daily,
    volume = vol_daily, dividend = div_daily, 
    other_cols = list(my_other_col = other_vec)
  )
  
  # Should be 2 weeks of data
  expect_equal(nrow(agg_data), 2)
  
  # Test second week's data (which has a holiday Friday)
  week2 <- agg_data[2, ]
  expect_equal(week2$dt, as.Date("2025-01-09")) # Date is the last trading day
  expect_equal(week2$open, 102) # First open of the week
  expect_equal(week2$high, 109) # Max high of the week
  expect_equal(week2$low, 101)   # Min low of the week
  expect_equal(week2$close, 108.5) # Last close of the week
  expect_equal(week2$volume, 1100 + 1200 + 1300 + 1400) # Sum of volume
  expect_equal(week2$dividend, 0.5 + 0.25) # Sum of dividends
  expect_equal(week2$my_other_col, 5) # Last value of other_col
})

test_that("Timeframe validation works correctly", {
  # Expect error for down-sampling
  expect_error(to_agg_timeframe("1w", "1d", dt = dt_daily, open_daily, high_daily, low_daily, close_daily),
               "Target timeframe must be greater than source timeframe.")

  # Expect error for same timeframe
  expect_error(to_agg_timeframe("1d", "1d", dt = dt_daily, open_daily, high_daily, low_daily, close_daily),
               "Target timeframe must be greater than source timeframe.")

  # Expect error for invalid string
  expect_error(to_agg_timeframe("1d", "2y", dt = dt_daily, open_daily, high_daily, low_daily, close_daily),
               "Invalid timeframe provided.")
})

test_that("Date-type strategy pattern works correctly", {
  # 1. Test that a valid aggregation for Date objects works without error
  expect_no_error(to_agg_timeframe("1d", "1w", dt = dt_daily, open = open_daily, high = high_daily, low = low_daily, close = close_daily))

  # 2. Test that using an invalid unit for Date objects throws the correct error
  expect_error(to_agg_timeframe("1d", "1h", dt = dt_daily, open = open_daily, high = high_daily, low = low_daily, close = close_daily),
               "Invalid 'to_timeframe' for Date objects: 1h")

  # 3. POSIXct objects should work with intraday timeframes
  agg_data <- to_agg_timeframe(
    from_timeframe = "1m", to_timeframe = "1h", dt = dt_intraday,
    open = open_intraday, high = high_intraday, low = low_intraday, close = close_intraday
  )
  expect_equal(nrow(agg_data), 2) # 9:30 and 10:00 are in different hours
})

test_that("Handles optional vectors correctly", {
  # Should run without error when optional vectors are NULL
  agg_data <- to_agg_timeframe(
    from_timeframe = "1d", to_timeframe = "1w", dt = dt_daily,
    open = open_daily, high = high_daily, low = low_daily, close = close_daily
  )
  
  # Check that optional columns are not present
  expect_false("volume" %in% names(agg_data))
  expect_false("dividend" %in% names(agg_data))
})
