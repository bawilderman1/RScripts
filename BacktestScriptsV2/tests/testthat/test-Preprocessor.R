library(testthat)

# Source the function to be tested and the test db helper
source("../../Preprocessor.R")
source("setup-db.R")

# Define the path for our temporary test database
TEST_DB_FILE <- "test_db.duckdb"

# Create the test database before running tests
create_test_db(TEST_DB_FILE)

# Test suite for the Preprocessor
test_that("Data is aggregated to weekly correctly", {
  weekly_data <- load_and_prepare_data(
    timeframe = "1w",
    db_path = TEST_DB_FILE,
    base_table = "spy_daily_test",
    dividend_table = "dividends_test"
  )
  
  # 1. Check that the number of rows is correct (3 weeks in test data)
  expect_equal(nrow(weekly_data), 3)
  
  # 2. Check that the date for the second week is Thursday, Jan 9 (since Jan 10 is a holiday)
  second_week <- weekly_data[2, ]
  expect_equal(second_week$dt, as.Date("2025-01-09"))
  
  # 3. Check that OHLC values are aggregated correctly for the second week
  expect_equal(second_week$open, 102.5)
  expect_equal(second_week$high, 107.0)
  expect_equal(second_week$low, 102.0)
  expect_equal(second_week$close, 106.5)
})

test_that("Dividends are processed and aggregated correctly", {
  weekly_data <- load_and_prepare_data(
    timeframe = "1w",
    db_path = TEST_DB_FILE,
    base_table = "spy_daily_test",
    dividend_table = "dividends_test"
  )
  
  # 1. The first week should have zero dividends
  expect_equal(weekly_data[1, ]$dividend, 0)
  
  # 2. The second week should have the sum of the two dividends (0.50 + 0.25)
  expect_equal(weekly_data[2, ]$dividend, 0.75)
  
  # 3. The third week should have zero dividends
  expect_equal(weekly_data[3, ]$dividend, 0)
})

test_that("Dividend processing is optional", {
  weekly_data_no_div <- load_and_prepare_data(
    timeframe = "1w",
    db_path = TEST_DB_FILE,
    base_table = "spy_daily_test",
    dividend_table = NULL
  )
  
  expect_true("dividend" %in% names(weekly_data_no_div))
  expect_equal(sum(weekly_data_no_div$dividend), 0)
})

test_that("Daily data is passed through correctly", {
  daily_data <- load_and_prepare_data(
    timeframe = "1d",
    db_path = TEST_DB_FILE,
    base_table = "spy_daily_test",
    dividend_table = NULL
  )
  
  expect_equal(nrow(daily_data), 9)
  expect_equal(daily_data[daily_data$dt == as.Date('2025-01-01'), ]$open, 100.0)
})

# Clean up the test database file after all tests are run
file.remove(TEST_DB_FILE)