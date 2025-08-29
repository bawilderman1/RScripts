library(testthat)
library(Rcpp)

# Compile the C++ code once for all tests
# Using verbose=TRUE to get detailed compiler output if it fails
print("Compiling BacktestHandler.cpp...")
Rcpp::sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/BacktestHandler.cpp", verbose = TRUE)
print("Compilation complete.")

# --- Test Case 1: BUY_AND_HOLD Strategy --- 
test_that("run_backtest_r handles BUY_AND_HOLD strategy correctly", {
  
  # 1. Mock OHLC data (no signal columns needed)
  ohlc_df_bnh <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 104, 106),
    high = c(103, 105, 107, 109),
    low = c(99, 101, 103, 105),
    close = c(102, 104, 106, 108)
  )
  
  # 2. Config list for BUY_AND_HOLD (no functions/signals)
  config_bnh <- list(
    initial_equity = 10000,
    trade_mode = "BUY_AND_HOLD",
    time_frame = "1d",
    entry_timing = "CLOSE",
    exit_timing = "CLOSE",
    slippage_pct = 0.0,
    commission_per_trade = 0.0
  )
  
  # 3. Call the C++ function
  print("Running BUY_AND_HOLD test...")
  results <- run_backtest_r(ohlc_df_bnh, config_bnh)
  print("BUY_AND_HOLD test complete.")
  
  # 4. Assertions
  # Correct calculation must account for whole shares
  first_close <- head(ohlc_df_bnh$close, 1)
  last_close <- tail(ohlc_df_bnh$close, 1)
  initial_equity <- config_bnh$initial_equity
  
  shares_bought <- floor(initial_equity / first_close)
  cash_remains <- initial_equity - (shares_bought * first_close)
  expected_final_equity <- cash_remains + (shares_bought * last_close)
  
  expect_equal(tail(results$equity, 1), expected_final_equity, tolerance = 1e-6)
  expect_equal(nrow(results), nrow(ohlc_df_bnh))
})


# --- Test Case 2: LONG Strategy with Signal Columns --- 
test_that("run_backtest_r handles LONG strategy with signal columns correctly", {

  # 1. Mock OHLC data with signal columns
  ohlc_df_long_cols <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 104, 106),
    high = c(103, 105, 107, 109),
    low = c(99, 101, 103, 105),
    close = c(102, 104, 106, 108),
    long_entry_signal = c(0, 1, 0, 0), # Enter on the second bar
    long_exit_signal = c(0, 0, 0, 1)   # Exit on the last bar
  )

  # 2. Config list for LONG strategy
  config_long <- list(
    initial_equity = 10000,
    trade_mode = "LONG",
    time_frame = "1d",
    entry_timing = "CLOSE",
    exit_timing = "CLOSE",
    slippage_pct = 0.001,
    commission_per_trade = 6.0
  )

  # 3. Call the C++ function
  print("Running LONG strategy (columns) test...")
  results <- run_backtest_r(ohlc_df_long_cols, config_long)
  print("LONG strategy (columns) test complete.")

  # 4. Assertions (simple check, can be made more specific)
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), nrow(ohlc_df_long_cols))
  
})

# --- Test Case 3: LONG Strategy with R Functions --- 
test_that("run_backtest_r handles LONG strategy with R functions correctly", {

  # 1. Mock OHLC data (no signal columns)
  # We'll add a dummy 'indicator' column for the function to use
  ohlc_df_long_funcs <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 104, 106),
    high = c(103, 105, 107, 109),
    low = c(99, 101, 103, 105),
    close = c(102, 104, 106, 108),
    indicator = c(5, 15, 10, 5) # Dummy indicator
  )

  # 2. Define R signal functions
  long_entry_func <- function(df) { df$indicator > 12 }
  long_exit_func <- function(df) { df$indicator < 6 }

  # 3. Config list with R functions
  config_long_funcs <- list(
    initial_equity = 10000,
    trade_mode = "LONG",
    time_frame = "1d",
    entry_timing = "CLOSE",
    exit_timing = "CLOSE",
    slippage_pct = 0.001,
    commission_per_trade = 6.0,
    long_entry = long_entry_func,
    long_exit = long_exit_func
  )

  # 4. Call the C++ function
  print("Running LONG strategy (functions) test...")
  results <- run_backtest_r(ohlc_df_long_funcs, config_long_funcs)
  print("LONG strategy (functions) test complete.")

  # 5. Assertions
  # Entry price: 104 (close of bar 2). Exit price: 108 (close of bar 4)
  # Shares: floor((10000 - 6) / (104 * 1.001)) = 96
  # Final Equity: 10351.648
  expect_equal(tail(results$equity, 1), 10351.648, tolerance = 1e-3)

})

# --- Test Case 4: SHORT Strategy with R Functions --- 
test_that("run_backtest_r handles SHORT strategy with R functions correctly", {

  # 1. Mock OHLC data
  ohlc_df_short_funcs <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 98, 96),
    high = c(103, 105, 99, 97),
    low = c(99, 101, 97, 95),
    close = c(102, 100, 98, 96),
    indicator = c(15, 5, 10, 15) # Dummy indicator
  )

  # 2. Define R signal functions
  short_entry_func <- function(df) { df$indicator < 6 }
  short_exit_func <- function(df) { df$indicator > 12 }

  # 3. Config list with R functions
  config_short_funcs <- list(
    initial_equity = 10000,
    trade_mode = "SHORT",
    time_frame = "1d",
    entry_timing = "CLOSE",
    exit_timing = "CLOSE",
    slippage_pct = 0.001,
    commission_per_trade = 6.0,
    short_entry = short_entry_func,
    short_exit = short_exit_func
  )

  # 4. Call the C++ function
  # This is expected to FAIL until we modify the C++ handler
  print("Running SHORT strategy (functions) test...")
  results <- run_backtest_r(ohlc_df_short_funcs, config_short_funcs)
  print("SHORT strategy (functions) test complete.")

  # 5. Assertions
  # Entry (short) on bar 2 at close 100. Exit on bar 4 at close 96.
  # Entry price: 100 * (1 - 0.001) = 99.9
  # Shares: floor((10000 - 6) / 99.9) = floor(9994 / 99.9) = 100
  # Cash after entry: 10000 + (100 * 99.9) - 6 = 19984
  # Exit price: 96 * (1 + 0.001) = 96.096
  # Cost to cover: 100 * 96.096 = 9609.6
  # Cash after exit: 19984 - 9609.6 - 6 = 10368.4
  expect_equal(tail(results$equity, 1), 10368.4, tolerance = 1e-3)

})


print("All tests defined. Running tests...")