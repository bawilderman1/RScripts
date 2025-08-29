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
  # This is expected to FAIL initially until we modify the C++ handler
  print("Running BUY_AND_HOLD test...")
  results <- run_backtest_r(ohlc_df_bnh, config_bnh)
  print("BUY_AND_HOLD test complete.")
  
  # 4. Assertions
  # The final equity should be the initial equity multiplied by the price appreciation
  # Price appreciation = (last close / first close)
  expected_final_equity <- 10000 * (108 / 102)
  
  # We expect the final row of the results to have the correct equity
  # Correct calculation must account for whole shares
  first_close <- head(ohlc_df_bnh$close, 1)
  last_close <- tail(ohlc_df_bnh$close, 1)
  initial_equity <- config_bnh$initial_equity
  
  shares_bought <- floor(initial_equity / first_close)
  cash_remains <- initial_equity - (shares_bought * first_close)
  expected_final_equity <- cash_remains + (shares_bought * last_close)
  
  expect_equal(tail(results$equity, 1), expected_final_equity, tolerance = 1e-6)
  expect_equal(nrow(results), nrow(ohlc_df_bnh)) # Should have one row per bar
})


# --- Test Case 2: LONG Strategy with Signals --- 
test_that("run_backtest_r handles LONG strategy with signals correctly", {

  # 1. Mock OHLC data with signal columns
  ohlc_df_long <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 104, 106),
    high = c(103, 105, 107, 109),
    low = c(99, 101, 103, 105),
    close = c(102, 104, 106, 108),
    entry_signal = c(0, 1, 0, 0), # Enter on the second bar
    exit_signal = c(0, 0, 0, 1)   # Exit on the last bar
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
  print("Running LONG strategy test...")
  results <- run_backtest_r(ohlc_df_long, config_long)
  print("LONG strategy test complete.")

  # 4. Assertions (simple check, can be made more specific)
  expect_true(is.data.frame(results))
  expect_equal(nrow(results), nrow(ohlc_df_long))
  
})

print("All tests defined. Running tests...")