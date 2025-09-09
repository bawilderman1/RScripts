library(testthat)
library(Rcpp)

# Compile the C++ code once for all tests
# Using verbose=TRUE to get detailed compiler output if it fails
print("Compiling BacktestHandler.cpp...")
Rcpp::sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/BacktestHandler.cpp", verbose = TRUE)
print("Compilation complete.")

# --- Test Case 1: BUY_AND_HOLD Strategy --- 
test_that("run_backtest_r handles BUY_AND_HOLD strategy correctly", {
  
  ohlc_df_bnh <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 104, 106),
    high = c(103, 105, 107, 109),
    low = c(99, 101, 103, 105),
    close = c(102, 104, 106, 108)
  )
  
  config_bnh <- list(
    initial_equity = 10000,
    trade_mode = "BUY_AND_HOLD",
    time_frame = "1d",
    entry_timing = "CLOSE",
    exit_timing = "CLOSE",
    slippage_pct = 0.0,
    commission_per_trade = 0.0
  )
  
  results <- run_backtest_r(ohlc_df_bnh, config_bnh)
  
  first_close <- 102
  last_close <- 108
  initial_equity <- 10000
  
  shares_bought <- floor(initial_equity / first_close)
  cash_remains <- initial_equity - (shares_bought * first_close)
  expected_final_equity <- cash_remains + (shares_bought * last_close)
  
  expect_equal(tail(results$equity, 1), round(expected_final_equity, 2))
  expect_equal(nrow(results), nrow(ohlc_df_bnh))
})


# --- Test Case 2: LONG Strategy with Signal Columns --- 
test_that("run_backtest_r handles LONG strategy with signal columns correctly", {

  ohlc_df_long_cols <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 104, 106),
    high = c(103, 105, 107, 109),
    low = c(99, 101, 103, 105),
    close = c(102, 104, 106, 108),
    long_entry_signal = c(0, 1, 0, 0), 
    long_exit_signal = c(0, 0, 0, 1)
  )

  config_long <- list(
    initial_equity = 10000,
    trade_mode = "LONG",
    time_frame = "1d",
    entry_timing = "CLOSE",
    exit_timing = "CLOSE",
    slippage_pct = 0.001,
    commission_per_trade = 6.0
  )

  results <- run_backtest_r(ohlc_df_long_cols, config_long)

  expect_true(is.data.frame(results))
  expect_equal(nrow(results), nrow(ohlc_df_long_cols))
  
})

# --- Test Case 3: LONG Strategy with R Functions --- 
test_that("run_backtest_r handles LONG strategy with R functions correctly", {

  ohlc_df_long_funcs <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 104, 106),
    high = c(103, 105, 107, 109),
    low = c(99, 101, 103, 105),
    close = c(102, 104, 106, 108),
    indicator = c(5, 15, 10, 5)
  )

  long_entry_func <- function(df) { df$indicator > 12 }
  long_exit_func <- function(df) { df$indicator < 6 }

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

  results <- run_backtest_r(ohlc_df_long_funcs, config_long_funcs)

  expect_true("Date" %in% class(results$dt))
  expect_equal(tail(results$equity, 1), 10351.65, tolerance = 1e-2)

})

# --- Test Case 4: SHORT Strategy with R Functions --- 
test_that("run_backtest_r handles SHORT strategy with R functions correctly", {

  ohlc_df_short_funcs <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 102, 98, 96),
    high = c(103, 105, 99, 97),
    low = c(99, 101, 97, 95),
    close = c(102, 100, 98, 96),
    indicator = c(15, 5, 10, 15)
  )

  short_entry_func <- function(df) { df$indicator < 6 }
  short_exit_func <- function(df) { df$indicator > 12 }

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

  results <- run_backtest_r(ohlc_df_short_funcs, config_short_funcs)

  expect_equal(tail(results$equity, 1), 10368.40, tolerance = 1e-2)

})

# --- Test Case 5: PNL and Data Type Verification ---
test_that("run_backtest_r calculates PNL correctly and returns correct types", {

  ohlc_df <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
    open = c(100, 102, 104, 106, 108),
    high = c(103, 105, 107, 109, 110),
    low = c(99, 101, 103, 105, 107),
    close = c(102, 104, 106, 108, 109),
    long_entry_signal = c(0, 1, 0, 0, 0),
    long_exit_signal = c(0, 0, 0, 1, 0)
  )

  config <- list(
    initial_equity = 10000,
    trade_mode = "LONG",
    time_frame = "1d",
    entry_timing = "CLOSE",
    exit_timing = "CLOSE",
    slippage_pct = 0.0,
    commission_per_trade = 5.0
  )

  results <- run_backtest_r(ohlc_df, config)

  final_realized_pnl <- tail(results$realized_pnl, 1)
  expect_equal(final_realized_pnl, 384.00)

  pnl_in_trade <- results$pnl_log_change_pct[3:4]
  expect_true(all(pnl_in_trade != 0))

  expect_equal(results$pnl_log_change_pct[1], 0)
  expect_equal(results$pnl_log_change_pct[5], 0)
  
  expect_true("Date" %in% class(results$dt))
})

# --- Test Case 6: No Trades Scenario ---
test_that("run_backtest_r handles no trades correctly", {
  ohlc_df <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02")),
    open = c(100, 102), high = c(103, 105), low = c(99, 101), close = c(102, 104),
    long_entry_signal = c(0, 0),
    long_exit_signal = c(0, 0)
  )
  config <- list(initial_equity = 10000, trade_mode = "LONG", time_frame = "1d",
                 entry_timing = "CLOSE", exit_timing = "CLOSE", slippage_pct = 0, commission_per_trade = 0)

  results <- run_backtest_r(ohlc_df, config)

  expect_equal(nrow(results), 2)
  expect_true(all(results$equity == 10000))
  expect_true(all(results$realized_pnl == 0))
  expect_true(all(results$trade_number == 0))
})

# --- Test Case 7: Stop-Loss Trigger ---
test_that("run_backtest_r triggers stop-loss and sets exit_reason correctly", {
  ohlc_df <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 105, 106, 90),
    high = c(102, 106, 107, 95),
    low = c(98, 104, 85, 88),
    close = c(101, 105.5, 88, 92),
    long_entry_signal = c(1, 0, 0, 0),
    long_exit_signal = c(0, 0, 0, 0)
  )
  config <- list(
    initial_equity = 10000, trade_mode = "LONG", time_frame = "1d",
    entry_timing = "OPEN", exit_timing = "CLOSE", slippage_pct = 0, commission_per_trade = 0,
    risk_config = list(stop_loss_pct = 0.10)
  )

  results <- run_backtest_r(ohlc_df, config)

  # Stop-loss triggers on bar 3 (index 2), where low (85) < stop_price (90)
  expect_equal(results$exit_reason[3], "STOP_LOSS")
  expect_equal(results$exit_reason[4], "") # Should be empty on subsequent bars

  final_pnl <- tail(results$realized_pnl, 1)
  expect_equal(final_pnl, -1000.00)
  expect_equal(tail(results$position_state, 1), "flat")
})

# --- Test Case 8: Monthly Data Integrity ---
test_that("Date intervals are handled correctly", {
  ohlc_df <- data.frame(
    dt = seq(as.Date("2023-01-01"), by = "month", length.out = 12),
    open = 100:111,
    high = 100:111,
    low = 100:111,
    close = 100:111,
    long_entry_signal = c(1,0,0,0,0,0,0,0,0,0,0,0),
    long_exit_signal = c(0,0,0,0,0,0,0,0,0,0,0,1)
  )

  config <- list(initial_equity = 10000, trade_mode = "LONG", time_frame = "1mo",
                 entry_timing = "CLOSE", exit_timing = "CLOSE", slippage_pct = 0, commission_per_trade = 0)

  results <- run_backtest_r(ohlc_df, config)

  expect_equal(nrow(results), nrow(ohlc_df))
  expect_true(all(diff(results$dt) > 27 & diff(results$dt) < 32))
})

# --- Test Case 9: Trade Drawdown Calculation ---
test_that("Trade drawdown is calculated correctly", {
  ohlc_df <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04", "2023-01-05")),
    open = c(100, 105, 110, 108, 106),
    high = c(102, 106, 112, 109, 107),
    low = c(98, 104, 108, 107, 105),
    close = c(105, 110, 108, 106, 104),
    long_entry_signal = c(1, 0, 0, 0, 0),
    long_exit_signal = c(0, 0, 0, 0, 1)
  )
  
  config <- list(initial_equity = 10000, trade_mode = "LONG", time_frame = "1d",
                 entry_timing = "OPEN", exit_timing = "CLOSE", slippage_pct = 0, commission_per_trade = 0)

  results <- run_backtest_r(ohlc_df, config)

  expect_equal(results$trade_drawdown_pct[4], 0.0364, tolerance = 1e-4)
  expect_equal(results$trade_drawdown_pct[5], 0)
})

# --- Test Case 10: Dividend Reinvestment ---
test_that("Dividends are reinvested correctly for LONG positions", {
  ohlc_df <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03")),
    open = c(100, 102, 104),
    high = c(103, 105, 107),
    low = c(99, 101, 103),
    close = c(102, 104, 106)
  )
  
  dividend_df <- data.frame(
    ex_date = as.Date(c("2023-01-02")),
    dividend_amount = 0.50
  )

  config <- list(
    initial_equity = 10000,
    trade_mode = "BUY_AND_HOLD",
    time_frame = "1d",
    entry_timing = "OPEN",
    exit_timing = "CLOSE",
    slippage_pct = 0,
    commission_per_trade = 0,
    dividend_data = dividend_df
  )

  results <- run_backtest_r(ohlc_df, config)

  # Entry on bar 1 at 100. Shares = 100.
  # Dividend of 0.50 on bar 2. Cash dividend = 100 * 0.50 = 50.
  # Reinvest at close of bar 2, which is 104.
  # New shares = 50 / 104 = 0.480769...
  # Total shares after reinvestment = 100 + 0.481 = 100.481
  expect_equal(results$share_quantity[2], 100.481, tolerance = 1e-3)
  # Position is closed on the last bar, so share quantity should be 0
  expect_equal(tail(results$share_quantity, 1), 0)
})

# --- Test Case 11: Time-Based Stop ---
test_that("run_backtest_r handles time-based stop correctly", {
  ohlc_df <- data.frame(
    dt = seq(as.Date("2023-01-01"), by = "day", length.out = 10),
    open = seq(100, 109),
    high = seq(101, 110),
    low = seq(99, 108),
    close = seq(100, 109),
    long_entry_signal = c(1, 0, 0, 0, 0, 0, 0, 0, 0, 0),
    long_exit_signal =  c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )

  config <- list(
    initial_equity = 10000,
    trade_mode = "LONG",
    time_frame = "1d",
    entry_timing = "OPEN",
    exit_timing = "CLOSE",
    slippage_pct = 0,
    commission_per_trade = 0,
    risk_config = list(max_bars_in_trade = 4)
  )

  results <- run_backtest_r(ohlc_df, config)

  # Enters on bar 1 (index 0).
  # Held for bars 1, 2, 3, 4 (indices 0, 1, 2, 3). Total 4 bars.
  # Exit should be triggered on bar 5 (index 4).
  expect_equal(results$position_state[1], "long")
  expect_equal(results$position_state[4], "long")
  expect_equal(results$position_state[5], "flat")
  expect_equal(tail(results$position_state, 1), "flat")
})

# --- Test Case 12: Fractional Sell on SHORT position ---
test_that("run_backtest_r handles fractional buy-to-cover for SHORT positions", {
  ohlc_df <- data.frame(
    dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
    open = c(100, 98, 96, 94),
    high = c(101, 99, 97, 95),
    low = c(99, 97, 95, 93),
    close = c(100, 98, 96, 94),
    short_entry_signal = c(1, 0, 0, 0),
    short_exit_signal = c(0, 0, 0, 0)
  )

  config <- list(
    initial_equity = 10000,
    trade_mode = "SHORT",
    time_frame = "1d",
    entry_timing = "OPEN",
    exit_timing = "CLOSE",
    slippage_pct = 0,
    commission_per_trade = 0,
    risk_config = list(
      fractional_sells = data.frame(
        profit_target_pct = c(0.05), # 5% profit (price drops to 95)
        fraction_to_sell = c(0.5)    # Cover 50% of the position
      )
    )
  )

  results <- run_backtest_r(ohlc_df, config)

  # Short entry on bar 1 at 100. Shares = 10000 / 100 = 100.
  initial_shares <- 100
  
  # Profit target is 100 * (1 - 0.05) = 95.
  # On bar 3, low is 95, so partial exit should trigger.
  # 50% of 100 shares (50) should be bought back.
  
  expect_equal(results$share_quantity[2], initial_shares) # Before exit
  expect_equal(results$share_quantity[3], initial_shares / 2) # After partial exit
  expect_equal(results$position_state[3], "short") # Still short
  expect_true(results$realized_pnl[3] > 0) # Should have realized a profit
  expect_equal(results$exit_reason[3], "PARTIAL_TAKE_PROFIT")
})


print("All tests defined. Running tests...")