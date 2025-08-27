library(Rcpp)

# 1. Create minimal mock OHLC data frame
ohlc_df <- data.frame(
  dt = as.Date(c("2023-01-01", "2023-01-02", "2023-01-03", "2023-01-04")),
  open = c(100, 102, 104, 106),
  high = c(103, 105, 107, 109),
  low = c(99, 101, 103, 105),
  close = c(102, 104, 106, 108),
  entry_signal = c(0, 1, 0, 0), # Enter on the second bar
  exit_signal = c(0, 0, 0, 1)   # Exit on the last bar
)

# 2. Create minimal mock config list
config_list <- list(
  initial_equity = 10000,
  trade_mode = "LONG",
  time_frame = "1d",
  entry_timing = "CLOSE",
  exit_timing = "CLOSE",
  slippage_pct = 0.001,
  commission_per_trade = 6.0
)

# 3. Compile and run
# Using verbose=TRUE to get detailed compiler output if it fails
Rcpp::sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/BacktestHandler.cpp", verbose = TRUE)

# 4. Call the C++ function
results <- run_backtest_r(ohlc_df, config_list)

# 5. Print results
print(results)
