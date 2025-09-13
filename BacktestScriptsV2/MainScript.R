library(tidyverse)
library(DBI)
library(duckdb)
library(lubridate)
library(Rcpp)

# =================================================================================
# 1. SOURCING & CONFIGURATION
# =================================================================================

# --- Source all external scripts ---
source("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/Preprocessor.R")
Rcpp::sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/BacktestHandler.cpp")
sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/ultimate_smoother.cpp")
source("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/AdhocCalcsScript.R")

# --- Main Configuration ---
TIME_FRAME <- "1mo" # Options: "1d", "1w", "1mo"
DB_PATH <- "C:/Users/bawil/Documents/StockData/Databases/spyanalysis.db"
BASE_TABLE <- "spy_1d_adj" # Base table for prices
DIVIDEND_TABLE <- "spy_1d_dividends" # Set to NULL to disable dividends

# =================================================================================
# 2. DATA PREPARATION & SIGNAL GENERATION
# =================================================================================

# --- Load and prepare data using the preprocessor ---
all_data <- load_and_prepare_data(
  timeframe = TIME_FRAME,
  db_path = DB_PATH,
  base_table = BASE_TABLE,
  dividend_table = DIVIDEND_TABLE
)

# --- Calculate indicators ---
data_with_smoother <- all_data %>%
  mutate(
    rn = row_number(),
    oc2 = (open + close) / 2,
    smoother_10 = ultimateSmoother(oc2, 10, 1),
    smoother_9  = ultimateSmoother(oc2, 9, 1)
  )

# --- Filter data for the backtest period ---
data_for_backtest <- data_with_smoother %>%
  filter(dt >= as.Date('2000-01-01')) %>%
  drop_na(smoother_10, smoother_9)

# =================================================================================
# 3. BACKTEST EXECUTION
# =================================================================================

# --- Configuration for the Ultimate Smoother strategy ---
strategy_cfg <- list(
  initial_equity = 100000.0,
  trade_mode = "SHORT",
  time_frame = TIME_FRAME,
  entry_timing = "CLOSE",
  exit_timing = "CLOSE",
  slippage_pct = 0.0005,
  commission_per_trade = 1.50,
  risk_config = list(
    stop_loss_pct = 0.05, # 5% stop-loss
    take_profit_pct = 0.10 # 10% take-profit
  ),
  # NOTE: dividend_data is no longer needed here; it's part of the main data frame.
  short_entry = function(df) {
    df %>%
      mutate(signal = ifelse(rn > 1 & (oc2 < smoother_10 & lag(oc2) >= lag(smoother_10)), 1, 0)) %>%
      pull(signal)
  },
  short_exit = function(df) {
    df %>%
      mutate(signal = ifelse(rn > 1 & (oc2 > smoother_9 & lag(oc2) <= lag(smoother_9)), 1, 0)) %>%
      pull(signal)
  }
)

# --- Run the Strategy backtest ---
cat("\n--- Running Ultimate Smoother Strategy Backtest ---
")
strategy_results <- run_backtest_r(data_for_backtest, strategy_cfg)

# --- Print results ---
print(head(strategy_results, 25))
cat("\r
...\n")
print(tail(strategy_results, 25))

# --- Ad-hoc Analysis ---
# Re-create the dividend_df needed for the ad-hoc script from the results
dividend_df_for_adhoc <- all_data %>%
  filter(dividend > 0) %>%
  select(ex_date = dt, dividend_amount = dividend)

calculate_short_dividend_cost(strategy_results, dividend_df_for_adhoc, strategy_cfg)
