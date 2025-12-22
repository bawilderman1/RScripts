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
TIME_FRAME <- "1w" # Options: "1d", "1w", "1mo"
DB_PATH <- "C:/Users/bawil/Documents/StockData/Databases/spyanalysis.db"

# =================================================================================
# 2. DATA PREPARATION & SIGNAL GENERATION
# =================================================================================

# --- Load Data (SQL is now managed here) ---
con <- dbConnect(duckdb::duckdb(), dbdir = DB_PATH, read_only = TRUE)
price_data <- dbGetQuery(con, "
  SELECT 
    p.dt, p.open, p.high, p.low, p.close, 
    COALESCE(d.dividend, 0.0) as dividend
  FROM spy_1d_adj p
  LEFT JOIN spy_1d_dividends d ON p.dt = d.dt
  ORDER BY p.dt
")
dbDisconnect(con, shutdown = TRUE)

# --- Aggregate Data using the new Preprocessor ---
# Convert to tibble for easier handling
price_data <- as_tibble(price_data) %>% mutate(dt = as.Date(dt))

aggregated_data <- to_agg_timeframe(
  from_timeframe = "1d",
  to_timeframe = TIME_FRAME,
  time_vec = price_data$dt,
  open = price_data$open,
  high = price_data$high,
  low = price_data$low,
  close = price_data$close,
  dividend = price_data$dividend,
  timestamp_priority = "last" # Use last day for weekly/monthly option-style analysis
)

# --- Calculate indicators ---
data_with_smoother <- aggregated_data %>%
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
  )
)

# Add signal functions to the config
strategy_cfg$short_entry <- function(df) {
  df %>%
    mutate(signal = ifelse(rn > 1 & (oc2 < smoother_10 & lag(oc2) >= lag(smoother_10)), 1, 0)) %>%
    pull(signal)
}
strategy_cfg$short_exit <- function(df) {
  df %>%
    mutate(signal = ifelse(rn > 1 & (oc2 > smoother_9 & lag(oc2) <= lag(smoother_9)), 1, 0)) %>%
    pull(signal)
}

# --- Run the Strategy backtest ---
cat("\n--- Running Ultimate Smoother Strategy Backtest ---
")
strategy_results <- run_backtest_r(data_for_backtest, strategy_cfg)

# --- Print results ---
print(head(strategy_results, 25))
cat("\r
...
")
print(tail(strategy_results, 25))

# --- Ad-hoc Analysis ---
# Join the period start/end dates from our input data to the results
results_with_periods <- strategy_results %>%
  inner_join(select(data_for_backtest, dt, period_start_dt, period_end_dt), by = "dt")

# Re-create the dividend_df needed for the ad-hoc script from the raw daily data
dividend_df_for_adhoc <- price_data %>%
  filter(dividend > 0) %>%
  select(ex_date = dt, dividend_amount = dividend)

calculate_short_dividend_cost(results_with_periods, dividend_df_for_adhoc, strategy_cfg)
