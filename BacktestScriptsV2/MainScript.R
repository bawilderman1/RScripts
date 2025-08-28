library(tidyverse)
library(DBI)
library(duckdb)
library(lubridate)
library(Rcpp)

# Source the C++ handler, which creates the 'run_backtest_r' function
Rcpp::sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/BacktestHandler.cpp")

# =================================================================================
# 1. DATA LOADING
# =================================================================================
con <- dbConnect(duckdb::duckdb(), dbdir = "C:/Users/bawil/Documents/StockData/Databases/spyanalysis.db", read_only = TRUE)
monthly_data <- dbGetQuery(
  con,
  "SELECT
    m.dt, m.open, m.high, m.low, m.close, d.dividend
   FROM spy_monthly m
   LEFT JOIN (SELECT
                time_bucket(to_months(1), dt) as dt, max(dividend) AS dividend 
              FROM spy_1d_dividends
              WHERE dt >= '1993-02-01' GROUP BY 1) d ON m.dt = d.dt
   ORDER BY m.dt;"
)
dbDisconnect(con, shutdown=TRUE)

# =================================================================================
# 2. DATA PREPARATION & SIGNAL GENERATION (SIMPLIFIED)
# =================================================================================
data_for_backtest <- as_tibble(monthly_data) %>%
  mutate(dt = as.Date(dt)) %>%
  drop_na(open, high, low, close) %>%
  mutate(
    # Using simple row number for signals, avoiding ultimate_smoother
    entry_signal = ifelse(row_number() == 20, 1, 0),
    exit_signal = ifelse(row_number() == 30, 1, 0)
  )

# Prepare dividend data for the config
dividend_df <- data_for_backtest %>%
  filter(!is.na(dividend)) %>%
  select(ex_date = dt, dividend_amount = dividend)

# =================================================================================
# 3. BACKTEST EXECUTION
# =================================================================================

# --- Configuration for the simplified strategy backtest ---
strategy_cfg <- list(
  initial_equity = 100000.0,
  trade_mode = "LONG",
  time_frame = "1mo",
  entry_timing = "CLOSE",
  exit_timing = "CLOSE",
  slippage_pct = 0.0005,
  commission_per_trade = 1.50,
  dividend_data = dividend_df
)

# --- Run the Strategy backtest ---
cat("\n--- Running Simplified Strategy Backtest ---\n")
strategy_results <- run_backtest_r(data_for_backtest, strategy_cfg)

# --- Print results ---
print(head(strategy_results, 25))
cat("\n...\n")
print(tail(strategy_results, 25))
