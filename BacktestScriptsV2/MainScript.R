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
# 2. DATA PREPARATION & SIGNAL GENERATION
# =================================================================================
sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScriptsV2/ultimate_smoother.cpp")

data_with_smoother <- as_tibble(monthly_data) %>%
  mutate(dt = as.Date(dt),
         rn = row_number(),
         oc2 = (open + close) / 2) %>%
  bind_cols(ultimateSmootherTbl(.$oc2, 10, 1))

data_for_backtest <- data_with_smoother %>%
  filter(dt >= as.Date('2000-01-01')) %>%
  drop_na(UltimateSmoother)


# Prepare dividend data for the config
dividend_df <- data_for_backtest %>%
  filter(!is.na(dividend)) %>%
  select(ex_date = dt, dividend_amount = dividend)

# =================================================================================
# 3. BACKTEST EXECUTION
# =================================================================================

# --- Configuration for the Ultimate Smoother strategy ---
strategy_cfg <- list(
  initial_equity = 100000.0,
  trade_mode = "LONG",
  time_frame = "1mo",
  entry_timing = "CLOSE",
  exit_timing = "CLOSE",
  slippage_pct = 0.0005,
  commission_per_trade = 1.50,
  dividend_data = dividend_df,
  long_entry = function(df) {
    df %>%
      mutate(signal = ifelse(rn > 1 & (oc2 > UltimateSmoother & lag(oc2) <= lag(UltimateSmoother)), 1, 0)) %>%
      pull(signal)
  },
  long_exit = function(df) {
    df %>%
      mutate(signal = ifelse(rn > 1 & (oc2 < UltimateSmoother & lag(oc2) >= lag(UltimateSmoother)), 1, 0)) %>%
      pull(signal)
  }
)

# --- Run the Strategy backtest ---
cat("\n--- Running Ultimate Smoother Strategy Backtest ---
")
strategy_results <- run_backtest_r(data_for_backtest, strategy_cfg)

# --- Print results ---
print(head(strategy_results, 25))
cat("\n...\n")
print(tail(strategy_results, 25))

