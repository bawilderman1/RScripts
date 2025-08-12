rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)

#library(conflicted)
library(tidyverse)
library(DBI)
library(duckdb)
#library(MASS)
#library(ggiraph)
#library(scales)
#library(ggthemes)
library(lubridate)
#library(patchwork)
#library(RColorBrewer)
#library(PerformanceAnalytics)
library(slider)
#library(tsbox)
library(purrr)
library(glue)
library(gt)
#library(highcharter)
#library(quantmod)
library(Rcpp)

source("C:/Users/bawil/Documents/RScripts/BacktestScripts/backtest_engine_gemini.R")

# Get Data

con <- dbConnect(duckdb::duckdb(), dbdir = "C:/Users/bawil/Documents/StockData/Databases/spyanalysis.db", read_only = FALSE)

result <- dbGetQuery(
  con,
  "WITH spy_dividend_cte AS (
  	SELECT 
  		time_bucket(to_months(1), dt) as dt,
  		max(dividend) AS dividend,
  	FROM spy_1d_dividends
  	where dt >= '1993-02-01'
  	group by time_bucket(to_months(1), dt)
  	order by time_bucket(to_months(1), dt)
  )
  SELECT m.*, 
  	hl.* EXCLUDE (dt),
  	cd.* EXCLUDE (dt),
  	d.* EXCLUDE (dt),
  	ga.* EXCLUDE (dt),
  	sp.* EXCLUDE (dt),
  	fc.* EXCLUDE (dt),
  	div.* EXCLUDE(dt),
  FROM spy_monthly m
  	LEFT JOIN spy_monthly_highlow hl ON m.dt = hl.dt
  	LEFT JOIN spy_monthly_consec_dir cd ON m.dt = cd.dt
  	LEFT JOIN spy_monthly_decycler125 d ON m.dt = d.dt
  	LEFT JOIN spy_monthly_geomavg5 ga ON m.dt = ga.dt
  	LEFT JOIN spy_monthly_augenspikes sp ON m.dt = sp.dt
  	LEFT JOIN spy_monthly_factorcalcs fc ON m.dt = fc.dt
  	LEFT JOIN spy_dividend_cte div ON m.dt = div.dt
  ORDER BY m.dt;")

## Bactest Setup/Run

### Initial Setup

sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScripts/ultimate_smoother.cpp")

test_strat_cfg <- list(
  trade_mode = TradeDirection$SHORT,
  entry_timing = TradeTiming$CLOSE,
  exit_timing = TradeTiming$CLOSE,
  init_eqty = 10000,
  incl_buynhold = TRUE,
  incl_dividends = TRUE
)

test_strtgy_entry <- \(., strat_cfg) {
  ifelse(.$rn > 1 & (.$oc2 < .$UltimateSmoother & lag(.$oc2) >= lag(.$UltimateSmoother)), 
         1, 0)
}
test_strtgy_exit <- \(., strat_cfg) {
  ifelse(.$rn > 1 & (.$oc2 > .$UltimateSmoother & lag(.$oc2) <= lag(.$UltimateSmoother)), 
         1, 0)
}

bt_strtgy <- result |>
  mutate(oc2 = (open + close) / 2,
         ultimateSmootherTbl(oc2, 10, 1)) |>
  filter(year(dt) %in% c(2000:2020)) |>
  mutate(rn = row_number(),
         mth_nbr = month(dt),
         dividend = if_else(is.na(dividend), 0, dividend)) |>
  select(dt, open, high, low, close, rn, mth_nbr, dividend, oc2, UltimateSmoother)

bt_metrics <- bt_processor(bt_strtgy,
                           test_strtgy_entry,
                           test_strtgy_exit,
                           test_strat_cfg)

bt_metrics$rslts_viz()

## Data Cleanup

dbDisconnect(con, shutdown=TRUE)