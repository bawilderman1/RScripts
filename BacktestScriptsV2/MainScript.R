library(tidyverse)
library(DBI)
library(duckdb)
library(lubridate)
library(slider)
library(purrr)
library(glue)
library(gt)
library(Rcpp)

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

# Backtest Functions

# TODO: Need to map and wrap Enum's from BacktestEngine.cpp into BacktestHandler.cpp with RCPP package
TradeTiming <- list(
  OPEN = "open",
  CLOSE = "close",
  NEXT_OPEN = "next_open"
)

# Bactest Setup/Run

sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScripts/ultimate_smoother.cpp")
sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScripts/BacktestEngine.cpp")
sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScripts/BacktestHandler.cpp")

# TODO: Need to properly map and wrap Config struct from BacktestEngine.cpp into BacktestHandler.cpp with RCPP package
strat_cfg <- list(
  entry_timing = TradeTiming$CLOSE,
  exit_timing = TradeTiming$CLOSE,
  init_eqty = 10000,
  incl_buynhold = TRUE,
  incl_dividends = TRUE
)

# TODO: These are the Long_Only Entry and Exit for the strategy to test
strtgy_entry <- \(.) {
  ifelse(.$rn > 1 & (.$oc2 > .$UltimateSmoother & lag(.$oc2) <= lag(.$UltimateSmoother)), 
         1, 0)
}
strtgy_exit <- \(.) {
  ifelse(.$rn > 1 & (.$oc2 < .$UltimateSmoother & lag(.$oc2) >= lag(.$UltimateSmoother)), 
         1, 0)
}

# TODO: Need to properly map and wrap run_backtest from BacktestEngine.cpp into BacktestHandler.cpp with RCPP package
# can use main from BacktestEngine.cpp as an example
# Would like to get back the resulting data table from the backtest processing and save it to a tibble that I can then call with view()

dbDisconnect(con, shutdown=TRUE)  