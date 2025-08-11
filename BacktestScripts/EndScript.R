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

cagr_func <- function(st_eqty, end_eqty, num_yrs) 
{
  ((end_eqty / st_eqty) ^ (1 / num_yrs)) - 1
}
max_dd_func <- function(dd_vctr)
{
  abs(min(dd_vctr))
}
mar_func <- function(cagr, max_dd)
{
  cagr / max_dd
}
sharp_func <- function(pnl_vctr)
{
  mean(pnl_vctr) / sd(pnl_vctr)
}
anlzd_sharp_func <- function(pnl_vctr, anlzd_scale = 12)
{
  sqrt(anlzd_scale) * (mean(pnl_vctr) / sd(pnl_vctr))
}

TradeTiming <- list(
  OPEN = "open",
  CLOSE = "close",
  NEXT_OPEN = "next_open"
)

add_trades_func <- \(tbl, entryFunc, exitFunc, ...)
{
  tbl |>
    mutate(
      trd_entry = entryFunc(tbl, ...),
      trd_exit = exitFunc(tbl, ...)
    )
}

# --- Core Simulator ---
bt_sim_func <- function(tbl,
                        strat_cfg = list(
                          entry_timing = "open",
                          exit_timing = "open",
                          init_eqty = 10000,
                          incl_buynhold = TRUE,
                          incl_dividends = TRUE
                        )) {
  
  tbl <- tbl |> mutate(rn = if (!"rn" %in% names(tbl)) row_number() else rn)
  tbl_rlen <- nrow(tbl)
  
  # Extract strategy config values
  init_eqty <- strat_cfg$init_eqty
  incl_dividends <- strat_cfg$incl_dividends
  incl_buynhold <- strat_cfg$incl_buynhold
  entry_timing <- strat_cfg$entry_timing
  exit_timing <- strat_cfg$exit_timing
  
  # Initialize vectors
  pos_size_vec <- numeric(tbl_rlen)
  pos_val_vec <- numeric(tbl_rlen)
  cash_vec <- numeric(tbl_rlen)
  eqty_vec <- numeric(tbl_rlen)
  eqty_low_vec <- numeric(tbl_rlen)
  eqty_high_vec <- numeric(tbl_rlen)
  bnh_ps_vec <- numeric(tbl_rlen)
  bnh_eqty_vec <- numeric(tbl_rlen)
  
  open_vec <- tbl$open
  close_vec <- tbl$close
  low_vec <- tbl$low
  high_vec <- tbl$high
  div_vec <- if (incl_dividends) tbl$dividend else rep(0, tbl_rlen)
  trd_entry_vec <- as.logical(tbl$trd_entry)
  trd_exit_vec <- as.logical(tbl$trd_exit)
  
  in_trade <- FALSE
  prev_pos_size <- 0
  
  for (i in seq_len(tbl_rlen)) {
    f_row <- i == 1
    
    entry <- trd_entry_vec[i]
    exit <- trd_exit_vec[i]
    
    # --- ENTRY ---
    if (!in_trade && entry) {
      exec_price <- switch(entry_timing,
                           open = open_vec[i],
                           close = close_vec[i],
                           next_open = if (i < tbl_rlen) open_vec[i + 1] else NA_real_
      )
      
      if (!is.na(exec_price)) {
        pos_size_vec[i] <- round(ifelse(f_row, init_eqty, cash_vec[i - 1]) / exec_price, 2)
        cash_vec[i] <- 0
        in_trade <- TRUE
      }
    }
    
    # --- EXIT ---
    else if (in_trade && exit) {
      exec_price <- switch(exit_timing,
                           open = open_vec[i],
                           close = close_vec[i],
                           next_open = if (i < tbl_rlen) open_vec[i + 1] else NA_real_
      )
      
      if (!is.na(exec_price)) {
        cash_vec[i] <- round(prev_pos_size * exec_price, 2)
        pos_size_vec[i] <- 0
        in_trade <- FALSE
      }
    }
    
    # --- HOLDING ---
    else {
      pos_size_vec[i] <- if (f_row) 0 else pos_size_vec[i - 1]
      cash_vec[i] <- if (f_row) init_eqty else cash_vec[i - 1]
    }
    
    prev_pos_size <- pos_size_vec[i]
    
    # --- Dividends into position ---
    if (incl_dividends && div_vec[i] > 0) {
      if (pos_size_vec[i] > 0) {
        pos_size_vec[i] <- pos_size_vec[i] + round((div_vec[i] * pos_size_vec[i]) / open_vec[i], 2)
      } else {
        cash_vec[i] <- cash_vec[i] + round(prev_pos_size * div_vec[i], 2)
      }
    }
    
    # --- Buy & Hold Logic ---
    if (incl_buynhold) {
      if (f_row) {
        bnh_ps_vec[i] <- round(init_eqty / open_vec[i], 2)
      } else {
        bnh_ps_vec[i] <- bnh_ps_vec[i - 1]
      }
      if (div_vec[i] > 0) {
        bnh_ps_vec[i] <- bnh_ps_vec[i] + round((div_vec[i] * bnh_ps_vec[i]) / open_vec[i], 2)
      }
    }
    
    pos_val_vec[i] <- round(pos_size_vec[i] * close_vec[i], 2)
    eqty_vec[i] <- cash_vec[i] + pos_val_vec[i]
    eqty_low_vec[i] <- round(cash_vec[i] + pos_size_vec[i] * low_vec[i], 2)
    eqty_high_vec[i] <- round(cash_vec[i] + pos_size_vec[i] * high_vec[i], 2)
    if (incl_buynhold) bnh_eqty_vec[i] <- round(bnh_ps_vec[i] * close_vec[i], 2)
  }
  
  tbl <- tbl |>
    mutate(
      pos_size = pos_size_vec,
      pos_val = pos_val_vec,
      cash = cash_vec,
      eqty = eqty_vec,
      eqty_l = eqty_low_vec,
      eqty_h = eqty_high_vec,
      drawdn = round(log(eqty / slide_max(eqty, before = Inf)), 4),
      pos_type = case_when(pos_size > 0 ~ "LONG", pos_size < 0 ~ "SHORT", TRUE ~ "FLAT"),
      trd_num = slide_sum(trd_entry, before = Inf)
    ) |>
    group_by(trd_num) |>
    mutate(
      is_closed = max(trd_exit),
      trd_length = row_number(),
      trd_base = round(first(open) * first(pos_size), 2),
      trd_drawdn = round(log(eqty / slide_max(eqty, before = Inf)), 4),
      trd_drawup = round(log(eqty / trd_base), 4),
      trd_ur_drawdn = round(log(eqty / slide_max(eqty_h, before = Inf)), 4),
      trd_ur_drawup = round(log(eqty_h / trd_base), 4)
    ) |>
    ungroup() |>
    mutate(
      trd_num = if_else(pos_size == 0, NA, trd_num),
      trd_length = if_else(pos_size == 0, NA, trd_length),
      trd_drawdn = if_else(pos_size == 0, NA, trd_drawdn),
      trd_drawup = if_else(pos_size == 0, NA, trd_drawup),
      trd_ur_drawdn = if_else(pos_size == 0, NA, trd_ur_drawdn),
      trd_ur_drawup = if_else(pos_size == 0, NA, trd_ur_drawup),
      trd_base = if_else(pos_size == 0, NA, trd_base),
      pct_chg = round(if_else(rn == 1, log(eqty / init_eqty), log(eqty / lag(eqty))), 4)
    )
  
  if (incl_buynhold) {
    tbl <- tbl |>
      mutate(
        bnh_pos_size = bnh_ps_vec,
        bnh_eqty = bnh_eqty_vec,
        bnh_pct_chg = round(if_else(rn == 1, log(bnh_eqty / init_eqty), log(bnh_eqty / lag(bnh_eqty))), 4),
        bnh_dd = round(log(bnh_eqty / slide_max(bnh_eqty, before = Inf)), 4)
      )
  }
  
  return(tbl)
}

bt_trds_func <- \(tbl) {
  tbl = tbl |>
    drop_na() |>
    group_by(trd_num) |>
    summarise(trd_st_dt = first(dt),
              trd_end_dt = last(dt),
              trd_type = first(pos_type),
              trd_rslt = case_when(
                last(is_closed) == 1 & last(eqty) - first(trd_base) > 0 ~ "Winner",
                last(is_closed) == 1 & last(eqty) - first(trd_base) <= 0 ~ "Loser",
                .default = "Open"),
              ttl_trd_len = max(trd_length),
              trd_pnl = last(eqty) - first(trd_base),
              trd_pnl_pct = round(log(last(eqty) / first(trd_base)), 4),
              max_trd_drawdn = min(trd_drawdn),
              max_trd_drawup = max(trd_drawup),
              max_ur_drawdn = min(trd_ur_drawdn),
              max_ur_drawup = max(trd_ur_drawup)) |>
    mutate(wl_seq = dense_rank(trd_rslt) |> consecutive_id()) |>
    group_by(wl_seq) |>
    mutate(consec_win_loss = if_else(trd_rslt == "Open", NA, seq_along(wl_seq)),
           .after = trd_rslt) |>
    ungroup() |>
    select(-wl_seq)
}

bt_winloss_func <- \(tbl) {
  tbl = tbl |>
    filter(trd_rslt != "Open") |>
    group_by(trd_rslt) |>
    summarise(trd_cnt = n(),
              gross_pnl = sum(trd_pnl),
              max_win_loss = if_else(
                first(trd_rslt) == "Winner",
                max(trd_pnl),
                min(trd_pnl)),
              max_consec_win_loss = max(consec_win_loss),
              avg_pnl = mean(trd_pnl, na.rm = TRUE),
              avg_length = mean(ttl_trd_len, na.rm = TRUE)
    ) 
}

bt_totals_func <- \(tbl, trds_tbl) {
  trds_tbl = trds_tbl |>
    filter(trd_rslt != "Open") |>
    summarise(summary = "Totals",
              trd_cnt = n(),
              pct_profitable = sum(ifelse(trd_rslt == "Winner", 1, 0)) / n(),
              net_profit = sum(trd_pnl),
              avg_pnl = mean(trd_pnl, na.rm = TRUE),
              avg_length = mean(ttl_trd_len, na.rm = TRUE),
              avg_win_loss_ratio = 
                mean(ifelse(trd_rslt == "Winner", trd_pnl, NA), na.rm = TRUE) /
                abs(mean(ifelse(trd_rslt == "Loser", trd_pnl, NA), na.rm = TRUE)),
              profit_factor = 
                sum(ifelse(trd_rslt == "Winner", 1, 0)) / 
                sum(ifelse(trd_rslt == "Loser", 1, 0)),
              rlzd_roa_pct = exp(sum(trd_pnl_pct)) - 1,
              ttl_roa_pct = exp(sum(trds_tbl$trd_pnl_pct)) - 1,
              max_trd_drawdn_pct = min(max_trd_drawdn),
              max_cum_drawdn_pct = min(tbl$drawdn))  
} 

bt_unrlzd_func <- \(tbl, init_eqty, incl_buynhold) {
  unrlzd_tbl = tbl |>
    summarise(
      unrlzd_net_profit = last(eqty) - init_eqty,
      unrlzd_roa_pct = exp(sum(pct_chg)) - 1
    )
  
  if (incl_buynhold) {
    bnh_unrlzd_tbl = tbl |>
      summarise(
        bnh_net_profit = last(bnh_eqty) - init_eqty,
        bnh_ttl_roa_pct = exp(sum(bnh_pct_chg)) - 1
      )
    
    unrlzd_tbl = unrlzd_tbl |>
      add_column(
        bnh_unrlzd_net_profit = bnh_unrlzd_tbl$bnh_net_profit,
        bnh_unrlzd_ttl_roa_pct = bnh_unrlzd_tbl$bnh_ttl_roa_pct
      )
  }
  
  return(unrlzd_tbl)
} 

bt_yrly_func <- \(tbl, init_eqty, incl_buynhold) {
  yrly_tbl = tbl |>
    mutate(yr = year(dt),
           prev_eqty = lag(eqty)) |>
    group_by(yr) |>
    summarise(st_eqty = if_else(is.na(first(prev_eqty)),
                                init_eqty,
                                first(prev_eqty)),
              end_eqty = last(eqty),
              max_yr_drawdn = min(drawdn))
  
  if (incl_buynhold) {
    bnh_yrly_tbl = tbl |>
      mutate(yr = year(dt),
             bnh_prev_eqty = lag(bnh_eqty)) |>
      group_by(yr) |>
      summarise(bnh_st_eqty = if_else(is.na(first(bnh_prev_eqty)),
                                      init_eqty,
                                      first(bnh_prev_eqty)),
                bnh_end_eqty = last(bnh_eqty),
                bnh_max_yr_dd = min(bnh_dd))
    
    yrly_tbl = yrly_tbl |>
      add_column(
        bnh_st_eqty = bnh_yrly_tbl$bnh_st_eqty,
        bnh_end_eqty = bnh_yrly_tbl$bnh_end_eqty,
        bnh_max_yr_dd = bnh_yrly_tbl$bnh_max_yr_dd
      )
  }
  
  return(yrly_tbl)
} 

calcs_strtgy_func <- \(tbl, yrly_tbl, trds_tbl) {
  time_in_mkt_val = nrow(tbl |> drop_na()) / nrow(tbl)
  
  cagr_val = round(
    cagr_func(
      dplyr::first(yrly_tbl$st_eqty),
      dplyr::last(yrly_tbl$end_eqty),
      nrow(yrly_tbl)),
    4)
  
  mar_val = round(
    mar_func(
      cagr_val,
      max_dd_func(yrly_tbl$max_yr_drawdn)),
    3)
  
  yrly_tbl_tail = dplyr::slice_tail(yrly_tbl, n = 3)
  calmar_val = round(
    mar_func(
      cagr_func(
        dplyr::first(yrly_tbl_tail$st_eqty),
        dplyr::last(yrly_tbl_tail$end_eqty),
        nrow(yrly_tbl_tail)),
      max_dd_func(yrly_tbl_tail$max_yr_drawdn)),
    3)
  
  sharp_val = round(sharp_func(trds_tbl$trd_pnl), 3)
  
  anlzd_sharp_val = round(anlzd_sharp_func(tbl$pct_chg, 12), 3)
  
  return(list(
    time_in_mkt = time_in_mkt_val,
    cagr = cagr_val,
    mar = mar_val,
    calmar = calmar_val,
    sharp = sharp_val,
    anlzd_sharp = anlzd_sharp_val
  ))
}

calcs_bnh_func <- \(tbl, yrly_tbl) {
  bnh_cagr_val = round(
    cagr_func(
      dplyr::first(yrly_tbl$bnh_st_eqty),
      dplyr::last(yrly_tbl$bnh_end_eqty),
      nrow(yrly_tbl)),
    4)
  
  bnh_mar_val = round(
    mar_func(
      bnh_cagr_val,
      max_dd_func(yrly_tbl$bnh_max_yr_dd)),
    3)
  
  yrly_tbl_tail = dplyr::slice_tail(yrly_tbl, n = 3)
  bnh_calmar_val = round(
    mar_func(
      cagr_func(
        dplyr::first(yrly_tbl_tail$bnh_st_eqty),
        dplyr::last(yrly_tbl_tail$bnh_end_eqty),
        nrow(yrly_tbl_tail)),
      max_dd_func(yrly_tbl_tail$bnh_max_yr_dd)),
    3)
  
  bnh_anlzd_sharp_val <- round(anlzd_sharp_func(tbl$bnh_pct_chg, 12), 3)
  
  return(list(
    cagr = bnh_cagr_val,
    mar = bnh_mar_val,
    calmar = bnh_calmar_val,
    anlzd_sharp = bnh_anlzd_sharp_val
  ))
}

# bt visualization

bt_viz_func <- \(metrics, init_eqty = 10000, incl_buynhold = FALSE) {
  wnr = metrics$data_winloss |> dplyr::filter(trd_rslt == "Winner")
  lsr = metrics$data_winloss |> dplyr::filter(trd_rslt == "Loser")
  ttl = metrics$data_totals
  unr = metrics$data_unrlzd
  st_dt = dplyr::first(metrics$data_series$dt)
  end_dt = dplyr::last(metrics$data_series$dt)
  
  bt_fmt_tbl = tribble(
    ~RowName, ~Group, ~Totals, ~Winners, ~Losers,
    "Trade Count", "Trade Stats", ttl$trd_cnt, wnr$trd_cnt, lsr$trd_cnt,
    "Profit & Loss", "Trade Stats", ttl$net_profit, wnr$gross_pnl, lsr$gross_pnl,
    "Avg P&L", "Trade Stats", ttl$avg_pnl, wnr$avg_pnl, lsr$avg_pnl,
    "Avg Bars", "Trade Stats", ttl$avg_length, wnr$avg_length, lsr$avg_length,
    "Max Consecutive", "Trade Stats", NA, wnr$max_consec_win_loss, lsr$max_consec_win_loss,
    "Max Amount", "Trade Stats", NA, wnr$max_win_loss, lsr$max_win_loss,
    "Profitable", "Summary Stats", ttl$pct_profitable, NA, NA,
    "Closed ROA", "Summary Stats", ttl$ttl_roa_pct, NA, NA,
    "Cumul. ROA", "Summary Stats", unr$unrlzd_roa_pct, NA, NA,
    "Max Trade DD", "Summary Stats", ttl$max_trd_drawdn_pct, NA, NA,
    "Max Cumul. DD", "Summary Stats", ttl$max_cum_drawdn_pct, NA, NA,
    "Profit Factor", "Summary Stats", ttl$profit_factor, NA, NA,
    "CAGR", "Summary Stats", metrics$calcs_strtgy$cagr, NA, NA,
    "MAR", "Summary Stats", metrics$calcs_strtgy$mar, NA, NA,
    "CALMAR", "Summary Stats", metrics$calcs_strtgy$calmar, NA, NA,
    "Sharpe", "Summary Stats", metrics$calcs_strtgy$sharp, NA, NA,
    "Ann. Sharpe", "Summary Stats", metrics$calcs_strtgy$anlzd_sharp, NA, NA,
    "Total Months", "Misc. Stats", nrow(metrics$data_series), NA, NA,
    "Trade Months", "Misc. Stats", nrow((metrics$data_series) |> drop_na()), NA, NA,
    "Time in Market", "Misc. Stats", metrics$calcs_strtgy$time_in_mkt, NA, NA,
    "Initial Equity", "Misc. Stats", init_eqty, NA, NA,
    "End Equity", "Misc. Stats", last(metrics$data_series$eqty), NA, NA,
  )
  
  if (incl_buynhold) {
    bnh_list = list(
      "Trade Count" = NA,
      "Profit & Loss" = NA,
      "Avg P&L" = NA,
      "Avg Bars" = NA,
      "Max Consecutive" = NA,
      "Max Amount" = NA,
      "Profitable" = NA,
      "Closed ROA" = NA,
      "Cumul. ROA" = unr$bnh_unrlzd_ttl_roa_pct,
      "Max Trade DD" = NA,
      "Max Cumul. DD" = min(metrics$data_series$bnh_dd),
      "Profit Factor" = NA,
      "CAGR" = metrics$calcs_bnh$cagr,
      "MAR" = metrics$calcs_bnh$mar,
      "CALMAR" = metrics$calcs_bnh$calmar,
      "Sharpe" = NA,
      "Ann. Sharpe" = metrics$calcs_bnh$anlzd_sharp,
      "Total Months" = NA,
      "Trade Months" = NA,
      "Time in Market" = NA,
      "Initial Equity" = init_eqty,
      "End Equity" = last(metrics$data_series$bnh_eqty)
    )
    
    bt_fmt_tbl = bt_fmt_tbl |>
      add_column(BNH = unlist(bnh_list, use.names = FALSE))
  }
  
  bt_gt = bt_fmt_tbl |>
    gt(
      rowname_col = "RowName",
      groupname_col = "Group"
    ) |>
    tab_header(title = "Backtest Results (SPY)",
               subtitle = glue::glue("{st_dt} to {end_dt} (Monthly Aggregation)")) |>
    opt_align_table_header(align = "left") |>
    opt_vertical_padding(scale = 0.5) |>
    tab_footnote(footnote = "*All Dividends are Reinvested") |>
    cols_width(
      RowName ~ px(150),
      everything() ~ px(150)
    ) |>
    tab_style(
      style = list(
        cell_fill(color = "#ececec"),
        cell_text(size = "larger")
      ),
      locations = cells_row_groups(groups = everything())
    ) |>
    fmt_percent(
      rows = RowName %in% c("Profitable", "Closed ROA", "Cumul. ROA", "CAGR", "Time in Market", "Max Trade DD", "Max Cumul. DD"),
      columns = names(bt_fmt_tbl)[-(1:2)],
      accounting = TRUE
    ) |>
    fmt_number(
      rows = RowName %in% c("Trade Count", "Max Consecutive", "Total Months", "Trade Months"),
      columns = names(bt_fmt_tbl)[-(1:2)],
      decimals = 0
    ) |>
    fmt_number(
      rows = RowName %in% c("Avg Bars", "Profit Factor", "MAR", "CALMAR", "Sharpe", "Ann. Sharpe"),
      columns = names(bt_fmt_tbl)[-(1:2)],
      decimals = 3
    ) |>
    fmt_currency(
      rows = RowName %in% c("Profit & Loss", "Avg P&L", "Max Amount", "Initial Equity", "End Equity"),
      columns = names(bt_fmt_tbl)[-(1:2)],
      accounting = TRUE
    ) |>
    sub_values(fn = \(x) is.na(x), replacement = "")
  
  if (incl_buynhold) {
    bt_gt = bt_gt |>
      cols_label(BNH ~ "Buy & Hold") |>
      tab_spanner(label = "Strategy", columns = c("Totals", "Winners", "Losers"))
  }
  
  return(bt_gt)
}

# Optimization Functions

get_trades_base <- \(tbl, fun, cnt)
tbl |>
  filter(trd_rslt != "Open") |>
  fun(trd_pnl_pct, n = cnt) |>
  arrange(trd_num)

get_n_worst_trades <- \(tbl, cnt)
get_trades_base(tbl, slice_min, cnt)
get_n_best_trades <- \(tbl, cnt)
get_trades_base(tbl, slice_max, cnt)

data_ex_n_trds_func <- \(data_tbl, trd_tbl, trd_type_func, n_trds) {
  rm_trds = as.numeric((trd_tbl |> trd_type_func(n_trds))$trd_num)
  
  criteria_ex_trds = data_tbl |>
    select(dt, trd_entry, trd_exit, trd_num) |>
    mutate(trd_num_x = ifelse(trd_exit == 1 & is.na(trd_num),
                              lag(trd_num),
                              trd_num),
           trd_entry = ifelse(trd_num %in% rm_trds, 0, trd_entry),
           trd_exit = ifelse(trd_num_x %in% rm_trds, 0, trd_exit)) |>
    select(-trd_num, -trd_num_x)
  
  data_ex_trds = data_tbl |>
    select(-c(trd_entry:tidyselect::last_col())) |>
    left_join(criteria_ex_trds, by = join_by(dt))
  
  return(data_ex_trds)
}

ex_n_trd_calcs_func <- \(strtgy_ex_trds, 
                         init_eqty = 10000, 
                         incl_dividends = FALSE) {
  ex_data_tbl = strtgy_ex_trds |>
    bt_sim_func(
      init_eqty = init_eqty,
      incl_dividends = incl_dividends,
      incl_buynhold = FALSE)
  
  ex_trd_tbl = ex_data_tbl |> bt_trds_func()
  ex_yrly_tbl = ex_data_tbl |> bt_yrly_func(init_eqty, FALSE)
  
  ex_calcs_strtgy = calcs_strtgy_func(
    tbl = ex_data_tbl,
    yrly_tbl = ex_yrly_tbl,
    trds_tbl = ex_trd_tbl)
  
  return(ex_calcs_strtgy)
}

calcs_to_row_func <- \(calc_list, trd_type, n_trds) {
  utils::modifyList(
    list(rm_trds = trd_type, n = n_trds), 
    calc_list) |>
    as_tibble_row()
}

rm_best_worst_trds_func <- \(data_tbl, 
                             trd_tbl, 
                             orig_calcs, 
                             n_trds = 3, 
                             init_eqty = 10000,
                             incl_dividends = FALSE) {
  all_trds_tbl = orig_calcs |> calcs_to_row_func("none", NA)
  
  ex_best_tbl = data_ex_n_trds_func(
    bt_metrics$data_series,
    bt_metrics$data_trd,
    get_n_best_trades,
    n_trds) |>
    ex_n_trd_calcs_func(init_eqty, incl_dividends) |>
    calcs_to_row_func("best", n_trds)
  
  ex_worst_tbl = bt_ex_worst3_tst <-data_ex_n_trds_func(
    bt_metrics$data_series,
    bt_metrics$data_trd,
    get_n_worst_trades,
    n_trds) |>
    ex_n_trd_calcs_func(init_eqty, incl_dividends) |>
    calcs_to_row_func("worst", n_trds)
  
  comb_tbl = rbind(ex_worst_tbl, all_trds_tbl, ex_best_tbl)
  
  return(comb_tbl)
}

rm_trds_gt_func <- \(tbl) {
  tbl |>
    dplyr::mutate(scenario = if_else(
      rm_trds == "none", 
      "All Trades", 
      glue("{n} {str_to_title(rm_trds)} Trade{ifelse(n != 1, 's', '')} Removed")
    ),
    .before = rm_trds) |>
    gt(
      rowname_col = "scenario"
    ) |>
    cols_hide(c(rm_trds, n)) |>
    cols_label(
      rm_trds = "Scenario",
      time_in_mkt = "Time in Market",
      cagr = "CAGR",
      mar = "MAR",
      calmar = "CALMAR",
      sharp = "Sharpe",
      anlzd_sharp = "Ann. Sharpe") |>
    tab_header(title = "Edge Case Comparisons") |>
    opt_align_table_header(align = "left") |>
    opt_vertical_padding(scale = 0.5) |>
    tab_footnote(footnote = "*All Dividends are Reinvested") |>
    cols_width(
      scenario ~ px(250),
      everything() ~ px(125)
    ) |>
    fmt_percent(
      columns = c(time_in_mkt, cagr),
      accounting = TRUE
    ) |>
    fmt_number(
      columns = c(mar, calmar, sharp, anlzd_sharp),
      decimals = 3
    )
}

## Backtest Processor

# source("src/backtest_engine2.R")

bt_processor <- \(data_tbl,
                  entry_func,
                  exit_func,
                  strat_cfg = list(
                    trade_mode = POSITION_SHORT,
                    entry_timing = TradeTiming$OPEN,
                    exit_timing = TradeTiming$OPEN,
                    init_eqty = 10000,
                    incl_buynhold = TRUE,
                    incl_dividends = TRUE
                  ),
                  ...) {
  strat_cfg$entry_timing <- match.arg(strat_cfg$entry_timing, unlist(TradeTiming))
  strat_cfg$exit_timing  <- match.arg(strat_cfg$exit_timing,  unlist(TradeTiming))
  
  data_val = data_tbl |>
    add_trades_func(entry_func, exit_func) |>
    bt_sim_func(
      strat_cfg = strat_cfg
    )
  
  trd_val = data_val |> bt_trds_func()
  
  winloss_val = trd_val |> bt_winloss_func()
  
  totals_val = bt_totals_func(data_val, trd_val)
  
  unrlzd_val = data_val |> bt_unrlzd_func(init_eqty, incl_buynhold)
  
  yrly_val = data_val |> bt_yrly_func(init_eqty, incl_buynhold)
  
  calcs_strtgy_val = calcs_strtgy_func(
    tbl = data_val,
    yrly_tbl = yrly_val,
    trds_tbl = trd_val)
  
  if (incl_buynhold) {
    calcs_bnh_val = calcs_bnh_func(
      tbl = data_val,
      yrly_tbl = yrly_val)
  }
  
  metrics_val = list(
    data_series = data_val,
    data_trd = trd_val,
    data_yrly = yrly_val,
    data_winloss = winloss_val,
    data_totals = totals_val,
    data_unrlzd = unrlzd_val,
    calcs_strtgy = calcs_strtgy_val
  )
  
  if (incl_buynhold) {
    metrics_val <- append(metrics_val, list(
      calcs_bnh = calcs_bnh_val
    ))
  }
  
  rslts_viz_func = \() bt_viz_func(metrics_val, init_eqty, incl_buynhold)
  
  best_worst_viz_func = \(n_trds = 1, as_tbl = FALSE) {
    rm_best_worst_trds = rm_best_worst_trds_func(
      data_tbl = data_val,
      trd_tbl = trd_val,
      orig_calcs = calcs_strtgy_val,
      n_trds = n_trds,
      init_eqty = init_eqty,
      incl_dividends = incl_dividends) 
    
    if (as_tbl) {
      return(rm_best_worst_trds)
    } else {
      rm_best_worst_trds_gt = rm_best_worst_trds |>
        rm_trds_gt_func()
      
      return(rm_best_worst_trds_gt)
    }
  }
  
  metrics_val = append(metrics_val, list(
    rslts_viz = rslts_viz_func,
    best_worst_viz = best_worst_viz_func
  ))
  
  return(metrics_val)
}

## Bactest Setup/Run

### Initial Setup

sourceCpp("C:/Users/bawil/Documents/RScripts/BacktestScripts/ultimate_smoother.cpp")

init_eqty <- 10000
incl_dividends <- TRUE
incl_buynhold <- TRUE

strat_cfg <- list(
  entry_timing = TradeTiming$CLOSE,
  exit_timing = TradeTiming$CLOSE,
  init_eqty = 10000,
  incl_buynhold = TRUE,
  incl_dividends = TRUE
)

strtgy_entry <- \(.) {
  ifelse(.$rn > 1 & (.$oc2 > .$UltimateSmoother & lag(.$oc2) <= lag(.$UltimateSmoother)), 
         1, 0)
}
strtgy_exit <- \(.) {
  ifelse(.$rn > 1 & (.$oc2 < .$UltimateSmoother & lag(.$oc2) >= lag(.$UltimateSmoother)), 
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
                           strtgy_entry,
                           strtgy_exit,
                           strat_cfg)

bt_metrics$rslts_viz()

## Data Cleanup

dbDisconnect(con, shutdown=TRUE)  