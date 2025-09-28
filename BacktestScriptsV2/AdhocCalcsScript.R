#' Calculate and print the total dividends paid during short positions.
#'
#' @param results_df The data frame of backtest results.
#' @param daily_dividends_df The data frame of raw, daily dividend payments.
#' @param config_list The strategy configuration list.
calculate_short_dividend_cost <- function(results_df, daily_dividends_df, config_list) {
  
  allowed_modes <- c("SHORT", "LONG_SHORT")
  if (is.null(config_list$trade_mode) || !(config_list$trade_mode %in% allowed_modes)) {
    return(invisible(NULL))
  }
  
  # Find bars where a short position was held
  short_periods <- results_df %>%
    mutate(effective_shares = lag(share_quantity, default = 0)) %>%
    filter(position_state == "short" | (position_state == "flat" & lag(position_state) == "short")) %>%
    select(period_start_dt, period_end_dt, effective_shares)

  # Use a non-equi join to find all dividends that fall within those periods
  dividends_while_short <- daily_dividends_df %>%
    inner_join(short_periods, by = join_by(ex_date >= period_start_dt, ex_date <= period_end_dt))

  total_dividends_paid <- dividends_while_short %>%
    summarise(total = sum(effective_shares * dividend_amount, na.rm = TRUE)) %>% 
    pull(total)
  
  cat(paste("
--- Ad-hoc Analysis ---"))
  cat(paste("
Total Dividends Paid While Short:", round(total_dividends_paid, 2), "
"))
  
  final_equity <- tail(results_df$equity, 1)
  initial_equity <- config_list$initial_equity
  realized_pnl <- tail(results_df$realized_pnl, 1)
  
  cat(paste("Final Equity:", round(final_equity, 2), "
"))
  cat(paste("Calculated Equity (Initial + PnL - Dividends):", round(initial_equity + realized_pnl - total_dividends_paid, 2), "

"))
}