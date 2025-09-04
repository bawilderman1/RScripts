#' Calculate and print the total dividends paid during short positions.
#'
#' @param results_df The data frame of backtest results.
#' @param dividends_df The data frame of dividend payments.
#' @param config_list The strategy configuration list.
calculate_short_dividend_cost <- function(results_df, dividends_df, config_list) {
  
  # The mode must be one that can contain short trades
  allowed_modes <- c("SHORT", "LONG_SHORT")
  if (is.null(config_list$trade_mode) || !(config_list$trade_mode %in% allowed_modes)) {
    return(invisible(NULL))
  }
  
    dividends_while_short <- results_df %>%
    filter(position_state == "short") %>%
    inner_join(dividends_df, by = c("dt" = "ex_date"))
  
  # Correctly calculate the total nominal dividend by multiplying shares by the per-share amount
  total_dividends_paid <- dividends_while_short %>%
    summarise(total = sum(share_quantity * dividend_amount, na.rm = TRUE)) %>% 
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