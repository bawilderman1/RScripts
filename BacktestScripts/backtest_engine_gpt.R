# Constants for position states
POSITION_NONE  <- "none"
POSITION_LONG  <- "long"
POSITION_SHORT <- "short"

#' Process a single trade bar for long positions
#'
#' @param i Integer index of the current bar
#' @param cash_prev Previous cash value
#' @param shares_prev Previous number of shares held
#' @param pos_prev Previous position state
#' @param price_vec Numeric vector of open prices
#' @param entry_vec Logical vector indicating entry signals
#' @param exit_vec Logical vector indicating exit signals
#' @param div_vec Numeric vector of dividend amounts
#' @param strat_cfg List containing strategy configuration parameters
#' @return A list with updated cash, shares, and position state
process_long_trade_bar <- function(i, cash_prev, shares_prev, pos_prev,
                                   price_vec, entry_vec, exit_vec, div_vec, strat_cfg) {
  price <- price_vec[i]
  div   <- div_vec[i]
  
  is_entry <- !is.na(entry_vec[i]) && entry_vec[i]
  is_exit  <- !is.na(exit_vec[i]) && exit_vec[i]

  if (pos_prev == POSITION_NONE && is_entry) {
    shares <- floor(cash_prev / price)
    cash <- cash_prev - shares * price
    position_state <- POSITION_LONG
  } else if (pos_prev == POSITION_LONG && is_exit) {
    cash <- cash_prev + shares_prev * price + shares_prev * div
    shares <- 0
    position_state <- POSITION_NONE
  } else {
    cash <- cash_prev + shares_prev * div
    shares <- shares_prev
    position_state <- pos_prev
  }
  
  return(list(cash = cash, shares = shares, position_state = position_state))  # RCPP_OK
}

#' Process a single trade bar for short positions
#'
#' @param i Integer index of the current bar
#' @param cash_prev Previous cash value
#' @param shares_prev Previous number of shares shorted (negative)
#' @param pos_prev Previous position state
#' @param price_vec Numeric vector of open prices
#' @param entry_vec Logical vector indicating entry signals
#' @param exit_vec Logical vector indicating exit signals
#' @param div_vec Numeric vector of dividend amounts
#' @param strat_cfg List containing strategy configuration parameters
#' @return A list with updated cash, shares, and position state
process_short_trade_bar <- function(i, cash_prev, shares_prev, pos_prev,
                                    price_vec, entry_vec, exit_vec, div_vec, strat_cfg) {
  price <- price_vec[i]
  div   <- div_vec[i]
  
  if (!is.na(entry_vec[i]) && entry_vec[i] && pos_prev == POSITION_NONE) {
    shares <- -floor(cash_prev / price)
    cash <- cash_prev - shares * price  # cash from short sale
    position_state <- POSITION_SHORT
  } else if (!is.na(exit_vec[i]) && exit_vec[i] && pos_prev == POSITION_SHORT) {
    cash <- cash_prev + shares_prev * price - abs(shares_prev) * div
    shares <- 0
    position_state <- POSITION_NONE
  } else {
    cash <- cash_prev - abs(shares_prev) * div
    shares <- shares_prev
    position_state <- pos_prev
  }
  
  return(list(cash = cash, shares = shares, position_state = position_state))  # RCPP_OK
}

#' add_trades_func
#'
#' Dispatches and attaches entry/exit signals to the data based on strategy mode.
#'
#' @param data A tibble or data.frame containing price and indicator columns.
#' @param entry_func Function that generates entry signals. Accepts (data, strat_cfg).
#' @param exit_func Function that generates exit signals. Accepts (data, strat_cfg).
#' @param strat_cfg A named list containing strategy configuration. Must include `trade_mode` ("long", "short", or "long_short").
#'
#' @return The input `data` with added signal columns for use in simulation.
add_trades_func2 <- function(data, entry_func, exit_func, strat_cfg) {
  trade_mode <- match.arg(strat_cfg$trade_mode, c("long", "short", "long_short"))
  
  if (trade_mode == "long") {
    data$entry <- entry_func(data, strat_cfg)
    data$exit  <- exit_func(data, strat_cfg)
    
  } else if (trade_mode == "short") {
    data$entry <- entry_func(data, strat_cfg)
    data$exit  <- exit_func(data, strat_cfg)
    
  } else if (trade_mode == "long_short") {
    if (is.null(strat_cfg$entry_func_long) || is.null(strat_cfg$exit_func_long) ||
        is.null(strat_cfg$entry_func_short) || is.null(strat_cfg$exit_func_short)) {
      stop("In long_short mode, strat_cfg must contain entry_func_long, exit_func_long, entry_func_short, and exit_func_short.")
    }
    
    data$entry_long  <- strat_cfg$entry_func_long(data, strat_cfg)
    data$exit_long   <- strat_cfg$exit_func_long(data, strat_cfg)
    data$entry_short <- strat_cfg$entry_func_short(data, strat_cfg)
    data$exit_short  <- strat_cfg$exit_func_short(data, strat_cfg)
  }
  
  return(data)
}

#' This is the main entry point for executing a trading simulation. It dispatches to
#' the appropriate engine (long-only, short-only, or long-short) and returns enhanced
#' performance metrics.
#'
#' @param data A tibble with columns: Open, entry, exit, and (optionally) Dividend.
#' @param strat_cfg A list with strategy configuration options, e.g.:
#' strat_cfg <- list(
#'   trade_mode = "long",         # or "short", "long_short"
#'   init_eqty = 100000,          # starting equity
#'   incl_buynhold = TRUE         # whether to include buy-and-hold benchmark
#' )
#'
#' @return A tibble containing per-bar simulation output with equity, drawdown,
#' position state, and trade-level metrics.
#'
#' @examples
#' data <- tibble::tibble(
#'   Open = c(100, 102, 104, 103, 105),
#'   entry = c(FALSE, TRUE, FALSE, FALSE, FALSE),
#'   exit  = c(FALSE, FALSE, FALSE, TRUE, FALSE),
#'   Dividend = c(0, 0, 0, 0, 0)
#' )
#'
#' strat_cfg <- list(trade_mode = "long", init_eqty = 100000, incl_buynhold = TRUE)
#' result <- bt_sim_func(data, strat_cfg)
#' print(result)
bt_sim_func2 <- function(data, strat_cfg) {
  trade_mode <- match.arg(strat_cfg$trade_mode, c("long", "short", "long_short"))
  
  out <- switch(
    trade_mode,
    long       = run_long_sim(data, strat_cfg),
    short      = run_short_sim(data, strat_cfg),
    long_short = run_long_short_sim(data, strat_cfg)
  )
  
  out <- enhance_bt_output(out, strat_cfg)
  
  return(out)
}

#' Post-processes raw simulation output
#'
#' Adds drawdown metrics, trade counts, unrealized metrics, and buy-n-hold comparisons
#'
#' @param tbl A tibble returned from one of the run_*_sim functions
#' @param strat_cfg Strategy config list (with init_eqty and incl_buynhold)
#' @return Enhanced tibble with additional columns for performance analysis
enhance_bt_output <- function(tbl, strat_cfg) {
  with(tbl, {
    trd_entry <- (position_state != dplyr::lag(position_state, default = POSITION_NONE)) & position_state != POSITION_NONE
    trd_exit  <- (position_state != dplyr::lead(position_state, default = POSITION_NONE)) & position_state != POSITION_NONE
  }) -> signals
  
  tbl <- bind_cols(tbl, signals)
  
  tbl <- tbl %>%
    mutate(
      pos_size = shares,
      pos_val = shares * Open,
      drawdn = round(log(equity / slider::slide_index_dbl(equity, seq_along(equity), max, .before = Inf)), 4),
      pos_type = case_when(pos_size > 0 ~ "LONG", pos_size < 0 ~ "SHORT", TRUE ~ "FLAT"),
      trd_num = slider::slide_index_int(trd_entry, seq_along(trd_entry), sum, .before = Inf)
    ) %>%
    group_by(trd_num) %>%
    mutate(
      is_closed = max(trd_exit),
      trd_length = row_number(),
      trd_base = round(first(Open) * first(pos_size), 2),
      trd_drawdn = round(log(equity / slider::slide_index_dbl(equity, seq_along(equity), max, .before = Inf)), 4),
      trd_drawup = round(log(equity / trd_base), 4),
      trd_ur_drawdn = round(log(equity / slider::slide_index_dbl(eqty_h, seq_along(eqty_h), max, .before = Inf)), 4),
      trd_ur_drawup = round(log(eqty_h / trd_base), 4)
    ) %>%
    ungroup() %>%
    mutate(
      trd_num = if_else(pos_size == 0, NA_integer_, trd_num),
      trd_length = if_else(pos_size == 0, NA_integer_, trd_length),
      trd_drawdn = if_else(pos_size == 0, NA_real_, trd_drawdn),
      trd_drawup = if_else(pos_size == 0, NA_real_, trd_drawup),
      trd_ur_drawdn = if_else(pos_size == 0, NA_real_, trd_ur_drawdn),
      trd_ur_drawup = if_else(pos_size == 0, NA_real_, trd_ur_drawup),
      trd_base = if_else(pos_size == 0, NA_real_, trd_base),
      pct_chg = round(if_else(row_number() == 1, log(equity / strat_cfg$init_eqty), log(equity / lag(equity))), 4)
    )
  
  if (!is.null(strat_cfg$incl_buynhold) && strat_cfg$incl_buynhold) {
    tbl <- tbl %>%
      mutate(
        bnh_pos_size = rep(1, n()),
        bnh_eqty = strat_cfg$init_eqty * Open / first(Open),
        bnh_pct_chg = round(if_else(row_number() == 1, log(bnh_eqty / strat_cfg$init_eqty), log(bnh_eqty / lag(bnh_eqty))), 4),
        bnh_dd = round(log(bnh_eqty / slider::slide_index_dbl(bnh_eqty, seq_along(bnh_eqty), max, .before = Inf)), 4)
      )
  }
  
  return(tbl)
}

#' Main loop for executing a long-only trading simulation
#'
#' Iterates over the time series to simulate long-only trading based on entry and exit signals.
#'
#' @param data A tibble with Open, entry, exit, and Dividend columns
#' @param strat_cfg A list with strategy configuration including init_eqty
#' @return A tibble of simulation results per time step
run_long_sim <- function(data, strat_cfg) {
  n <- nrow(data)
  init_eqty <- strat_cfg$init_eqty
  
  cash_vec   <- numeric(n)
  shares_vec <- integer(n)
  eqty_vec   <- numeric(n)
  eqty_h_vec <- numeric(n)
  eqty_l_vec <- numeric(n)
  pos_state_vec <- character(n)
  
  cash <- init_eqty
  shares <- 0
  pos_state <- POSITION_NONE
  
  for (i in seq_len(n)) {
    result <- process_long_trade_bar(
      i, cash, shares, pos_state,
      price_vec = data$Open,
      entry_vec = data$entry,
      exit_vec = data$exit,
      div_vec = data$Dividend,
      strat_cfg = strat_cfg
    )
    
    cash <- result$cash
    shares <- result$shares
    pos_state <- result$position_state
    
    equity <- cash + shares * data$Open[i]
    
    cash_vec[i] <- cash
    shares_vec[i] <- shares
    eqty_vec[i] <- equity
    eqty_h_vec[i] <- if (i == 1) equity else max(equity, eqty_h_vec[i-1])
    eqty_l_vec[i] <- if (i == 1) equity else min(equity, eqty_l_vec[i-1])
    pos_state_vec[i] <- pos_state
  }
  
  return(tibble::tibble(
    rn = seq_len(n),
    Open = data$Open,
    entry = data$entry,
    exit = data$exit,
    Dividend = data$Dividend,
    shares = shares_vec,
    cash = cash_vec,
    equity = eqty_vec,
    eqty_h = eqty_h_vec,
    eqty_l = eqty_l_vec,
    position_state = pos_state_vec
  ))
}

#' Simulate a short-only strategy
#'
#' @param data A tibble with Open, entry, exit, and Dividend columns
#' @param strat_cfg A list with strategy configuration including init_eqty
#' @return A tibble of simulation results per time step
run_short_sim <- function(data, strat_cfg) {
  n <- nrow(data)
  init_eqty <- strat_cfg$init_eqty
  
  cash_vec   <- numeric(n)
  shares_vec <- integer(n)
  eqty_vec   <- numeric(n)
  eqty_h_vec <- numeric(n)
  eqty_l_vec <- numeric(n)
  pos_state_vec <- character(n)
  
  cash <- init_eqty
  shares <- 0
  pos_state <- POSITION_NONE
  
  for (i in seq_len(n)) {
    result <- process_short_trade_bar(
      i, cash, shares, pos_state,
      price_vec = data$Open,
      entry_vec = data$entry,
      exit_vec = data$exit,
      div_vec = data$Dividend,
      strat_cfg = strat_cfg
    )
    
    cash <- result$cash
    shares <- result$shares
    pos_state <- result$position_state
    
    equity <- cash + shares * data$Open[i]
    
    cash_vec[i] <- cash
    shares_vec[i] <- shares
    eqty_vec[i] <- equity
    eqty_h_vec[i] <- if (i == 1) equity else max(equity, eqty_h_vec[i-1])
    eqty_l_vec[i] <- if (i == 1) equity else min(equity, eqty_l_vec[i-1])
    pos_state_vec[i] <- pos_state
  }
  
  return(tibble::tibble(
    rn = seq_len(n),
    Open = data$Open,
    entry = data$entry,
    exit = data$exit,
    Dividend = data$Dividend,
    shares = shares_vec,
    cash = cash_vec,
    equity = eqty_vec,
    eqty_h = eqty_h_vec,
    eqty_l = eqty_l_vec,
    position_state = pos_state_vec
  ))
}

#' Simulate a long-short switching strategy
#'
#' @param data A tibble with Open, entry, exit, and Dividend columns
#' @param strat_cfg A list with strategy configuration including init_eqty
#' @return A tibble of simulation results per time step
run_long_short_sim <- function(data, strat_cfg) {
  n <- nrow(data)
  init_eqty <- strat_cfg$init_eqty
  incl_dividends <- strat_cfg$incl_dividends
  
  cash_vec   <- numeric(n)
  shares_vec <- integer(n)
  eqty_vec   <- numeric(n)
  eqty_h_vec <- numeric(n)
  eqty_l_vec <- numeric(n)
  pos_state_vec <- character(n)
  
  div_vec <- if (incl_dividends && "dividend" %in% names(data)) data$dividend else rep(0, n)
  
  cash <- init_eqty
  shares <- 0
  pos_state <- POSITION_NONE
  
  for (i in seq_len(n)) {
    if (pos_state == POSITION_LONG || pos_state == POSITION_NONE) {
      if ((pos_state == POSITION_NONE && !is.na(data$entry[i]) && data$entry[i]) || 
          (pos_state == POSITION_LONG && !is.na(data$exit[i]) && data$exit[i])) {
        result <- process_long_trade_bar(
          i, cash, shares, pos_state,
          price_vec = data$open,
          entry_vec = data$entry,
          exit_vec = data$exit,
          div_vec = div_vec,
          strat_cfg = strat_cfg
        )
      } else {
        result <- list(cash = cash, shares = shares, position_state = pos_state)
      }
    } else {
      if ((pos_state == POSITION_NONE && !is.na(data$entry[i]) && data$entry[i]) || 
          (pos_state == POSITION_SHORT && !is.na(data$exit[i]) && data$exit[i])) {
        result <- process_short_trade_bar(
          i, cash, shares, pos_state,
          price_vec = data$open,
          entry_vec = data$entry,
          exit_vec = data$exit,
          div_vec = div_vec,
          strat_cfg = strat_cfg
        )
      } else {
        result <- list(cash = cash, shares = shares, position_state = pos_state)
      }
    }
    
    cash <- result$cash
    shares <- result$shares
    pos_state <- result$position_state
    
    equity <- cash + shares * data$open[i]
    
    cash_vec[i] <- cash
    shares_vec[i] <- shares
    eqty_vec[i] <- equity
    eqty_h_vec[i] <- if (i == 1) equity else max(equity, eqty_h_vec[i-1])
    eqty_l_vec[i] <- if (i == 1) equity else min(equity, eqty_l_vec[i-1])
    pos_state_vec[i] <- pos_state
  }
  
  return(tibble::tibble(
    rn = seq_len(n),
    open = data$open,
    entry = data$entry,
    exit = data$exit,
    dividend = div_vec,
    shares = shares_vec,
    cash = cash_vec,
    equity = eqty_vec,
    eqty_h = eqty_h_vec,
    eqty_l = eqty_l_vec,
    position_state = pos_state_vec
  ))
}
