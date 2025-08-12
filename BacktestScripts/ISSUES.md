## Short Trade Entry Logic Issue

**TLDR Summary:**
Short trade entries are not being executed in `bt_sim_func`, resulting in zero trades for short-only strategies.

**Issue Description:**
When running a backtest with `trade_mode = TradeDirection$SHORT`, the `bt_sim_func` completes without errors, but the resulting trade metrics show 0 trades executed. This indicates that the conditions for entering a short trade are never met or are immediately exited. The equity curve also reflects a continuous decline, suggesting no active short positions are being managed.

**Tried Solutions:**
1.  **Corrected cash calculation for short entries**: Initially, there was an error in how cash was updated upon entering a short position. This was corrected from `cash_vec[i] <- round(ifelse(f_row, init_eqty, cash_vec[i-1]) - (pos_size_vec[i] * exec_price), 2)` to `cash_vec[i] <- round(ifelse(f_row, init_eqty, cash_vec[i-1]) + abs(pos_size * exec_price), 2)`. This fixed a logical error but did not resolve the 0 trades issue.
2.  **Refined trade entry conditions**: The `if` conditions for `entry_long` and `entry_short` were updated to explicitly check `trade_mode` and `TradeDirection` values to ensure correct branching for `LONG`, `SHORT`, and `LONG_SHORT` modes. This was to ensure that the correct entry signals were being evaluated based on the `trade_mode`.

**Relevant Code Snippets from `bt_sim_func`:**

```R
# --- ENTRY ---
if (position_state == PositionState$NONE) {
  if ((trade_mode == TradeDirection$LONG || trade_mode == TradeDirection$LONG_SHORT) && entry_long) {
    exec_price <- switch(entry_timing,
                         open = open_vec[i],
                         close = close_vec[i],
                         next_open = if (i < tbl_rlen) open_vec[i + 1] else NA_real_
    )

    if (!is.na(exec_price)) {
      pos_size_vec[i] <- round(ifelse(f_row, init_eqty, cash_vec[i - 1]) / exec_price, 2)
      cash_vec[i] <- 0
      position_state <- PositionState$LONG
    }
  } else if ((trade_mode == TradeDirection$SHORT || trade_mode == TradeDirection$LONG_SHORT) && entry_short) {
    exec_price <- switch(entry_timing,
                         open = open_vec[i],
                         close = close_vec[i],
                         next_open = if (i < tbl_rlen) open_vec[i + 1] else NA_real_
    )

    if (!is.na(exec_price)) {
      pos_size <- -round(ifelse(f_row, init_eqty, cash_vec[i - 1]) / exec_price, 2)
      pos_size_vec[i] <- pos_size
      cash_vec[i] <- round(ifelse(f_row, init_eqty, cash_vec[i-1]) + abs(pos_size * exec_price), 2)
      position_state <- PositionState$SHORT
    }
  }
}

# --- HOLDING ---
if (pos_size_vec[i] == 0 && cash_vec[i] == 0) { # No trade happened
    pos_size_vec[i] <- if (f_row) 0 else pos_size_vec[i - 1]
    cash_vec[i] <- if (f_row) init_eqty else cash_vec[i - 1]
}
```

**Context:**
The `strtgy_entry` and `strtgy_exit` functions in `EndScript.R` are defined as:
```R
strtgy_entry <- \(., strat_cfg) {
  ifelse(.$rn > 1 & (.$oc2 > .$UltimateSmoother & lag(.$oc2) <= lag(.$UltimateSmoother)),
         1, 0)
}
strtgy_exit <- \(., strat_cfg) {
  ifelse(.$rn > 1 & (.$oc2 < .$UltimateSmoother & lag(.$oc2) >= lag(.$UltimateSmoother)),
         1, 0)
}
```
For the short test, these were swapped in `EndScript.R`:
```R
strtgy_entry <- \(., strat_cfg) {
  ifelse(.$rn > 1 & (.$oc2 < .$UltimateSmoother & lag(.$oc2) >= lag(.$UltimateSmoother)),
         1, 0)
}
strtgy_exit <- \(., strat_cfg) {
  ifelse(.$rn > 1 & (.$oc2 > .$UltimateSmoother & lag(.$oc2) <= lag(.$UltimateSmoother)),
         1, 0)
}
```
The `add_trades_func` correctly creates `trd_entry_short` and `trd_exit_short` columns when `trade_mode` is `TradeDirection$SHORT`. The issue seems to be within the `bt_sim_func` loop where these signals are processed (or not processed). The `pos_size_vec[i] == 0 && cash_vec[i] == 0` condition for "HOLDING" might be prematurely resetting the state if an entry is attempted but not fully processed.
