### 1. Simple Long-Only Strategy
// SCENARIO: A basic trend-following strategy that only takes long positions.
// No advanced risk management, no costs. Good for a baseline performance check.
Config simple_long_config = {
    100000.0,                           // initial_equity
    TradeMode::LONG,                    // trade_mode
    "1d",                               // time_frame
    TimingOption::CLOSE,                // entry_timing
    TimingOption::CLOSE,                // exit_timing
    {},                                 // risk_config (empty)
    0.0,                                // slippage_pct
    0.0,                                // commission_per_trade
    {},                                 // dividend_data (empty)
    example_long_entry,                 // long_entry function
    example_long_exit,                  // long_exit function
    nullptr,                            // short_entry (disabled)
    nullptr                             // short_exit (disabled)
};

### 2. Buy and Hold Benchmark
// SCENARIO: A simple benchmark to compare your strategy against.
// Buys on the first bar and holds until the last. Costs are included for a fair comparison.
// NOTE: The `run_backtest` function has special logic to handle this mode; signal functions are ignored.
Config buy_and_hold_config = {
    100000.0,
    TradeMode::BUY_AND_HOLD,
    "1d",
    TimingOption::OPEN, // Buy at the first day's open
    TimingOption::CLOSE, // Sell at the last day's close
    {},
    0.0005, // 0.05% slippage
    1.00,   // $1 commission for entry
    {},
    nullptr, // Ignored
    nullptr, // Ignored
    nullptr, // Ignored
    nullptr  // Ignored
};

### 3. Strategy with Full Risk Management & Costs
// SCENARIO: A realistic test of a long-only strategy including slippage, commissions,
// a hard stop-loss, and a partial take-profit rule.
Config full_risk_config = {
    100000.0,
    TradeMode::LONG,
    "1d",
    TimingOption::CLOSE,
    TimingOption::CLOSE,
    { // risk_config
        0.08, // 8% Stop Loss
        0.25, // 25% Take Profit (full exit)
        {     // Fractional Sell Rules
            {0.10, 0.50} // Sell 50% of position at 10% profit
        }
    },
    0.0005, // 0.05% slippage
    1.50,   // $1.50 commission per trade
    {},
    example_long_entry,
    example_long_exit,
    nullptr,
    nullptr
};

### 4. Total Return Test on Daily Data (e.g., Covered Call ETF)
// SCENARIO: Testing a total return strategy on daily data.
// NOTE: The `dividend_data` map uses Unix timestamps for the keys, which MUST be the ex-dividend dates.
// The `time_frame` is set to "1d", so the engine will look for an exact timestamp match for each bar.
Config total_return_daily_config = {
    100000.0,
    TradeMode::LONG,
    "1d", // Critical: Set to "1d" for precise dividend processing
    TimingOption::CLOSE,
    TimingOption::CLOSE,
    {},
    0.0, // Assume low/no costs for this example
    0.0,
    { // dividend_data: map of [ex_dividend_timestamp -> dividend_per_share]
        {1678886400, 0.55}, // Example: Pays $0.55/share on this date
        {1686662400, 0.58}  // Example: Pays $0.58/share on this date
    },
    example_long_entry,
    example_long_exit,
    nullptr,
    nullptr
};

### 5. Total Return Test on Monthly Data
// SCENARIO: Testing a total return strategy on monthly data.
// NOTE: The `time_frame` is now "1mo". The engine will aggregate all dividends
// that have an ex-date falling between the start of one monthly bar and the start of the next.
Config total_return_monthly_config = {
    100000.0,
    TradeMode::LONG,
    "1mo", // Critical: Set to "1mo" for aggregation logic
    TimingOption::CLOSE,
    TimingOption::CLOSE,
    {},
    0.0,
    0.0,
    { // dividend_data: The same dividend data as the daily test
        {1678886400, 0.55},
        {1686662400, 0.58}
    },
    example_long_entry,
    example_long_exit,
    nullptr,
    nullptr
};