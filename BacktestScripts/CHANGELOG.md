## 2025-08-12

- **Goal**: Refactor the backtesting engine to support long and short positions.
- **Files Affected**: `backtest_engine_gemini.R`, `EndScript.R`
- **Summary of Changes**:
    - Moved all backtesting functions from `MainScript.R` and `EndScript.R` to a new file `backtest_engine_gemini.R`.
    - Created `PositionState` and `TradeDirection` enums to manage trade states and directions.
    - Refactored `add_trades_func` to handle `long`, `short`, and `long_short` trade modes.
    - Refactored `bt_sim_func` to use the new `PositionState` enum and handle both long and short trades.
    - Successfully tested the `long` trade direction, which produces the same results as the original script.
- **Current Status**: The `short` trade direction is not working as expected. The backtest runs without errors but does not generate any trades. The issue seems to be in the trade execution logic within `bt_sim_func`. Further debugging is required to identify and fix the issue with the short-selling logic.