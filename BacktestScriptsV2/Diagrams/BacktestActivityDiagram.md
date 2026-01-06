```mermaid
activityDiagram
    title Backtest Process Flow

    |MainScript.R|
    start
    :1. Sourcing & Configuration;
    :Source R scripts (Preprocessor, AdhocCalcs);
    :Compile & source C++ files via Rcpp
    (BacktestHandler, ultimate_smoother);
    :Set configuration (TIME_FRAME, DB_PATH);

    :2. Data Preparation;
    :Connect to DuckDB;
    :Fetch daily price data via SQL;
    :Disconnect from DB;

    |#LightBlue:Preprocessor.R|
    :Call to_agg_timeframe();
    note right: Aggregates daily data to<br>the specified TIME_FRAME (e.g., '1w').
    
    |MainScript.R|
    :Data Aggregated;

    |#Thistle:ultimate_smoother.cpp|
    :Call ultimateSmoother() in a loop;
    note right: C++ function calculates<br>technical indicators for each row.
    
    |MainScript.R|
    :Indicators Calculated;
    :Filter data for backtest period;
    
    :3. Backtest Execution;
    :Define strategy configuration list (strategy_cfg)
    including entry/exit signal functions in R;
    
    |#Coral:BacktestHandler.cpp|
    :Call run_backtest_r();
    partition Backtest Core Logic {
        :Loop through each row of the time-series data;
        if (In an open position?) then (yes)
            :Check for exit signal (calls R function);
            if (Exit signal triggered?) then (yes)
                :Close position;
                :Record trade;
            else (no)
                :Check for stop-loss or take-profit;
                if (SL/TP hit?) then (yes)
                    :Close position;
                    :Record trade;
                else (no)
                    :Hold position;
                endif
            endif
        else (no)
            :Check for entry signal (calls R function);
            if (Entry signal triggered?) then (yes)
                :Open new position;
            endif
        endif
        :Update equity and performance metrics;
    }
    :Return results data.frame;

    |MainScript.R|
    :Backtest Finished;
    :Print results summary;
    
    :4. Ad-hoc Analysis;
    :Prepare data for analysis;

    |#LightGreen:AdhocCalcsScript.R|
    :Call calculate_short_dividend_cost();
    note right: Performs post-backtest<br>dividend cost calculation.
    
    |MainScript.R|
    :Analysis Complete;
    stop
```
