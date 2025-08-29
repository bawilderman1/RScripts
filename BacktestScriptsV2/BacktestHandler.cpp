#include <Rcpp.h>
#include "BacktestEngine.h"

// =================================================================================
// HELPER FUNCTIONS
// =================================================================================

TradeMode map_trade_mode(std::string mode_str) {
    if (mode_str == "LONG") return TradeMode::LONG;
    if (mode_str == "SHORT") return TradeMode::SHORT;
    if (mode_str == "LONG_SHORT") return TradeMode::LONG_SHORT;
    if (mode_str == "BUY_AND_HOLD") return TradeMode::BUY_AND_HOLD;
    Rcpp::stop("Invalid trade_mode: " + mode_str);
}

TimingOption map_timing_option(std::string timing_str) {
    if (timing_str == "OPEN") return TimingOption::OPEN;
    if (timing_str == "CLOSE") return TimingOption::CLOSE;
    if (timing_str == "NEXT_OPEN") return TimingOption::NEXT_OPEN;
    Rcpp::stop("Invalid timing_option: " + timing_str);
}

bool hasColumn(const Rcpp::DataFrame& df, const std::string& colName) {
    Rcpp::CharacterVector cols = df.names();
    return std::find(cols.begin(), cols.end(), colName) != cols.end();
}

// =================================================================================
// MAIN EXPORTED FUNCTION
// =================================================================================

// [[Rcpp::export]]
Rcpp::DataFrame run_backtest_r(Rcpp::DataFrame ohlc_df, Rcpp::List config_list) {
    // --- 1. POPULATE THE C++ CONFIG STRUCT ---
    Config config;
    config.initial_equity = Rcpp::as<double>(config_list["initial_equity"]);
    config.slippage_pct = Rcpp::as<double>(config_list["slippage_pct"]);
    config.commission_per_trade = Rcpp::as<double>(config_list["commission_per_trade"]);
    config.time_frame = Rcpp::as<std::string>(config_list["time_frame"]);
    config.trade_mode = map_trade_mode(Rcpp::as<std::string>(config_list["trade_mode"]));
    config.entry_timing = map_timing_option(Rcpp::as<std::string>(config_list["entry_timing"]));
    config.exit_timing = map_timing_option(Rcpp::as<std::string>(config_list["exit_timing"]));

    if (config_list.containsElementNamed("risk_config")) {
        Rcpp::List risk_list = config_list["risk_config"];
        if(risk_list.containsElementNamed("stop_loss_pct")) config.risk_config.stop_loss_pct = Rcpp::as<double>(risk_list["stop_loss_pct"]);
        if(risk_list.containsElementNamed("take_profit_pct")) config.risk_config.take_profit_pct = Rcpp::as<double>(risk_list["take_profit_pct"]);
        if (risk_list.containsElementNamed("fractional_sells")) {
            Rcpp::DataFrame fs_df = risk_list["fractional_sells"];
            Rcpp::NumericVector profit_targets = fs_df["profit_target_pct"];
            Rcpp::NumericVector fractions = fs_df["fraction_to_sell"];
            for (int i = 0; i < fs_df.nrows(); ++i) {
                config.risk_config.fractional_sells.push_back({profit_targets(i), fractions(i)});
            }
        }
    }

    if (config_list.containsElementNamed("dividend_data")) {
        Rcpp::DataFrame div_df = config_list["dividend_data"];
        if (div_df.nrows() > 0) {
            Rcpp::DateVector dates = div_df["ex_date"];
            Rcpp::NumericVector amounts = div_df["dividend_amount"];
            for (int i = 0; i < div_df.nrows(); ++i) {
                long long timestamp = static_cast<long long>(dates(i)) * 86400;
                config.dividend_data[timestamp] = amounts(i);
            }
        }
    }

    // --- 2. CONVERT R DATAFRAME TO C++ OHLC VECTOR ---
    std::vector<OHLC> ohlc_data;
    ohlc_data.reserve(ohlc_df.nrows());
    Rcpp::NumericVector open = ohlc_df["open"];
    Rcpp::NumericVector high = ohlc_df["high"];
    Rcpp::NumericVector low = ohlc_df["low"];
    Rcpp::NumericVector close = ohlc_df["close"];
    Rcpp::DateVector dates = ohlc_df["dt"];
    for (int i = 0; i < ohlc_df.nrows(); ++i) {
        ohlc_data.push_back({ open(i), high(i), low(i), close(i), static_cast<long long>(dates(i)) * 86400 });
    }

    // --- 3. CREATE AND ASSIGN SIGNAL FUNCTIONS ---
    if (config.trade_mode == TradeMode::LONG || config.trade_mode == TradeMode::LONG_SHORT) {
        if (!hasColumn(ohlc_df, "entry_signal") || !hasColumn(ohlc_df, "exit_signal")) {
            Rcpp::stop("For LONG or LONG_SHORT mode, ohlc_df must contain 'entry_signal' and 'exit_signal' columns.");
        }
        Rcpp::IntegerVector entry_vec = ohlc_df["entry_signal"];
        Rcpp::IntegerVector exit_vec = ohlc_df["exit_signal"];
        config.long_entry = [entry_vec](const OHLC&, const auto&, size_t i) { return entry_vec(i) == 1; };
        config.long_exit = [exit_vec](const OHLC&, const auto&, size_t i) { return exit_vec(i) == 1; };
        
        // For now, short signals are not implemented from R
        config.short_entry = nullptr;
        config.short_exit = nullptr;

    } else {
        // For BUY_AND_HOLD or other modes, ensure all functions are null
        config.long_entry = nullptr;
        config.long_exit = nullptr;
        config.short_entry = nullptr;
        config.short_exit = nullptr;
    }

    // --- 4. RUN THE BACKTEST ---
    std::vector<BarData> results = run_backtest(ohlc_data, config);

    // --- 5. CONVERT RESULTS BACK TO R DATAFRAME ---
    Rcpp::DateVector dt_out;
    Rcpp::IntegerVector trade_number_out, share_quantity_out;
    Rcpp::CharacterVector position_state_out;
    Rcpp::NumericVector equity_out, cash_out, ongoing_pnl_out, realized_pnl_out, 
                          pnl_log_change_pct_out, equity_drawdown_out, equity_drawup_out, 
                          trade_drawdown_out, trade_drawup_out;

    if (static_cast<R_xlen_t>(results.size()) == dates.size() + 1) {
        for (size_t i = 1; i < results.size(); ++i) {
            const auto& bar = results[i];
            dt_out.push_back(dates(i-1));
            trade_number_out.push_back(bar.trade_number);
            position_state_out.push_back(bar.position_state);
            share_quantity_out.push_back(bar.share_quantity);
            equity_out.push_back(bar.equity);
            cash_out.push_back(bar.cash);
            ongoing_pnl_out.push_back(bar.ongoing_pnl);
            realized_pnl_out.push_back(bar.realized_pnl);
            pnl_log_change_pct_out.push_back(bar.pnl_log_change_pct);
            equity_drawdown_out.push_back(bar.equity_drawdown);
            equity_drawup_out.push_back(bar.equity_drawup);
            trade_drawdown_out.push_back(bar.trade_drawdown);
            trade_drawup_out.push_back(bar.trade_drawup);
        }
    }

    return Rcpp::DataFrame::create(
        Rcpp::Named("dt") = dt_out,
        Rcpp::Named("trade_number") = trade_number_out,
        Rcpp::Named("position_state") = position_state_out,
        Rcpp::Named("share_quantity") = share_quantity_out,
        Rcpp::Named("equity") = equity_out,
        Rcpp::Named("cash") = cash_out,
        Rcpp::Named("ongoing_pnl") = ongoing_pnl_out,
        Rcpp::Named("realized_pnl") = realized_pnl_out,
        Rcpp::Named("pnl_log_change_pct") = pnl_log_change_pct_out,
        Rcpp::Named("equity_drawdown_pct") = equity_drawdown_out,
        Rcpp::Named("equity_drawup_pct") = equity_drawup_out,
        Rcpp::Named("trade_drawdown_pct") = trade_drawdown_out,
        Rcpp::Named("trade_drawup_pct") = trade_drawup_out
    );
}