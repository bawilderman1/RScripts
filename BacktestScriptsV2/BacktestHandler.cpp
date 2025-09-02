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

struct SignalVectors {
    Rcpp::IntegerVector long_entry;
    Rcpp::IntegerVector long_exit;
    Rcpp::IntegerVector short_entry;
    Rcpp::IntegerVector short_exit;
};

// Helper to resolve a single pair of signals (entry/exit)
std::pair<Rcpp::IntegerVector, Rcpp::IntegerVector> resolve_signal_pair(const Rcpp::List& config_list, const Rcpp::DataFrame& ohlc_df, const std::string& entry_name, const std::string& exit_name) {
    Rcpp::IntegerVector entry_vec;
    Rcpp::IntegerVector exit_vec;

    std::string col_entry_name = entry_name + "_signal";
    std::string col_exit_name = exit_name + "_signal";

    if (config_list.containsElementNamed(entry_name.c_str()) && Rf_isFunction(config_list[entry_name.c_str()])) {
        if (!config_list.containsElementNamed(exit_name.c_str()) || !Rf_isFunction(config_list[exit_name.c_str()])) {
            Rcpp::stop("If '" + entry_name + "' is a function, '" + exit_name + "' must also be a function.");
        }
        Rcpp::Function entry_func = config_list[entry_name.c_str()];
        Rcpp::Function exit_func = config_list[exit_name.c_str()];
        entry_vec = Rcpp::as<Rcpp::IntegerVector>(Rcpp::LogicalVector(entry_func(ohlc_df)));
        exit_vec = Rcpp::as<Rcpp::IntegerVector>(Rcpp::LogicalVector(exit_func(ohlc_df)));
    } else if (hasColumn(ohlc_df, col_entry_name) && hasColumn(ohlc_df, col_exit_name)) {
        entry_vec = ohlc_df[col_entry_name];
        exit_vec = ohlc_df[col_exit_name];
    }
    return std::make_pair(entry_vec, exit_vec);
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
    config.long_entry = nullptr;
    config.long_exit = nullptr;
    config.short_entry = nullptr;
    config.short_exit = nullptr;

    if (config.trade_mode == TradeMode::LONG || config.trade_mode == TradeMode::LONG_SHORT) {
        auto long_signals = resolve_signal_pair(config_list, ohlc_df, "long_entry", "long_exit");
        if (long_signals.first.size() > 0) { // Check if signals were found
            config.long_entry = [vec = long_signals.first](const OHLC&, const auto&, size_t i) { return vec[i] == 1; };
            config.long_exit = [vec = long_signals.second](const OHLC&, const auto&, size_t i) { return vec[i] == 1; };
        } else if (config.trade_mode == TradeMode::LONG) {
             Rcpp::stop("For LONG mode, valid signals must be provided (functions or columns).");
        }
    }

    if (config.trade_mode == TradeMode::SHORT || config.trade_mode == TradeMode::LONG_SHORT) {
        auto short_signals = resolve_signal_pair(config_list, ohlc_df, "short_entry", "short_exit");
        if (short_signals.first.size() > 0) { // Check if signals were found
            config.short_entry = [vec = short_signals.first](const OHLC&, const auto&, size_t i) { return vec[i] == 1; };
            config.short_exit = [vec = short_signals.second](const OHLC&, const auto&, size_t i) { return vec[i] == 1; };
        } else if (config.trade_mode == TradeMode::SHORT) {
             Rcpp::stop("For SHORT mode, valid signals must be provided (functions or columns).");
        }
    }
    
    if (config.trade_mode == TradeMode::LONG_SHORT && !config.long_entry && !config.short_entry) {
        Rcpp::stop("For LONG_SHORT mode, at least one set of valid signals (long or short) must be provided.");
    }

    // --- 4. RUN THE BACKTEST ---
    std::vector<BarData> results = run_backtest(ohlc_data, config);

    // --- 5. CONVERT RESULTS BACK TO R DATAFRAME ---
    Rcpp::DateVector dt_out;
    Rcpp::NumericVector share_quantity_out;
    Rcpp::CharacterVector position_state_out;
    Rcpp::NumericVector equity_out, cash_out, ongoing_pnl_out, realized_pnl_out, 
                          pnl_log_change_pct_out, equity_drawdown_out, equity_drawup_out, 
                          trade_drawdown_out, trade_drawup_out;
    Rcpp::IntegerVector trade_number_out;

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

    dt_out.attr("class") = "Date";

    dt_out.attr("class") = "Date";

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