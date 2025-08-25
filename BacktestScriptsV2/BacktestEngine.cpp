#include <iostream>
#include <vector>
#include <string>
#include <functional>
#include <cmath>
#include <numeric>
#include <algorithm>
#include <iomanip>
#include <map>

// =================================================================================
// DATA STRUCTURES
// =================================================================================

/**
 * @struct OHLC
 * @brief Represents a single bar of price data.
 */
struct OHLC {
    double open;
    double high;
    double low;
    double close;
    long long timestamp;
};

/**
 * @struct FractionalSellRule
 * @brief Defines a rule for selling a fraction of a position at a specific profit target.
 */
struct FractionalSellRule {
    double profit_target_pct;
    double fraction_to_sell;
};

/**
 * @struct RiskManagementConfig
 * @brief Holds all parameters related to risk and trade management.
 */
struct RiskManagementConfig {
    double stop_loss_pct = 0.0;
    double take_profit_pct = 0.0;
    std::vector<FractionalSellRule> fractional_sells;
};

/**
 * @enum TradeMode
 * @brief Defines the allowed trading directions for the strategy.
 */
enum class TradeMode { LONG, SHORT, LONG_SHORT, BUY_AND_HOLD };

/**
 * @enum TimingOption
 * @brief Specifies at which point in a bar a trade should be executed.
 */
enum class TimingOption { OPEN, CLOSE, NEXT_OPEN };

/**
 * @struct Config
 * @brief Holds all configuration parameters for a single backtest run.
 */
struct Config {
    double initial_equity;
    TradeMode trade_mode;
    std::string time_frame;
    TimingOption entry_timing;
    TimingOption exit_timing;
    RiskManagementConfig risk_config;
    double slippage_pct = 0.0;
    double commission_per_trade = 0.0;
    std::map<long long, double> dividend_data;
    std::function<bool(const OHLC&, const std::vector<OHLC>&, size_t)> long_entry;
    std::function<bool(const OHLC&, const std::vector<OHLC>&, size_t)> long_exit;
    std::function<bool(const OHLC&, const std::vector<OHLC>&, size_t)> short_entry;
    std::function<bool(const OHLC&, const std::vector<OHLC>&, size_t)> short_exit;
};

/**
 * @struct BarData
 * @brief Stores the complete state of the portfolio for a single bar. This is the primary output.
 */
struct BarData {
    int trade_number = 0;
    std::string position_state = "flat";
    int share_quantity = 0;
    double equity = 0.0;
    double cash = 0.0;
    double ongoing_pnl = 0.0;
    double realized_pnl = 0.0;
    double pnl_log_change_pct = 0.0;
    double equity_drawdown = 0.0;
    double equity_drawup = 0.0;
    double trade_drawdown = 0.0;
    double trade_drawup = 0.0;
};

// =================================================================================
// FORWARD DECLARATIONS OF HELPER FUNCTIONS
// =================================================================================

void process_entry(const std::string& direction, size_t i, const std::vector<OHLC>& ohlc_data, const Config& config, BarData& current_bar, int& trade_count, double& entry_price, double& peak_trade_equity, double& trough_trade_equity, std::map<int, bool>& fractional_sells_triggered);
void process_full_exit(const std::string& direction, const Config& config, BarData& current_bar, BarData& prev_bar, double& entry_price, double base_exit_price);
void process_partial_exit(const std::string& direction, const Config& config, double base_exit_price, double fraction, BarData& current_bar, double& entry_price);
double get_price(const OHLC& bar, TimingOption timing, const std::vector<OHLC>& ohlc_data, size_t current_index);
double get_price_with_slippage(double price, const std::string& direction, const Config& config);

// =================================================================================
// MAIN BACKTESTING ENGINE
// =================================================================================

/**
 * @brief The main function that orchestrates the entire backtest simulation.
 * @param ohlc_data A vector of OHLC structs representing the historical price data.
 * @param config A Config struct containing all parameters for the simulation.
 * @return A vector of BarData structs representing the portfolio's state at each bar.
 */
std::vector<BarData> run_backtest(const std::vector<OHLC>& ohlc_data, Config config) {
    std::vector<BarData> results;
    if (ohlc_data.empty()) {
        return results;
    }

    // --- Handle Buy-and-Hold Mode ---
    // If the mode is Buy and Hold, we override the signal functions to create a single
    // trade that spans the entire dataset. This is cleaner than a separate code path.
    if (config.trade_mode == TradeMode::BUY_AND_HOLD) {
        config.long_entry = [&](const OHLC&, const std::vector<OHLC>&, size_t i) {
            return i == 0; // Entry signal on the very first bar
        };
        config.long_exit = [&](const OHLC&, const std::vector<OHLC>&, size_t i) {
            return i == ohlc_data.size() - 1; // Exit signal on the very last bar
        };
    }

    BarData initial_bar;
    initial_bar.equity = config.initial_equity;
    initial_bar.cash = config.initial_equity;
    results.push_back(initial_bar);

    int trade_count = 0;
    double entry_price = 0.0;
    double peak_equity = config.initial_equity;
    double trough_equity = config.initial_equity;
    double peak_trade_equity = 0.0;
    double trough_trade_equity = 0.0;
    std::map<int, bool> fractional_sells_triggered;

    for (size_t i = 0; i < ohlc_data.size(); ++i) {
        BarData prev_bar = results.back();
        BarData current_bar = prev_bar;
        const OHLC& bar = ohlc_data[i];
        bool position_exited_this_bar = false;

        // --- 1. PROCESS POINT-IN-TIME EVENTS (DIVIDENDS) ---
        if (!config.dividend_data.empty() && current_bar.position_state == "long") {
            if (config.time_frame == "1d") {
                auto it = config.dividend_data.find(bar.timestamp);
                if (it != config.dividend_data.end()) {
                    current_bar.cash += (it->second * current_bar.share_quantity);
                }
            } else {
                long long bar_start_time = bar.timestamp;
                long long bar_end_time = (i + 1 < ohlc_data.size()) ? ohlc_data[i+1].timestamp : 9999999999LL;
                double total_dividends_for_bar = 0.0;
                auto it = config.dividend_data.lower_bound(bar_start_time);
                while (it != config.dividend_data.end() && it->first < bar_end_time) {
                    total_dividends_for_bar += it->second;
                    ++it;
                }
                if (total_dividends_for_bar > 0.0) {
                    current_bar.cash += (total_dividends_for_bar * current_bar.share_quantity);
                }
            }
        }

        // --- 2. PROCESS EXITS (RISK AND SIGNAL) FOR OPEN POSITIONS ---
        if (current_bar.position_state == "long") {
            if (config.risk_config.stop_loss_pct > 0.0) {
                double stop_price = entry_price * (1.0 - config.risk_config.stop_loss_pct);
                if (bar.low <= stop_price) {
                    process_full_exit("long", config, current_bar, prev_bar, entry_price, stop_price);
                    position_exited_this_bar = true;
                }
            }
            if (!position_exited_this_bar && config.risk_config.take_profit_pct > 0.0) {
                double take_profit_price = entry_price * (1.0 + config.risk_config.take_profit_pct);
                if (bar.high >= take_profit_price) {
                    process_full_exit("long", config, current_bar, prev_bar, entry_price, take_profit_price);
                    position_exited_this_bar = true;
                }
            }
            if (!position_exited_this_bar) {
                for (size_t j = 0; j < config.risk_config.fractional_sells.size(); ++j) {
                    const auto& rule = config.risk_config.fractional_sells[j];
                    if (!fractional_sells_triggered[j]) {
                        double partial_profit_price = entry_price * (1.0 + rule.profit_target_pct);
                        if (bar.high >= partial_profit_price) {
                            process_partial_exit("long", config, partial_profit_price, rule.fraction_to_sell, current_bar, entry_price);
                            fractional_sells_triggered[j] = true;
                        }
                    }
                }
            }
            if (!position_exited_this_bar && config.long_exit && config.long_exit(bar, ohlc_data, i)) {
                double base_exit_price = get_price(bar, config.exit_timing, ohlc_data, i);
                process_full_exit("long", config, current_bar, prev_bar, entry_price, base_exit_price);
                position_exited_this_bar = true;
            }
        }

        // --- 3. PROCESS ENTRIES IF POSITION IS FLAT ---
        if (current_bar.position_state == "flat" && !position_exited_this_bar) {
            if ((config.trade_mode == TradeMode::LONG || config.trade_mode == TradeMode::LONG_SHORT || config.trade_mode == TradeMode::BUY_AND_HOLD) && config.long_entry && config.long_entry(bar, ohlc_data, i)) {
                process_entry("long", i, ohlc_data, config, current_bar, trade_count, entry_price, peak_trade_equity, trough_trade_equity, fractional_sells_triggered);
            }
        }

        // --- 4. UPDATE ONGOING METRICS FOR THE BAR ---
        if (current_bar.position_state == "long") {
            current_bar.ongoing_pnl = (bar.close - entry_price) * current_bar.share_quantity;
            current_bar.equity = current_bar.cash + current_bar.realized_pnl + (bar.close * current_bar.share_quantity);
        } else {
            current_bar.equity = current_bar.cash;
            current_bar.ongoing_pnl = 0;
        }

        peak_equity = std::max(peak_equity, current_bar.equity);
        trough_equity = std::min(trough_equity, current_bar.equity);
        current_bar.equity_drawdown = (peak_equity > 0) ? (peak_equity - current_bar.equity) / peak_equity : 0;
        current_bar.equity_drawup = (trough_equity > 0) ? (current_bar.equity - trough_equity) / trough_equity : 0;

        if (current_bar.position_state != "flat") {
            double current_trade_value = current_bar.cash + current_bar.realized_pnl + (bar.close * current_bar.share_quantity);
            peak_trade_equity = std::max(peak_trade_equity, current_trade_value);
            trough_trade_equity = std::min(trough_trade_equity, current_trade_value);
            current_bar.trade_drawdown = (peak_trade_equity > 0) ? (peak_trade_equity - current_trade_value) / peak_trade_equity : 0;
            current_bar.trade_drawup = (trough_trade_equity > 0) ? (current_trade_value - trough_trade_equity) / trough_trade_equity : 0;
        } else {
            current_bar.trade_drawdown = 0;
            current_bar.trade_drawup = 0;
        }

        results.push_back(current_bar);
    }
    return results;
}

// =================================================================================
// HELPER FUNCTION IMPLEMENTATIONS
// =================================================================================

void process_entry(const std::string& direction, size_t i, const std::vector<OHLC>& ohlc_data, const Config& config, BarData& current_bar, int& trade_count, double& entry_price, double& peak_trade_equity, double& trough_trade_equity, std::map<int, bool>& fractional_sells_triggered) {
    const OHLC& bar = ohlc_data[i];
    double base_price = get_price(bar, config.entry_timing, ohlc_data, i);
    
    if (direction == "long") {
        double execution_price = get_price_with_slippage(base_price, "buy", config);
        double cash_for_purchase = current_bar.cash - config.commission_per_trade;
        if (cash_for_purchase <= 0) return;

        current_bar.share_quantity = static_cast<int>(cash_for_purchase / execution_price);
        if (current_bar.share_quantity == 0) return;

        double cost = current_bar.share_quantity * execution_price;
        current_bar.cash -= (cost + config.commission_per_trade);
        current_bar.position_state = "long";
        entry_price = base_price;
    }

    trade_count++;
    current_bar.trade_number = trade_count;
    current_bar.realized_pnl = 0.0;
    peak_trade_equity = trough_trade_equity = current_bar.equity;
    fractional_sells_triggered.clear();
}

void process_full_exit(const std::string& direction, const Config& config, BarData& current_bar, BarData& prev_bar, double& entry_price, double base_exit_price) {
    if (direction == "long") {
        double execution_price = get_price_with_slippage(base_exit_price, "sell", config);
        double proceeds = (current_bar.share_quantity * execution_price);
        current_bar.cash += (proceeds - config.commission_per_trade);
        current_bar.equity = current_bar.cash + current_bar.realized_pnl;
        if (prev_bar.equity != 0) {
            current_bar.pnl_log_change_pct = log(current_bar.equity / prev_bar.equity);
        }
    }

    current_bar.share_quantity = 0;
    current_bar.position_state = "flat";
    entry_price = 0.0;
}

void process_partial_exit(const std::string& direction, const Config& config, double base_exit_price, double fraction, BarData& current_bar, double& entry_price) {
    if (direction == "long") {
        int initial_shares = static_cast<int>(current_bar.share_quantity / (1.0 - fraction));
        int shares_to_sell = static_cast<int>(initial_shares * fraction);

        if (shares_to_sell > 0 && current_bar.share_quantity >= shares_to_sell) {
            double execution_price = get_price_with_slippage(base_exit_price, "sell", config);
            double pnl_from_sale = (execution_price - get_price_with_slippage(entry_price, "buy", config)) * shares_to_sell;
            
            current_bar.realized_pnl += pnl_from_sale;
            current_bar.cash += (shares_to_sell * execution_price) - config.commission_per_trade;
            current_bar.share_quantity -= shares_to_sell;
        }
    }
}

double get_price(const OHLC& bar, TimingOption timing, const std::vector<OHLC>& ohlc_data, size_t current_index) {
    switch (timing) {
        case TimingOption::OPEN: return bar.open;
        case TimingOption::CLOSE: return bar.close;
        case TimingOption::NEXT_OPEN:
            if (current_index + 1 < ohlc_data.size()) {
                return ohlc_data[current_index + 1].open;
            }
            return bar.close; // Fallback for last bar
    }
    return bar.close; // Default
}

double get_price_with_slippage(double price, const std::string& direction, const Config& config) {
    if (direction == "buy") return price * (1.0 + config.slippage_pct);
    if (direction == "sell") return price * (1.0 - config.slippage_pct);
    return price;
}