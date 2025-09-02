#include "BacktestEngine.h"
#include <iostream>
#include <vector>
#include <string>
#include <functional>
#include <cmath>
#include <numeric>
#include <algorithm>
#include <iomanip>
#include <map>

// Helper function for rounding
double round_to(double value, int decimal_places) {
    const double multiplier = std::pow(10.0, decimal_places);
    return std::round(value * multiplier) / multiplier;
}


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

    BarData initial_bar;
    initial_bar.equity = round_to(config.initial_equity, 2);
    initial_bar.cash = round_to(config.initial_equity, 2);
    results.push_back(initial_bar);

    int trade_count = 0;
    double entry_price = 0.0;
    double cost_basis = 0.0;
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

        // --- 1. PROCESS DIVIDENDS ---
        if (!config.dividend_data.empty()) {
            long long bar_start_time = bar.timestamp;
            long long bar_end_time = (i + 1 < ohlc_data.size()) ? ohlc_data[i+1].timestamp : 9999999999LL;
            double total_dividends_for_bar = 0.0;
            auto it = config.dividend_data.lower_bound(bar_start_time);
            while (it != config.dividend_data.end() && it->first < bar_end_time) {
                total_dividends_for_bar += it->second;
                ++it;
            }

            if (total_dividends_for_bar > 0.0) {
                double dividend_cash = total_dividends_for_bar * current_bar.share_quantity;
                if (current_bar.position_state == "long") {
                    // Reinvest dividend
                    double reinvest_price = get_price(bar, TimingOption::CLOSE, ohlc_data, i);
                    if (reinvest_price > 0) {
                        double shares_to_buy = dividend_cash / reinvest_price;
                        current_bar.share_quantity += shares_to_buy;
                        // Cost basis update is tricky here. For simplicity, we assume the new shares have a cost basis equal to their purchase price.
                        // A more advanced implementation would track lots.
                    }
                } else if (current_bar.position_state == "short") {
                    current_bar.cash -= dividend_cash;
                }
            }
        }

        // --- 2. PROCESS EXITS (RISK AND SIGNAL) FOR OPEN POSITIONS ---
        if (current_bar.position_state == "long") {
            if (config.risk_config.stop_loss_pct > 0.0) {
                double stop_price = entry_price * (1.0 - config.risk_config.stop_loss_pct);
                if (bar.low <= stop_price) {
                    process_full_exit("long", config, current_bar, prev_bar, entry_price, cost_basis, stop_price);
                    position_exited_this_bar = true;
                }
            }
            if (!position_exited_this_bar && config.risk_config.take_profit_pct > 0.0) {
                double take_profit_price = entry_price * (1.0 + config.risk_config.take_profit_pct);
                if (bar.high >= take_profit_price) {
                    process_full_exit("long", config, current_bar, prev_bar, entry_price, cost_basis, take_profit_price);
                    position_exited_this_bar = true;
                }
            }
            if (!position_exited_this_bar) {
                for (size_t j = 0; j < config.risk_config.fractional_sells.size(); ++j) {
                    const auto& rule = config.risk_config.fractional_sells[j];
                    if (!fractional_sells_triggered[j]) {
                        double partial_profit_price = entry_price * (1.0 + rule.profit_target_pct);
                        if (bar.high >= partial_profit_price) {
                            process_partial_exit("long", config, partial_profit_price, rule.fraction_to_sell, current_bar, entry_price, cost_basis);
                            fractional_sells_triggered[j] = true;
                        }
                    }
                }
            }
            bool is_bnh_exit = (config.trade_mode == TradeMode::BUY_AND_HOLD && i == ohlc_data.size() - 1);
            bool is_signal_exit = config.long_exit && config.long_exit(bar, ohlc_data, i);

            if (!position_exited_this_bar && (is_bnh_exit || is_signal_exit)) {
                double base_exit_price = get_price(bar, config.exit_timing, ohlc_data, i);
                process_full_exit("long", config, current_bar, prev_bar, entry_price, cost_basis, base_exit_price);
                position_exited_this_bar = true;
            }
        } else if (current_bar.position_state == "short") {
            if (config.risk_config.stop_loss_pct > 0.0) {
                double stop_price = entry_price * (1.0 + config.risk_config.stop_loss_pct);
                if (bar.high >= stop_price) {
                    process_full_exit("short", config, current_bar, prev_bar, entry_price, cost_basis, stop_price);
                    position_exited_this_bar = true;
                }
            }
            if (!position_exited_this_bar && config.risk_config.take_profit_pct > 0.0) {
                double take_profit_price = entry_price * (1.0 - config.risk_config.take_profit_pct);
                if (bar.low <= take_profit_price) {
                    process_full_exit("short", config, current_bar, prev_bar, entry_price, cost_basis, take_profit_price);
                    position_exited_this_bar = true;
                }
            }
            if (!position_exited_this_bar && config.short_exit && config.short_exit(bar, ohlc_data, i)) {
                process_full_exit("short", config, current_bar, prev_bar, entry_price, cost_basis, get_price(bar, config.exit_timing, ohlc_data, i));
                position_exited_this_bar = true;
            }
        }

        // --- 3. PROCESS ENTRIES IF POSITION IS FLAT ---
        if (current_bar.position_state == "flat" && !position_exited_this_bar) {
            bool is_bnh_entry = (config.trade_mode == TradeMode::BUY_AND_HOLD && i == 0);
            bool is_long_signal_entry = (config.trade_mode == TradeMode::LONG || config.trade_mode == TradeMode::LONG_SHORT) && config.long_entry && config.long_entry(bar, ohlc_data, i);
            bool is_short_signal_entry = (config.trade_mode == TradeMode::SHORT || config.trade_mode == TradeMode::LONG_SHORT) && config.short_entry && config.short_entry(bar, ohlc_data, i);

            if (is_bnh_entry || is_long_signal_entry) {
                process_entry("long", i, ohlc_data, config, current_bar, trade_count, entry_price, cost_basis, peak_trade_equity, trough_trade_equity, fractional_sells_triggered);
            } else if (is_short_signal_entry) {
                process_entry("short", i, ohlc_data, config, current_bar, trade_count, entry_price, cost_basis, peak_trade_equity, trough_trade_equity, fractional_sells_triggered);
            }
        }

        // --- 4. UPDATE ONGOING METRICS FOR THE BAR ---
        if (current_bar.position_state == "long") {
            current_bar.ongoing_pnl = (bar.close - entry_price) * current_bar.share_quantity;
            current_bar.equity = current_bar.cash + (bar.close * current_bar.share_quantity);
        } else if (current_bar.position_state == "short") {
            current_bar.ongoing_pnl = (entry_price - bar.close) * current_bar.share_quantity;
            current_bar.equity = current_bar.cash - (bar.close * current_bar.share_quantity);
        } else {
            current_bar.equity = current_bar.cash;
            current_bar.ongoing_pnl = 0;
        }

        peak_equity = std::max(peak_equity, current_bar.equity);
        trough_equity = std::min(trough_equity, current_bar.equity);
        
        // --- 5. ROUNDING AND FINAL CALCULATIONS ---
        current_bar.share_quantity = round_to(current_bar.share_quantity, 3);
        current_bar.equity = round_to(current_bar.equity, 2);
        current_bar.cash = round_to(current_bar.cash, 2);
        current_bar.ongoing_pnl = round_to(current_bar.ongoing_pnl, 2);
        current_bar.realized_pnl = round_to(current_bar.realized_pnl, 2);

        current_bar.equity_drawdown = (peak_equity > 0) ? round_to((peak_equity - current_bar.equity) / peak_equity, 4) : 0;
        current_bar.equity_drawup = (trough_equity > 0) ? round_to((current_bar.equity - trough_equity) / trough_equity, 4) : 0;

        if (current_bar.position_state != "flat") {
            double current_trade_value = current_bar.equity;
            peak_trade_equity = std::max(peak_trade_equity, current_trade_value);
            trough_trade_equity = std::min(trough_trade_equity, current_trade_value);
            current_bar.trade_drawdown = (peak_trade_equity > 0) ? round_to((peak_trade_equity - current_trade_value) / peak_trade_equity, 4) : 0;
            current_bar.trade_drawup = (trough_trade_equity > 0) ? round_to((current_trade_value - trough_trade_equity) / trough_trade_equity, 4) : 0;
        } else {
            current_bar.trade_drawdown = 0;
            current_bar.trade_drawup = 0;
        }
        
        if (results.back().equity != 0) {
            current_bar.pnl_log_change_pct = round_to(log(current_bar.equity / results.back().equity), 4);
        } else {
            current_bar.pnl_log_change_pct = 0;
        }

        results.push_back(current_bar);
    }
    return results;
}

// =================================================================================
// HELPER FUNCTION IMPLEMENTATIONS
// =================================================================================

void process_entry(const std::string& direction, size_t i, const std::vector<OHLC>& ohlc_data, const Config& config, BarData& current_bar, int& trade_count, double& entry_price, double& cost_basis, double& peak_trade_equity, double& trough_trade_equity, std::map<int, bool>& fractional_sells_triggered) {
    const OHLC& bar = ohlc_data[i];
    double base_price = get_price(bar, config.entry_timing, ohlc_data, i);
    
    if (direction == "long") {
        double execution_price = get_price_with_slippage(base_price, "buy", config);
        double cash_for_purchase = current_bar.cash - config.commission_per_trade;
        if (cash_for_purchase <= 0) return;

        current_bar.share_quantity = static_cast<int>(cash_for_purchase / execution_price); // Whole shares for new entries
        if (current_bar.share_quantity == 0) return;

        double cost = current_bar.share_quantity * execution_price;
        cost_basis = cost;
        current_bar.cash -= (cost + config.commission_per_trade);
        current_bar.position_state = "long";
        entry_price = base_price;
    } else if (direction == "short") {
        double execution_price = get_price_with_slippage(base_price, "sell", config);
        double short_value = current_bar.equity;
        if (short_value <= 0) return;
        current_bar.share_quantity = static_cast<int>(short_value / execution_price); // Whole shares for new entries
        if (current_bar.share_quantity == 0) return;
        double proceeds = current_bar.share_quantity * execution_price;
        cost_basis = proceeds;
        current_bar.cash += (proceeds - config.commission_per_trade);
        current_bar.position_state = "short";
        entry_price = base_price;
    }

    trade_count++;
    current_bar.trade_number = trade_count;
    peak_trade_equity = trough_trade_equity = current_bar.equity;
    fractional_sells_triggered.clear();
}

void process_full_exit(const std::string& direction, const Config& config, BarData& current_bar, BarData& prev_bar, double& entry_price, double& cost_basis, double base_exit_price) {
    double pnl = 0.0;
    if (direction == "long") {
        double execution_price = get_price_with_slippage(base_exit_price, "sell", config);
        double proceeds = (current_bar.share_quantity * execution_price);
        current_bar.cash += (proceeds - config.commission_per_trade);
        if (cost_basis > 0) {
            pnl = proceeds - cost_basis;
        }
    } else if (direction == "short") {
        double execution_price = get_price_with_slippage(base_exit_price, "buy", config);
        double cost_to_cover = (current_bar.share_quantity * execution_price);
        current_bar.cash -= (cost_to_cover + config.commission_per_trade);
        if (cost_basis > 0) {
            pnl = cost_basis - cost_to_cover;
        }
    }

    current_bar.realized_pnl = prev_bar.realized_pnl + pnl;
    cost_basis = 0.0;
    current_bar.equity = current_bar.cash;
    current_bar.share_quantity = 0.0;
    current_bar.position_state = "flat";
    entry_price = 0.0;
}

void process_partial_exit(const std::string& direction, const Config& config, double base_exit_price, double fraction, BarData& current_bar, double& entry_price, double& cost_basis) {
    if (direction == "long") {
        double shares_to_sell = current_bar.share_quantity * fraction;

        if (shares_to_sell > 0 && current_bar.share_quantity >= shares_to_sell) {
            double execution_price = get_price_with_slippage(base_exit_price, "sell", config);
            
            double cost_of_sold_shares = (cost_basis / current_bar.share_quantity) * shares_to_sell;
            double pnl_from_sale = (shares_to_sell * execution_price) - cost_of_sold_shares;

            current_bar.realized_pnl += pnl_from_sale;
            current_bar.cash += (shares_to_sell * execution_price) - config.commission_per_trade;
            current_bar.share_quantity -= shares_to_sell;
            cost_basis -= cost_of_sold_shares;
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