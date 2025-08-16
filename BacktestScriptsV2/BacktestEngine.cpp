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
 * @param open The opening price of the period.
 * @param high The highest price of the period.
 * @param low The lowest price of the period.
 * @param close The closing price of the period.
 * @param timestamp A Unix timestamp representing the start of the bar's period.
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
 * @param profit_target_pct The profit percentage (e.g., 0.10 for 10%) that triggers the sale.
 * @param fraction_to_sell The fraction of the original position to sell (e.g., 0.50 for 50%).
 */
struct FractionalSellRule {
    double profit_target_pct;
    double fraction_to_sell;
};

/**
 * @struct RiskManagementConfig
 * @brief Holds all parameters related to risk and trade management.
 * @param stop_loss_pct The percentage loss from the entry price that triggers a full exit (e.g., 0.05 for 5%). 0.0 to disable.
 * @param take_profit_pct The percentage profit from the entry price that triggers a full exit (e.g., 0.20 for 20%). 0.0 to disable.
 * @param fractional_sells A vector of FractionalSellRule structs to define partial profit-taking rules.
 */
struct RiskManagementConfig {
    double stop_loss_pct = 0.0;
    double take_profit_pct = 0.0;
    std::vector<FractionalSellRule> fractional_sells;
};

/**
 * @enum TradeMode
 * @brief Defines the allowed trading directions for the strategy.
 * @value LONG Only long trades are permitted.
 * @value SHORT Only short trades are permitted (not fully implemented in this example).
 * @value LONG_SHORT Both long and short trades are permitted.
 * @value BUY_AND_HOLD A special mode for benchmarking; buys on the first bar and sells on the last.
 */
enum class TradeMode { LONG, SHORT, LONG_SHORT, BUY_AND_HOLD };

/**
 * @enum TimingOption
 * @brief Specifies at which point in a bar a trade should be executed.
 * @value OPEN Execute at the opening price of the bar.
 * @value CLOSE Execute at the closing price of the bar.
 * @value NEXT_OPEN Execute at the opening price of the *next* bar.
 */
enum class TimingOption { OPEN, CLOSE, NEXT_OPEN };

/**
 * @struct Config
 * @brief Holds all configuration parameters for a single backtest run.
 * @param initial_equity The starting cash balance for the backtest.
 * @param trade_mode The trading direction rules (see TradeMode enum).
 * @param time_frame A string representing the OHLC data's time frame (e.g., "1d", "1w", "1mo"). Crucial for dividend processing.
 * @param entry_timing The execution timing for entry trades (see TimingOption enum).
 * @param exit_timing The execution timing for signal-based exits (see TimingOption enum).
 * @param risk_config A struct containing all risk management rules.
 * @param slippage_pct The percentage cost applied to trades to simulate price slippage (e.g., 0.0005 for 0.05%).
 * @param commission_per_trade A flat fee applied to every transaction (entry, full exit, partial exit).
 * @param dividend_data An optional map of [timestamp -> dividend_per_share] to simulate total returns. The timestamp must be the ex-dividend date.
 * @param long_entry A function that returns true if a long entry signal occurs on the current bar.
 * @param long_exit A function that returns true if a long exit signal occurs on the current bar.
 * @param short_entry A function that returns true if a short entry signal occurs on the current bar.
 * @param short_exit A function that returns true if a short exit signal occurs on the current bar.
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
 * @param trade_number The unique identifier for the current trade. Increments with each new entry.
 * @param position_state The current state of the position: "long", "short", or "flat".
 * @param share_quantity The number of shares currently held.
 * @param equity The total value of the portfolio (cash + value of held shares).
 * @param cash The amount of cash on hand.
 * @param ongoing_pnl The unrealized profit or loss of the currently open position.
 * @param realized_pnl The profit that has been "locked in" from partial sells during the current open trade.
 * @param pnl_log_change_pct The logarithmic percentage change in equity after a trade is fully closed.
 * @param equity_drawdown The percentage drop from the portfolio's peak equity to the current equity.
 * @param equity_drawup The percentage rise from the portfolio's trough equity to the current equity.
 * @param trade_drawdown The percentage drop from the current trade's peak value to its current value.
 * @param trade_drawup The percentage rise from the current trade's trough value to its current value.
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
void print_header();
void print_bar_data(const BarData& data, const OHLC& ohlc);
bool example_long_entry(const OHLC& bar, const std::vector<OHLC>& ohlc_data, size_t i);
bool example_long_exit(const OHLC& bar, const std::vector<OHLC>& ohlc_data, size_t i);


// =================================================================================
// MAIN BACKTESTING ENGINE
// =================================================================================

/**
 * @brief The main function that orchestrates the entire backtest simulation.
 * @param ohlc_data A vector of OHLC structs representing the historical price data.
 * @param config A Config struct containing all parameters for the simulation.
 * @output Prints a bar-by-bar log of the portfolio's state to the console.
 */
void run_backtest(const std::vector<OHLC>& ohlc_data, const Config& config) {
    if (ohlc_data.empty()) { std::cout << "OHLC data is empty." << std::endl; return; }

    std::vector<BarData> results;
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

    print_header();

    for (size_t i = 0; i < ohlc_data.size(); ++i) {
        BarData prev_bar = results.back();
        BarData current_bar = prev_bar;
        const OHLC& bar = ohlc_data[i];
        bool position_exited_this_bar = false;

        // --- 1. PROCESS POINT-IN-TIME EVENTS (DIVIDENDS) ---
        // This time-aware module runs before any trade logic.
        if (!config.dividend_data.empty() && current_bar.position_state == "long") {
            if (config.time_frame == "1d") {
                // Precise Path: For daily data, check for an ex-dividend date match.
                auto it = config.dividend_data.find(bar.timestamp);
                if (it != config.dividend_data.end()) {
                    current_bar.cash += (it->second * current_bar.share_quantity);
                }
            } else {
                // Aggregation Path: For weekly/monthly data, check for ex-dates within the bar's time range.
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
            // A. Check for risk-based exits (Stop-Loss, Take-Profit)
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
            // B. Check for fractional sells
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
            // C. Check for signal-based exits
            if (!position_exited_this_bar && config.long_exit(bar, ohlc_data, i)) {
                double base_exit_price = get_price(bar, config.exit_timing, ohlc_data, i);
                process_full_exit("long", config, current_bar, prev_bar, entry_price, base_exit_price);
                position_exited_this_bar = true;
            }
        }

        // --- 3. PROCESS ENTRIES IF POSITION IS FLAT ---
        if (current_bar.position_state == "flat") {
            if ((config.trade_mode == TradeMode::LONG || config.trade_mode == TradeMode::LONG_SHORT) && config.long_entry(bar, ohlc_data, i)) {
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
        print_bar_data(current_bar, bar);
    }
}

// =================================================================================
// HELPER FUNCTION IMPLEMENTATIONS
// =================================================================================

/**
 * @brief Processes a new trade entry, calculating costs and updating portfolio state.
 * @param direction The direction of the trade ("long" or "short").
 * @param i The current index in the ohlc_data vector.
 * @param ohlc_data The full history of price data.
 * @param config The backtest configuration.
 * @param current_bar The BarData struct for the current bar, which will be modified.
 * @param trade_count A reference to the global trade counter.
 * @param entry_price A reference to the variable holding the current trade's entry price.
 * @param peak_trade_equity A reference to the variable tracking the peak equity of the current trade.
 * @param trough_trade_equity A reference to the variable tracking the trough equity of the current trade.
 * @param fractional_sells_triggered A map to track which fractional sell rules have been executed for this trade.
 */
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

/**
 * @brief Processes a full exit from a position, calculating costs and updating portfolio state.
 * @param direction The direction of the trade being exited ("long" or "short").
 * @param config The backtest configuration.
 * @param current_bar The BarData struct for the current bar, which will be modified.
 * @param prev_bar The BarData struct from the previous bar, for calculating log returns.
 * @param entry_price A reference to the current trade's entry price, which will be reset.
 * @param base_exit_price The ideal exit price before applying slippage.
 */
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

/**
 * @brief Processes a partial exit (fractional sell), calculating costs and updating portfolio state.
 * @param direction The direction of the trade being partially exited ("long" or "short").
 * @param config The backtest configuration.
 * @param base_exit_price The ideal exit price before applying slippage.
 * @param fraction The fraction of the original position to sell.
 * @param current_bar The BarData struct for the current bar, which will be modified.
 * @param entry_price The entry price of the current trade.
 */
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

/**
 * @brief Retrieves the execution price from a bar based on the timing option.
 * @param bar The current OHLC bar.
 * @param timing The TimingOption (OPEN, CLOSE, NEXT_OPEN).
 * @param ohlc_data The full history of price data (for NEXT_OPEN).
 * @param current_index The current index in the ohlc_data vector.
 * @return The appropriate execution price.
 */
double get_price(const OHLC& bar, TimingOption timing, const std::vector<OHLC>& ohlc_data, size_t current_index) {
    switch (timing) { case TimingOption::OPEN: return bar.open; case TimingOption::CLOSE: return bar.close; case TimingOption::NEXT_OPEN: if (current_index + 1 < ohlc_data.size()) return ohlc_data[current_index + 1].open; return bar.close; } return bar.close;
}

/**
 * @brief Applies slippage to a price based on the trade direction.
 * @param price The ideal execution price.
 * @param direction The direction of the trade ("buy" or "sell").
 * @param config The backtest configuration.
 * @return The adjusted price after applying slippage.
 */
double get_price_with_slippage(double price, const std::string& direction, const Config& config) {
    if (direction == "buy") return price * (1.0 + config.slippage_pct);
    if (direction == "sell") return price * (1.0 - config.slippage_pct);
    return price;
}

/**
 * @brief A simple example signal function for long entry.
 * @return True if the current close is higher than the previous close.
 */
bool example_long_entry(const OHLC& bar, const std::vector<OHLC>& ohlc_data, size_t i) { if (i < 1) return false; return bar.close > ohlc_data[i-1].close; }

/**
 * @brief A simple example signal function for long exit.
 * @return True if the current close is lower than the previous close.
 */
bool example_long_exit(const OHLC& bar, const std::vector<OHLC>& ohlc_data, size_t i) { if (i < 1) return false; return bar.close < ohlc_data[i-1].close; }

/**
 * @brief Prints the header for the output log.
 */
void print_header() { std::cout << std::left << std::setw(5) << "Bar" << std::setw(10) << "Close" << std::setw(7) << "Trade#" << std::setw(10) << "State" << std::setw(10) << "Shares" << std::setw(12) << "Equity" << std::setw(12) << "Cash" << std::setw(12) << "OngoingP&L" << std::setw(14) << "RealizedP&L" << std::setw(15) << "P&L Log(%)" << std::setw(15) << "Eq. Drawdown" << std::setw(15) << "Eq. Drawup" << std::setw(15) << "Tr. Drawdown" << std::setw(15) << "Tr. Drawup" << std::endl; }

/**
 * @brief Prints a single row of the output log for a given bar.
 */
void print_bar_data(const BarData& data, const OHLC& ohlc) { static int bar_count = 0; std::cout << std::fixed << std::setprecision(2); std::cout << std::left << std::setw(5) << bar_count++ << std::setw(10) << ohlc.close << std::setw(7) << data.trade_number << std::setw(10) << data.position_state << std::setw(10) << data.share_quantity << std::setw(12) << data.equity << std::setw(12) << data.cash << std::setw(12) << data.ongoing_pnl << std::setw(14) << data.realized_pnl << std::setw(15) << data.pnl_log_change_pct * 100.0 << std::setw(15) << data.equity_drawdown * 100.0 << std::setw(15) << data.equity_drawup * 100.0 << std::setw(15) << data.trade_drawdown * 100.0 << std::setw(15) << data.trade_drawup * 100.0 << std::endl; }


// =================================================================================
// MAIN FUNCTION
// =================================================================================

int main() {
    std::cout << "--- Running Backtest with Advanced Risk Management & Costs ---" << std::endl;

    std::vector<OHLC> ohlc_data = {
        {100.0, 102.0, 99.0, 101.0, 1678886400},   // Bar 0: Enter long
        {101.5, 105.0, 101.0, 104.0, 1678972800},  // Bar 1: Price rises
        {104.2, 112.0, 104.0, 111.0, 1679059200},  // Bar 2: Price spikes to 112, triggers 10% partial sell
        {110.0, 111.0, 108.0, 109.0, 1679145600},  // Bar 3: Price holds
        {108.8, 109.5, 95.0,  96.0,  1679232000},  // Bar 4: Price drops, triggers stop-loss
        {96.2,  98.0,  95.5,  97.0,  1679318400},  // Bar 5: Flat
    };

    Config config = {
        100000.0,
        TradeMode::LONG,
        "1d",
        TimingOption::CLOSE,
        TimingOption::CLOSE,
        { 0.05, 0.0, { {0.10, 0.50} } }, // Risk: 5% SL, no TP, sell 50% at 10% profit
        0.0005, // 0.05% slippage
        1.00,   // $1.00 commission per trade
        {},     // No dividend data for this run
        example_long_entry,
        example_long_exit,
        nullptr,
        nullptr
    };

    run_backtest(ohlc_data, config);

    return 0;
}