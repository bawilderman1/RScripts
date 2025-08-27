#ifndef BACKTEST_ENGINE_H
#define BACKTEST_ENGINE_H

#include <vector>
#include <string>
#include <functional>
#include <map>

// =================================================================================
// DATA STRUCTURES
// =================================================================================

struct OHLC {
    double open;
    double high;
    double low;
    double close;
    long long timestamp;
};

struct FractionalSellRule {
    double profit_target_pct;
    double fraction_to_sell;
};

struct RiskManagementConfig {
    double stop_loss_pct = 0.0;
    double take_profit_pct = 0.0;
    std::vector<FractionalSellRule> fractional_sells;
};

enum class TradeMode { LONG, SHORT, LONG_SHORT, BUY_AND_HOLD };

enum class TimingOption { OPEN, CLOSE, NEXT_OPEN };

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
// FUNCTION DECLARATIONS
// =================================================================================

void process_entry(const std::string& direction, size_t i, const std::vector<OHLC>& ohlc_data, const Config& config, BarData& current_bar, int& trade_count, double& entry_price, double& peak_trade_equity, double& trough_trade_equity, std::map<int, bool>& fractional_sells_triggered);
void process_full_exit(const std::string& direction, const Config& config, BarData& current_bar, BarData& prev_bar, double& entry_price, double base_exit_price);
void process_partial_exit(const std::string& direction, const Config& config, double base_exit_price, double fraction, BarData& current_bar, double& entry_price);
double get_price(const OHLC& bar, TimingOption timing, const std::vector<OHLC>& ohlc_data, size_t current_index);
double get_price_with_slippage(double price, const std::string& direction, const Config& config);

std::vector<BarData> run_backtest(const std::vector<OHLC>& ohlc_data, Config config);

#endif // BACKTEST_ENGINE_H
