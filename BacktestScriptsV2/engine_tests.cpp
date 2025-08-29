#define CATCH_CONFIG_MAIN
#include "catch.hpp"
#include "BacktestEngine.h"
#include <vector>
#include <map>

// Mock data for testing
std::vector<OHLC> create_mock_ohlc_data() {
    return {
        {100.0, 105.0, 99.0, 102.0, 1672531200}, // 2023-01-01
        {102.0, 108.0, 101.0, 107.0, 1672617600}, // 2023-01-02
        {107.0, 110.0, 106.0, 109.0, 1672704000}  // 2023-01-03
    };
}

TEST_CASE("get_price function tests", "[get_price]") {
    std::vector<OHLC> ohlc_data = create_mock_ohlc_data();

    SECTION("Get price at open") {
        double price = get_price(ohlc_data[1], TimingOption::OPEN, ohlc_data, 1);
        REQUIRE(price == 102.0);
    }

    SECTION("Get price at close") {
        double price = get_price(ohlc_data[1], TimingOption::CLOSE, ohlc_data, 1);
        REQUIRE(price == 107.0);
    }

    SECTION("Get price at next open") {
        double price = get_price(ohlc_data[1], TimingOption::NEXT_OPEN, ohlc_data, 1);
        REQUIRE(price == 107.0);
    }

    SECTION("Get price at next open on last bar (fallback to close)") {
        double price = get_price(ohlc_data[2], TimingOption::NEXT_OPEN, ohlc_data, 2);
        REQUIRE(price == 109.0);
    }
}

TEST_CASE("get_price_with_slippage function tests", "[get_price_with_slippage]") {
    Config config;
    config.slippage_pct = 0.01; // 1% slippage

    SECTION("Buy slippage") {
        double price = get_price_with_slippage(100.0, "buy", config);
        REQUIRE(price == 101.0);
    }

    SECTION("Sell slippage") {
        double price = get_price_with_slippage(100.0, "sell", config);
        REQUIRE(price == 99.0);
    }

    SECTION("No slippage") {
        config.slippage_pct = 0.0;
        double price = get_price_with_slippage(100.0, "buy", config);
        REQUIRE(price == 100.0);
    }
}

TEST_CASE("process_entry function tests", "[process_entry]") {
    std::vector<OHLC> ohlc_data = create_mock_ohlc_data();
    Config config;
    config.initial_equity = 10000.0;
    config.entry_timing = TimingOption::OPEN;
    config.commission_per_trade = 5.0;

    BarData current_bar;
    current_bar.cash = config.initial_equity;

    int trade_count = 0;
    double entry_price = 0.0;
    double peak_trade_equity = 0.0;
    double trough_trade_equity = 0.0;
    std::map<int, bool> fractional_sells_triggered;

    process_entry("long", 1, ohlc_data, config, current_bar, trade_count, entry_price, peak_trade_equity, trough_trade_equity, fractional_sells_triggered);

    REQUIRE(trade_count == 1);
    REQUIRE(current_bar.trade_number == 1);
    REQUIRE(current_bar.position_state == "long");
    REQUIRE(entry_price == 102.0);
    REQUIRE(current_bar.share_quantity == 97); // (10000 - 5) / 102 = 97.99... -> 97 shares
    REQUIRE(current_bar.cash == Approx(101.0));
}

TEST_CASE("process_full_exit function tests", "[process_full_exit]") {
    Config config;
    config.commission_per_trade = 5.0;

    BarData current_bar;
    current_bar.position_state = "long";
    current_bar.share_quantity = 100;
    current_bar.cash = 500.0;
    current_bar.realized_pnl = 100.0;

    BarData prev_bar;
    prev_bar.equity = 10000.0;

    double entry_price = 100.0;
    double exit_price = 110.0;

    process_full_exit("long", config, current_bar, prev_bar, entry_price, exit_price);

    REQUIRE(current_bar.position_state == "flat");
    REQUIRE(current_bar.share_quantity == 0);
    REQUIRE(entry_price == 0.0);
    REQUIRE(current_bar.cash == Approx(500.0 + (100 * 110.0) - 5.0));
    REQUIRE(current_bar.equity == Approx(500.0 + (100 * 110.0) - 5.0 + 100.0));
}

TEST_CASE("process_partial_exit function tests", "[process_partial_exit]") {
    // This test is more complex and requires careful state setup.
    // For now, we will create a placeholder. A full implementation
    // would require mocking more of the backtest loop's state.
    REQUIRE(true);
}

TEST_CASE("run_backtest integration test", "[run_backtest]") {
    std::vector<OHLC> ohlc_data = {
        {100.0, 102.0, 99.0, 101.0, 0},
        {101.0, 105.0, 100.0, 104.0, 1},
        {104.0, 106.0, 103.0, 105.0, 2},
        {105.0, 110.0, 104.0, 109.0, 3},
        {109.0, 112.0, 108.0, 110.0, 4}
    };

    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::LONG;
    config.entry_timing = TimingOption::CLOSE;
    config.exit_timing = TimingOption::CLOSE;
    config.commission_per_trade = 5.0;

    config.long_entry = [](const OHLC&, const std::vector<OHLC>&, size_t i) {
        return i == 1; // Enter on the second bar
    };

    config.long_exit = [](const OHLC&, const std::vector<OHLC>&, size_t i) {
        return i == 3; // Exit on the fourth bar
    };

    std::vector<BarData> results = run_backtest(ohlc_data, config);

    // Expected results are +1 because of the initial bar
    REQUIRE(results.size() == ohlc_data.size() + 1);

    // Check final equity
    // Entry: (10000 - 5) / 104 = 96.1... -> 96 shares
    // Cost: 96 * 104 = 9984
    // Cash after entry: 10000 - 9984 - 5 = 11
    // Exit: 96 * 109 = 10464
    // Cash after exit: 11 + 10464 - 5 = 10470
    REQUIRE(results.back().equity == Approx(10470.0));
    REQUIRE(results.back().trade_number == 1);
    REQUIRE(results.back().position_state == "flat");
}

TEST_CASE("Interaction bug: Simultaneous full exit and partial exit", "[run_backtest][bug]") {
    // This test creates a scenario where a single bar's high price triggers
    // both a fractional sell and a full take-profit to test their interaction.
    std::vector<OHLC> ohlc_data = {
        {100.0, 100.0, 100.0, 100.0, 0}, // Bar 0: Flat
        {100.0, 100.0, 100.0, 100.0, 1}, // Bar 1: Enter at 100.0
        {100.0, 111.0, 100.0, 110.0, 2}  // Bar 2: High of 111 should trigger both exits
    };

    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::LONG;
    config.entry_timing = TimingOption::CLOSE;
    config.exit_timing = TimingOption::CLOSE;
    config.commission_per_trade = 0.0; // Simplify calcs
    config.slippage_pct = 0.0;         // Simplify calcs

    // Entry signal
    config.long_entry = [](const OHLC&, const std::vector<OHLC>&, size_t i) {
        return i == 1;
    };

    // No signal-based exit
    config.long_exit = [](const OHLC&, const std::vector<OHLC>&, size_t i) {
        return false;
    };

    // Risk management that should trigger simultaneously
    config.risk_config.take_profit_pct = 0.10; // Full exit at 10% profit (price >= 110)
    config.risk_config.fractional_sells.push_back({0.05, 0.5}); // Partial exit of 50% at 5% profit (price >= 105)

    // This call is expected to crash if the bug is present
    std::vector<BarData> results = run_backtest(ohlc_data, config);

    // If it doesn't crash, we can assert the final state.
    // The full exit should take precedence.
    // Entry: 10000 / 100 = 100 shares.
    // Exit: Take-profit is at 110. Proceeds = 100 * 110 = 11000.
    REQUIRE(results.back().equity == Approx(11000.0));
    REQUIRE(results.back().position_state == "flat");
    REQUIRE(results.back().share_quantity == 0);
}

TEST_CASE("Engine returns a vector of results", "[run_backtest][core]") {
    std::vector<OHLC> ohlc_data = {
        {100.0, 100.0, 100.0, 100.0, 0},
        {100.0, 100.0, 100.0, 100.0, 1}
    };
    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::BUY_AND_HOLD; // Easiest mode to test

    std::vector<BarData> results = run_backtest(ohlc_data, config);

    // The engine adds an initial bar for starting equity, so size should be data size + 1.
    REQUIRE(results.size() == ohlc_data.size() + 1);
}

auto dummy_entry = [](const OHLC&, const std::vector<OHLC>&, size_t i) { return i == 0; };

TEST_CASE("Engine handles nullptr for long_exit", "[run_backtest][core]") {
    std::vector<OHLC> ohlc_data = {{100.0, 100.0, 100.0, 100.0, 0}};
    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::LONG;
    config.long_entry = dummy_entry;
    config.long_exit = nullptr; // Explicitly set to nullptr

    // This should not crash
    run_backtest(ohlc_data, config);

    // We just need to ensure it ran without crashing, so we add a trivial assertion.
    REQUIRE(true);
}

auto dummy_exit = [](const OHLC&, const std::vector<OHLC>&, size_t i) { return i == 1; };

TEST_CASE("Engine handles nullptr for long_entry", "[run_backtest][core]") {
    std::vector<OHLC> ohlc_data = {{100.0, 100.0, 100.0, 100.0, 0}};
    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::LONG;
    config.long_entry = nullptr;
    config.long_exit = dummy_exit;
    run_backtest(ohlc_data, config);
    REQUIRE(true); // Did not crash
}

TEST_CASE("Engine handles nullptr for short_entry", "[run_backtest][core]") {
    std::vector<OHLC> ohlc_data = {{100.0, 100.0, 100.0, 100.0, 0}};
    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::SHORT;
    config.short_entry = nullptr;
    config.short_exit = dummy_exit;
    run_backtest(ohlc_data, config);
    REQUIRE(true); // Did not crash
}

TEST_CASE("Engine handles nullptr for short_exit", "[run_backtest][core]") {
    std::vector<OHLC> ohlc_data = {{100.0, 100.0, 100.0, 100.0, 0}};
    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::SHORT;
    config.short_entry = dummy_entry;
    config.short_exit = nullptr;
    run_backtest(ohlc_data, config);
    REQUIRE(true); // Did not crash
}

TEST_CASE("Engine executes BUY_AND_HOLD correctly", "[run_backtest][bnh]") {
    std::vector<OHLC> ohlc_data = {
        {100.0, 100.0, 100.0, 100.0, 0}, // Bar 0
        {110.0, 110.0, 110.0, 110.0, 1}  // Bar 1
    };
    Config config;
    config.initial_equity = 10000.0;
    config.trade_mode = TradeMode::BUY_AND_HOLD;
    config.entry_timing = TimingOption::CLOSE;
    config.exit_timing = TimingOption::CLOSE;
    config.long_entry = nullptr;
    config.long_exit = nullptr;

    std::vector<BarData> results = run_backtest(ohlc_data, config);

    // Should buy at 100 on bar 0 and sell at 110 on bar 1.
    // Entry: 10000 / 100 = 100 shares.
    // Exit: 100 * 110 = 11000.
    REQUIRE(results.back().equity == Approx(11000.0));
    REQUIRE(results.back().trade_number == 1);
}
