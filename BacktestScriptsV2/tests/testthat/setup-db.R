library(DBI)
library(duckdb)

# This function creates and populates a temporary database file for testing.
create_test_db <- function(db_file) {
  # Clean up old db file if it exists
  if (file.exists(db_file)) {
    file.remove(db_file)
  }
  
  con <- dbConnect(duckdb::duckdb(), dbdir = db_file)

  # Create a sample daily price table
  # Data spans 3 weeks. Note that Fri, Jan 10 is missing (holiday).
  dbExecute(con, "
    CREATE TABLE spy_daily_test (
      dt DATE,
      open DOUBLE,
      high DOUBLE,
      low DOUBLE,
      close DOUBLE
    );
  ")
  dbExecute(con, "
    INSERT INTO spy_daily_test VALUES
      ('2025-01-01', 100.0, 101.0, 99.0, 100.5), -- Wed
      ('2025-01-02', 100.5, 102.0, 100.0, 101.5), -- Thu
      ('2025-01-03', 101.5, 103.0, 101.0, 102.5), -- Fri
      ('2025-01-06', 102.5, 104.0, 102.0, 103.5), -- Mon
      ('2025-01-07', 103.5, 105.0, 103.0, 104.5), -- Tue
      ('2025-01-08', 104.5, 106.0, 104.0, 105.5), -- Wed
      ('2025-01-09', 105.5, 107.0, 105.0, 106.5), -- Thu (Last day of this week)
      ('2025-01-13', 106.5, 108.0, 106.0, 107.5), -- Mon
      ('2025-01-14', 107.5, 109.0, 107.0, 108.5)  -- Tue
  ")

  # Create a sample dividend table
  dbExecute(con, "
    CREATE TABLE dividends_test (
      dt DATE,
      dividend DOUBLE
    );
  ")
  dbExecute(con, "
    INSERT INTO dividends_test VALUES
      ('2025-01-07', 0.50), -- Belongs to the week ending Jan 09
      ('2025-01-08', 0.25)  -- Also belongs to the week ending Jan 09
  ")

  # Disconnect from the database
  dbDisconnect(con, shutdown = TRUE)
}