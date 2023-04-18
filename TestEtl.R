library(tidyverse)
library(httr)
library(lubridate)
library(slider)
library(purrr)
library("DBI")
library(cowplot)

require(httr)

overwrite_file <- TRUE
ticker <- "DIA"
from_date <- "2023-01-01"
to_date <- "2023-02-28"
api_key <- Sys.getenv("FmpApiKey")
setwd("C:/Users/bawil/Documents/StockData/IntradayPrices")
file_name <- paste(ticker, "_15m.csv", col="", sep="")

datesTest <- seq(ymd(from_date), ymd(to_date), by="months")
cat(format(datesTest, usetz=TRUE, precision="ymdhms"))
datesTest2 <- seq(ymd(from_date)%m+%months(1), ymd(to_date) +1, by="months") -1
cat(format(datesTest2, usetz=TRUE))

urls <- map2(
  datesTest, 
  datesTest2, 
  ~ paste(
    "https://financialmodelingprep.com/api/v3/historical-chart/15min/",
    ticker,
    "?from=",format(.x),
    "&to=",format(.y),
    "&apikey=",api_key,
    col="",sep=""
  ))
cat(unlist(urls))

reqTibble <- tibble(
  startDt = datesTest,
  endDt = datesTest2,
  url = unlist(urls)
)
print(reqTibble)

if (overwrite_file)
{
  cat("overwrite_file:", overwrite_file, "\n")
  
  readr::write_csv(
    data.frame(
      date=POSIXct(),
      open=double(),
      low=double(),
      high=double(),
      close=double(),
      volume=double()
    ),
    file = file_name,
    append = FALSE,
    col_names = TRUE,
    quote = "none")
}

headers = c(
  `Upgrade-Insecure-Requests` = '1'
)

params = list(
  `datatype` = 'csv'
)

slide(
  reqTibble,
  .f = function(.x)
  {
    httr::GET(
      url = paste(
        "https://financialmodelingprep.com/api/v3/historical-chart/15min/",
        ticker,
        "?from=",format(.x$startDt),
        "&to=",format(.x$endDt),
        "&apikey=",api_key,
        col="",sep=""
      ),
      httr::add_headers(.headers=headers),
      query = params
    ) %>% 
      content(as = 'text') %>%
      readr::read_csv() %>% 
      as_tibble() %>% 
      filter(`%within%`(ymd_hms(date), interval(.x$startDt, .x$endDt+minutes(1439)))) %>% 
      arrange(ymd_hms(date)) %>% 
      readr::write_csv(
        file = file_name,
        append = TRUE,
        col_names = FALSE,
        quote = "none")
  })

# tsd <- ymd("2023-01-01")
# ted <- ymd("2023-01-31")
# df2 <- httr::GET(
#     url = paste(
#       "https://financialmodelingprep.com/api/v3/historical-chart/15min/",
#       ticker,
#       "?from=",format(tsd),
#       "&to=",format(ted),
#       "&apikey=",api_key,
#       col="",sep=""
#     ),
#     httr::add_headers(.headers=headers),
#     query = params
#   ) %>% 
#   content(as = 'text') %>%
#   readr::read_csv() %>% 
#   as_tibble() %>% 
#   filter(`%within%`(ymd_hms(date), interval(tsd, ted+minutes(1439)))) %>% 
#   arrange(ymd_hms(date))
# 
# head(df2)
# tail(df2)
# print(df2)
# typeof(df2$date)
# 
# print(typeof(df2$date))
# print(typeof(ymd("2023-01-01")))
# class(df2)
# sd <- ymd("2005-03-01")
# ed <- ymd("2005-03-31")+minutes(1439)
# df4 <- df2 %>%
#   filter(`%within%`(ymd_hms(date), interval(sd, ed)))
# print(df4)
# 
# td <- ymd_hms("2005-02-22 10:45:00")
# td1 <- ymd_hms("2005-03-31 16:00:00")
# `%within%`(td, interval(sd, ed))
#          
# df3 <- df2 %>%
#   select(date, close) %>%
#   mutate(c2 = close * 2)
# 
# print(df3)

#dt <- df2 %>%
#  mutate(d = as_date(date)) %>% 
#  select(d) %>% 
#  summarise(min = min(d),
#            max = max(d))
#dt <- df2 %>%
#  select(date) %>% 
#  summarise(min = min(date),
#            max = max(date))
#print(dt)

# ef2 <- as_tibble(ef)
# sd1 <- ymd("2005-04-01")
# ed1 <- ymd("2005-04-30")+minutes(1439)
# ef4 <- ef2 %>%
#   filter(`%within%`(ymd_hms(date), interval(sd1, ed1)))
# 
# #union df4 and ef4 and then order by date asc
# aggTbl <- df4 %>%
#   bind_rows(ef4) %>%
#   arrange(ymd_hms(date))

rf <- readr::read_csv(c(file.path(getwd(), file_name)))
head(rf)
rf1 <- rf %>% 
  mutate(dateonly = as_date(date)) %>% 
  mutate(wday = wday(date, label = TRUE, week_start = 1)) %>% 
  group_by(dateonly) %>% 
  arrange(date) %>% 
  mutate(bar_of_day = row_number()) %>%
  filter(bar_of_day < 27) %>% 
  mutate(day_high = max(high)) %>% 
  mutate(day_low = min(low)) %>% 
  mutate(day_open = first(open)) %>% 
  mutate(day_close = last(close)) %>% 
  mutate(day_half = if_else(bar_of_day <= 13, 1, 2)) %>% 
  ungroup()
head(rf1)

con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
duckdb::duckdb_register(con, "DIA_15M", rf)
drf2 <- tbl(con, "DIA_15M") %>% 
  filter(date > '2023-02-10') %>% 
  collect()

drf3 <- dbGetQuery(con, "SELECT * FROM DIA_15M WHERE date < '2023-02-10'")
dbExecute(con, "COPY DIA_15M TO 'DIA_15m.parquet' (FORMAT 'PARQUET', allow_overwrite TRUE)")
drf4 <- dbGetQuery(con, "SELECT * FROM DIA_15m.parquet")
dbDisconnect(con, shutdown=TRUE)

pudh <- ggplot(
  rf1 %>% 
    filter(high == day_high) %>% 
    filter(day_close > day_open),
  aes(x = bar_of_day)) +
  geom_histogram(bins = 26) +
  ggtitle("High Bar of Day", "Up Day")

pudl <- ggplot(
  rf1 %>% 
    filter(low == day_low) %>% 
    filter(day_close > day_open),
  aes(x = bar_of_day)) +
  geom_histogram(bins = 26) +
  ggtitle("Low Bar of Day", "Up Day")

pddh <- ggplot(
  rf1 %>% 
    filter(high == day_high) %>% 
    filter(day_close < day_open),
  aes(x = bar_of_day)) +
  geom_histogram(bins = 26) +
  ggtitle("High Bar of Day", "Down Day")

pddl <- ggplot(
  rf1 %>% 
    filter(low == day_low) %>% 
    filter(day_close < day_open),
  aes(x = bar_of_day)) +
  geom_histogram(bins = 26) +
  ggtitle("Low Bar of Day", "Down Day")

cowplot::plot_grid(
  pudh, pudl, pddh, pddl,
  labels = "AUTO", ncol = 2, align = "h"
)
