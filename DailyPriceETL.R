library(tidyverse)
library(httr)
library(lubridate)

require(httr)

ticker <- "DIA"
from_date <- "2000-01-01"
to_date <- "2022-12-31"
api_key <- Sys.getenv("FmpApiKey")
setwd("C:/Users/bawil/Documents/StockData/DailyPrices")

file_name <- paste(ticker, "_daily.csv", col="", sep="")
#cat("File Name:", file_name, "\n\n")

url_string <- paste(
  "https://financialmodelingprep.com/api/v3/historical-price-full/",
  ticker,
  "?from=",from_date,
  "&to=",to_date,
  "&apikey=",api_key,
  col="",sep="")
#cat("URL String:", url_string, "\n\n")

headers = c(
  `Upgrade-Insecure-Requests` = '1'
)

params = list(
  `datatype` = 'csv'
)

res <- httr::GET(url = url_string, httr::add_headers(.headers=headers), query = params)
df <- read.csv(text=content(res, 'text'))

readr::write_csv(
  x = df,
  file = file_name,
  append = FALSE,
  col_names = TRUE,
  quote = "none")