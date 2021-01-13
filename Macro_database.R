library(tidyquant)
library(tidyverse)
library(Rblpapi)
library(xgboost)
library(RQuantLib)
library(Matrix)
library(writexl)
library(RSQLite)
library(WindR)
library(reticulate)


# 1. Setup -------------------------------------------------------------------
blpConnect()
w.start()
end_date <- today() - 1
end_date <- as.Date("2021-01-05")
start_date_WTD <- end_date - days(7)
start_date_MTD <- floor_date(end_date, "month") - 1
start_date_YTD <- floor_date(end_date, "year") - 1

blp_flds <- c("PX_LAST", "FUND_FLOW", "YIELD_TO_WORST", "OAS_SOVEREIGN_CURVE", 
              "Modified_Duration") 

wind_flds <- c("close", "ytm_b", "duration")

con <- dbConnect(RSQLite::SQLite(), "Macro_database.sqlite")

opt <- structure(c("PREVIOUS_VALUE", 
                   "ALL_CALENDAR_DAYS"),
                 names = c("nonTradingDayFillMethod", 
                           "nonTradingDayFillOption"))


getdata <- function(ticker, key , date01, date02) {
  
  opt <- structure(c("PREVIOUS_VALUE", 
                     "ALL_CALENDAR_DAYS"),
                   names = c("nonTradingDayFillMethod", 
                             "nonTradingDayFillOption"))
  
  data0 <- bdh(ticker, key, as.Date(date01), 
               as.Date(date02), options = opt)
  
  data1 <- mapply(function(x, y) {y <- y %>% mutate(ID = x) },
                  names(data0), data0, 
                  USE.NAMES = FALSE, SIMPLIFY = FALSE)
  
  data2 <- as_tibble(do.call("bind_rows", data1))
  
  return(data2)
}
# 
# # fields = c("close", "ytm_b", "duration")
# getdata_wind <- function(tickers, fields, date_start, date_end) {
#   l = c()
#   for (ticker in tickers) {
#     df <- w.wsd(ticker, fields, date_start, end_date, 
#                             "returnType=1;Days=Alldays;Fill=Previous")[[2]] %>%
#       mutate(ID = ticker)
#     l[[ticker]] = df
#   }
#   result <- bind_rows(l)
#   return(result)
# }


getdata_wind <- function(tickers, fields, start_date, end_date){
  l = c()
  for (field in fields) {
    data <- w.wsd(tickers, field, start_date, end_date,
                  "returnType=1;Days=Alldays;Fill=Previous")$Data
    data1 <- data %>%
      pivot_longer(cols = 2:dim(data)[2], names_to = "ID", values_to = field)
    l[[field]] = data1
  }
  if (length(l) > 1){
    l1 <- fields[1]
    l2 <- fields[2:length(l)]
    result <- l[[l1]]
    for (field in l2) {
      result <- result %>% left_join(l[[field]])
    }
  } else {
    l1 <- fields[1]
    result <- l[[l1]]
  }
  result <- result %>%
    rename(date = DATETIME)
  return(result)
}


# 2. update database ------------------------------------------------------

# field_b = c("PX_LAST")
# field_w = c("close")
# write index_price
writedb1 <- function (con, field_b, field_w, start_date, end_date) {
  tickers <- dbReadTable(con, "Region") %>%
    filter(Source == "Bloomberg")
  index_blp <- getdata(tickers$ID, field_b, start_date, end_date)
  index_blp <- index_blp %>%
    mutate(date = as.character(date)) %>%
    relocate(date, ID)
  
  tickers_wind <- dbReadTable(con, "Region") %>%
    filter(Source == "Wind")
  index_wind <- getdata_wind(tickers_wind$ID, field_w, start_date, end_date)
  index_wind <- index_wind %>%
    rename(PX_LAST = close) %>%
    mutate(date = as.character(date))

  result <- bind_rows(index_blp, index_wind)
  dbWriteTable(con, "Index_price", result)
}


updatedb1 <- function (con, field_b, field_w, start_date, end_date) {
  start_date <- dbGetQuery(con, "SELECT MAX(date) FROM Index_price")[['MAX(date)']]
  tickers <- dbReadTable(con, "Region") %>%
    filter(Source == "Bloomberg")
  index_blp <- getdata(tickers$ID, field_b, start_date, end_date)
  index_blp <- index_blp %>%
    mutate(date = as.character(date)) %>%
    relocate(date, ID) %>%
    filter(date > start_date)
  
  tickers_wind <- dbReadTable(con, "Region") %>%
    filter(Source == "Wind")
  index_wind <- getdata_wind(tickers_wind$ID, field_w, start_date, end_date)
  index_wind <- index_wind %>%
    rename(PX_LAST = close) %>%
    mutate(date = as.character(date)) %>%
    filter(date > start_date)
  
  result <- bind_rows(index_blp, index_wind)
  dbWriteTable(con, "Index_price", result, append = TRUE)
}


# field_b = blp_flds[3:5]
# field_w = wind_flds[2:3]
# write index_atr
writedb2 <- function (con, field_b, field_w, start_date, end_date) {
  tickers <- dbReadTable(con, "Region") %>%
    filter(Source == "Bloomberg", Type == "Credit")
  index_blp <- getdata(tickers$ID, field_b, start_date, end_date)
  index_blp <- index_blp %>%
    mutate(date = as.character(date)) %>%
    relocate(date, ID)
  
  tickers_wind <- dbReadTable(con, "Region") %>%
    filter(Source == "Wind", Type == "Credit")
  index_wind <- getdata_wind(tickers_wind$ID, field_w, start_date, end_date)
  index_wind <- index_wind %>%
    mutate(date = as.character(date))
  
  dbWriteTable(con, "Index_atr_blp", index_blp)
  dbWriteTable(con, "Index_atr_wind", index_wind)
}


updatedb2 <- function (con, field_b, field_w, start_date, end_date) {
  start_date <- dbGetQuery(con, "SELECT MAX(date) FROM Index_atr_blp")[['MAX(date)']]
  tickers <- dbReadTable(con, "Region") %>%
    filter(Source == "Bloomberg", Type == "Credit")
  index_blp <- getdata(tickers$ID, field_b, start_date, end_date)
  index_blp <- index_blp %>%
    mutate(date = as.character(date)) %>%
    relocate(date, ID) %>%
    filter(date > start_date)
  
  start_date <- dbGetQuery(con, "SELECT MAX(date) FROM Index_atr_wind")[['MAX(date)']]
  tickers_wind <- dbReadTable(con, "Region") %>%
    filter(Source == "Wind", Type == "Credit")
  index_wind <- getdata_wind(tickers_wind$ID, field_w, start_date, end_date)
  index_wind <- index_wind %>%
    mutate(date = as.character(date)) %>%
    filter(date > start_date)
  
  dbWriteTable(con, "Index_atr_blp", index_blp, append = TRUE)
  dbWriteTable(con, "Index_atr_wind", index_wind, append = TRUE)
}

# field_b = blp_flds[2]
# write fund_flow_etf
writedb3 <- function (con, field_b, start_date, end_date) {
  tickers <- dbReadTable(con, "Region") %>%
    filter(Source == "Bloomberg", Type == "ETF")
  
  data <- getdata(tickers$ID, c("FUND_FLOW", "PX_LAST"), "2019-01-01", end_date) %>%
    mutate(date = as.character(date)) %>%
    relocate(date, ID)
  
  dbWriteTable(con, "FUND_FLOW_ETF", data, overwrite = TRUE)
}


updatedb3 <- function (con, field_b, start_date, end_date) {
  start_date <- dbGetQuery(con, "SELECT MAX(date) FROM FUND_FLOW_ETF")[['MAX(date)']]
  
  tickers <- dbReadTable(con, "Region") %>%
    filter(Source == "Bloomberg", Type == "ETF")
  
  data <- getdata(tickers$ID, field_b, start_date, end_date) %>%
    mutate(date = as.character(date)) %>%
    relocate(date, ID) %>%
    filter(date > start_date)
  
  dbWriteTable(con, "FUND_FLOW_ETF", data, append = TRUE)
}


# Main function
# 重写数据库
writedb1(con, c("PX_LAST"), c("close"), start_date, end_date)
writedb2(con, blp_flds[3:5], wind_flds[2:3], start_date, end_date)
writedb3(con, blp_flds[2], start_date, end_date)
py_run_file("PyScripts/Extract_CICC_CF.py")


# 更新数据库
updatedb1(con, c("PX_LAST"), c("close"), start_date, end_date)
updatedb2(con, blp_flds[3:5], wind_flds[2:3], start_date, end_date)
updatedb3(con, blp_flds[2], start_date, end_date)
py_run_file("PyScripts/Update_CICC_CF.py")

dbDisconnect(con)

