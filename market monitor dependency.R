library(tidyverse)
library(RSQLite)
library(DBI)
library(lubridate)
library(RQuantLib)
library(psych)
library(YieldCurve)
library(highcharter)
library(scatterD3)
library(DT)
library(formattable)
library(knitr)
library(kableExtra)
library(treemap)
library(d3treeR)


# 1. Transfer the database -------------------------

get_month_end <- function (df_dates, calendar) {
  months_end <- df_dates %>%
    separate(date, into = c("year", "month", "day"), sep = "-", remove = FALSE) %>%
    group_by(year, month) %>%
    arrange(year, month, day) %>%
    top_n(1, date) %>%
    filter(isEndOfMonth(calendar, ymd(date))) %>%
    ungroup() %>%
    select(date)
  return(months_end)
}


write_newcnusd <- function(con1, con2) {
  hist_data <- dbReadTable(con1, "hist_data") %>%
    arrange(date, ID) %>%
    mutate(price_dirty = price,
           year = year(ymd(date)),
           month = month(ymd(date)))
  df_dates <- dbGetQuery(con1, "SELECT DISTINCT date FROM hist_data") %>%
    arrange(date)
  bond_info <- dbReadTable(con1, "Info")
  cat <- dbReadTable(con1, "ID")
  Label <- dbReadTable(con1, "cat")
  mapping_credit2 <- dbReadTable(con1, "mapping_credit2")
  
  bonds_info <- hist_data %>%
    filter(date %in% get_month_end(df_dates, "HongKong")$date) %>%
    left_join(bond_info, by = 'ID') %>%
    left_join(mapping_credit2, by = 'Credit') %>%
    rename(rating2 = RTG2, Label = cat1) %>%
    arrange(date, ID)
  
  dbWriteTable(con2, "hist_data", hist_data, overwrite = TRUE)
  dbWriteTable(con2, "bonds_info", bonds_info, overwrite = TRUE)
  dbWriteTable(con2, "cat", cat, overwrite = TRUE)
  dbWriteTable(con2, "Label", Label, overwrite = TRUE)
  dbWriteTable(con2, "mapping_credit2", mapping_credit2, overwrite = TRUE)
}


# con1 hou's database, con2 new database
update_newcnusd <- function(con1, con2) {
  cat <- dbReadTable(con1, "ID")
  dbWriteTable(con2, "cat", cat, overwrite = TRUE)
  Label <- dbReadTable(con1, "cat")
  dbWriteTable(con2, "Label", Label, overwrite = TRUE)
  mapping_credit2 <- dbReadTable(con1, "mapping_credit2")
  dbWriteTable(con2, "mapping_credit2", mapping_credit2, overwrite = TRUE)
  
  dates_1 <- dbGetQuery(con1, "SELECT DISTINCT date FROM hist_data") %>%
    arrange(date)
  dates_2 <- dbGetQuery(con2, "SELECT DISTINCT date FROM hist_data") %>%
    arrange(date)
  dates_new <- setdiff(dates_1, dates_2)
  hist_data <- dbGetQuery(con1, sprintf("SELECT * FROM hist_data WHERE date >= '%s'", min(dates_new$date))) %>%
    arrange(date, ID) %>%
    mutate(price_dirty = price,
           year = year(ymd(date)),
           month = month(ymd(date)))
  dbWriteTable(con2, "hist_data", hist_data, append = TRUE)
  
  bond_info <- dbReadTable(con1, "Info")
  months_new <- get_month_end(dates_new, "HongKong")$date
  bonds_info <- hist_data %>%
    filter(date %in% months_new) %>%
    left_join(bond_info, by = 'ID') %>%
    left_join(mapping_credit2, by = 'Credit') %>%
    rename(rating2 = RTG2, Label = cat1) %>%
    arrange(date, ID)
  dbWriteTable(con2, "bonds_info", bonds_info, append = TRUE)
}


transfer_database <- function(con1, con2) {
  if (dbExistsTable(con2, "hist_data")) {update_newcnusd(con1, con2)
  } else {write_newcnusd(con1, con2)}
}



# 2. Calculate Index ------------------------------------------------------

# 此处group1为date，group2为cat，传入bonds_info的月数据
tidy_bonds <- function(df, group) {
  tidy_df <- df %>%
    filter(!is.na(price * price_dirty) & date < END_DT & 
             duration >= 0.3 & YTM > 0 & spread > 0 &
             AMT_OUTSTANDING > 0 & !is.na(!!sym(group))) %>%
    group_by(date) %>%
    filter(YTM <= quantile(YTM, 0.98, na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(date, !!sym(group)) %>%
    filter(n() > 1) %>%
    ungroup()
  tidy_df
}


#df = bonds_tidy
#group = "cat"
# df为经过筛选后的bonds_info
cal_weight <- function(df, group) {
  df_weights <- df %>%
    group_by(date, !!sym(group)) %>%
    dplyr::mutate(weight_wt = price_dirty * AMT_OUTSTANDING / sum(price_dirty * AMT_OUTSTANDING),
           weight_eq = 1 / n(),
           year = ifelse(month(ymd(date)) == 12, year(ymd(date)) + 1, year(ymd(date))),
           month = ifelse(month(ymd(date)) == 12, 1, month(ymd(date)) + 1),
           Amount = AMT_OUTSTANDING / 100000000) %>%
    ungroup() %>%
    select(date, year, month, ID, !!sym(group), price, price_dirty, CPN, Amount, weight_wt, weight_eq) %>%
    arrange(year, month, !!sym(group)) %>%
    relocate(date, year, month)
  df_weights
}


# cal_weight是计算cat，Label，rating等大类指数，cal_weight1是计算cat分类下的细分指数
cal_weight1 <- function(bonds_info, cat1, cat2) {
  df_weight <- bonds_info %>%
    filter(cat == cat1) %>%
    group_by(date, rating2) %>%
    mutate(weight_wt = price_dirty * AMT_OUTSTANDING / sum(price_dirty * AMT_OUTSTANDING),
           weight_eq = 1 / n(),
           year = ifelse(month(ymd(date)) == 12, year(ymd(date)) + 1, year(ymd(date))),
           month = ifelse(month(ymd(date)) == 12, 1, month(ymd(date)) + 1),
           Amount = AMT_OUTSTANDING / 1000000) %>%
    ungroup() %>%
    select(date, year, month, ID, cat, !!sym(cat2), price, price_dirty, CPN, Amount, weight_wt, weight_eq) %>%
    arrange(year, month, !!sym(cat2)) %>%
    relocate(date, year, month)
  df_weight
}


#weights = df_weight
#group = "rating2"
#method = "weight_wt"
#
cal_index <- function(hist_data, weights, group, method="weight_wt") {
  weights <- select(weights, -price_dirty)
  
  df <- left_join(hist_data, weights, 
                  by = c("year", "month", "ID")) %>%
    na.omit() %>%
    mutate(ACI = CPN * yearFraction(ymd(date.y), ymd(date.x), rep(6, dim(.)[1])),
           price_return = (price.x - price.y) / price_dirty,
           total_return = (price.x - price.y + ACI) / price_dirty) %>%
    group_by(!!sym(group), date.x) %>%
    summarise(price_return_i = sum(!!sym(method) * price_return),
              total_return_i = sum(!!sym(method) * total_return),
              YTM_i = sum(!!sym(method) * YTM),
              spread_i = sum(!!sym(method) * spread),
              duration_i = sum(!!sym(method) * duration),
              DTS_i = sum(!!sym(method) * duration * spread),
              .groups = "keep") %>%
    ungroup() %>%
    select(date = date.x, !!sym(group), 
           price_return = price_return_i, total_return = total_return_i, 
           YTM = YTM_i, spread = spread_i, duration = duration_i, DTS = DTS_i) %>%
    group_by(!!sym(group)) %>%
    arrange(!!sym(group), date) %>%
    mutate(price_net = 1 + price_return, 
           price_total = 1 + total_return, 
           pct_chg1 = ifelse(month(ymd(date)) > month(ymd(lag(date))), total_return,
                             price_total / lag(price_total) - 1),
           pct_chg1 = replace_na(pct_chg1, 0),
           index_price_total = cumprod(1 + pct_chg1),
           
           pct_chg2 = ifelse(month(ymd(date)) > month(ymd(lag(date))), price_return,
                             price_net / lag(price_net) - 1),
           pct_chg2 = replace_na(pct_chg2, 0),
           index_price_total = cumprod(1 + pct_chg1),
           index_price_net = cumprod(1 + pct_chg2)) %>%
    mutate(cal_method = method) %>%
    select(date, cal_method, !!sym(group), YTM, spread, duration, DTS,
           index_price_net, index_price_total, return_MTD = total_return)
}


# 计算按照cat * rating2 / Label交叉分组的指数
cal_index1 <- function(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                       group = "rating2", method = "weight_wt") {
  bonds_info <- tidy_bonds(bonds_info, "cat")
  index_prices = list()
  for (i in cats) {
    df_weight <- cal_weight1(bonds_info, i, group)
    index_prices[[i]] <- cal_index(hist_data, df_weight, group, method) %>%
      mutate(cat = i)
  }
  df_prices <- bind_rows(index_prices)
  df_prices
}


# 计算全部指数的数据
cal_write_index <- function(con2, bonds_info, hist_data, cats = c("cat", "rating2", "Label"),
                            method = "weight_wt") {
  bonds_info_tidy = list()
  for (i in cats) {
    bonds_info_tidy[[i]] = tidy_bonds(bonds_info, i)
  }
  
  index_weights = list()
  for (i in cats) {
    index_weights[[i]] = cal_weight(bonds_info_tidy[[i]], i)
    dbWriteTable(con2, paste("index_", i, "_wt", sep=""), index_weights[[i]], overwrite = TRUE)
  }
  
  index_prices = list()
  for (i in cats) {
    index_prices[[i]] = cal_index(hist_data, index_weights[[i]], group = i, method)
    dbWriteTable(con2, paste("index_", i, "_price", sep=""), index_prices[[i]], overwrite = TRUE)
  }
  
  # 由于generic index算法不同，所以单列
  tidy_df <- tidy_bonds(bonds_info, group = "cat") %>%
    filter(cat == "HY Property") %>%
    mutate(dur_cat = cut(duration, breaks = c(0, 1, 3, 5, 10, Inf),
                         labels = c("0~1", "1~3", "3~5", "5~10", "10+")),
           generic = paste(Label, dur_cat, sep = "_")) %>%
    arrange(generic, date)
  df_weights <- cal_weight(tidy_df, "generic")
  df_price <- cal_index(hist_data, df_weights, group = "generic")
  dbWriteTable(con2, "index_generic_wt", df_weights, overwrite = TRUE)
  dbWriteTable(con2, "index_generic_price", df_price, overwrite = TRUE)
}


# 计算summary table 用到的指数数据，以cat和rating2交叉
cal_write_index1 <- function(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                             group = "rating2", method = "weight_wt", append = FALSE) {
  bonds_info <- tidy_bonds(bonds_info, "cat")

  index_prices = list()
  for (i in cats) {
    df_weight <- cal_weight1(bonds_info, i, group)
    index_prices[[i]] <- cal_index(hist_data, df_weight, group, method) %>%
      mutate(cat = i)
  }
  df_prices <- bind_rows(index_prices)
  if (append) {
    dbWriteTable(con2, paste("index_sum_table_", group, sep=""), 
                 df_prices, append = TRUE)
  } else {
    dbWriteTable(con2, paste("index_sum_table_", group, sep=""), 
                 df_prices, overwrite = TRUE) 
  }
}


# 更新指数价格和权重数据
#cats = c("cat", "rating2", "Label")
#method = "weight_wt"
update_index <- function(con2, cats = c("cat", "rating2", "Label"),
                         method = "weight_wt") {
  # 找下最新指数数据到哪
  index_wt_date = dbGetQuery(con2, "SELECT DISTINCT date FROM index_cat_wt")
  bonds_info <- dbGetQuery(con2, sprintf("SELECT * FROM bonds_info WHERE date > '%s'", max(index_wt_date$date)))
  
  if (dim(bonds_info)[1] == 0) {
    print("latest bonds info, no need to update")
    } else {
      index_weights = list()
      bonds_info_tidy = list()
      for (i in cats) {
        bonds_info_tidy[[i]] = tidy_bonds(bonds_info, i)
      }
      
      for (i in cats) {
        index_weights[[i]] = cal_weight(bonds_info_tidy[[i]], i)
        dbWriteTable(con2, paste("index_", i, "_wt", sep=""), index_weights[[i]], append = TRUE)
      }
      
      # generic单列
      tidy_df <- bonds_info_tidy$cat %>%
        filter(cat == "HY Property") %>%
        mutate(dur_cat = cut(duration, breaks = c(0, 1, 3, 5, 10, Inf),
                             labels = c("0~1", "1~3", "3~5", "5~10", "10+")),
               generic = paste(Label, dur_cat, sep = "_")) %>%
        arrange(generic, date)
      df_weights <- cal_weight(tidy_df, "generic")
      dbWriteTable(con2, "index_generic_wt", df_weights, append = TRUE)
    }

  # 计算index price
  index_price_date = dbGetQuery(con2, "SELECT DISTINCT date FROM index_cat_price")
  hist_data = dbGetQuery(con2, sprintf("SELECT * FROM hist_data WHERE date >= '%s'", max(index_price_date$date)))
  if (max(hist_data$date) == max(index_price_date$date)) {
    print("latest hist date, no need to update.")
    } else{
      bonds_info = dbGetQuery(con2, sprintf("SELECT * FROM bonds_info WHERE date > '%s'", 
                                            ymd(max(index_price_date$date)) %m-% months(2)))
      bonds_info_tidy = list()
      for (i in cats) {
        bonds_info_tidy[[i]] = tidy_bonds(bonds_info, i)
      }
      
      index_weights = list()
      for (i in cats) {
        index_weights[[i]] = cal_weight(bonds_info_tidy[[i]], i)
      }
      
      index_prices = list()
      for (i in cats) {
        index_price_old = dbGetQuery(
          con2, 
          sprintf("SELECT %s, index_price_net AS p_net, index_price_total AS p_total FROM %s WHERE date = '%s'",
                  i,
                  paste("index_", i, "_price", sep=""), 
                  max(index_price_date$date))
          )
        
        index_prices[[i]] <- cal_index(hist_data, index_weights[[i]], group = i, method) %>%
          arrange(!!sym(i), date) %>%
          left_join(index_price_old, by = i) %>%
          group_by(!!sym(i)) %>%
          mutate(index_price_net = index_price_net * p_net,
                 index_price_total = index_price_total * p_total) %>%
          select(-p_net, -p_total) %>%
          filter(date > max(index_price_date$date))
        dbWriteTable(con2, paste("index_", i, "_price", sep=""), index_prices[[i]], append = TRUE)
      }
      # generic 单列
      df_price <- cal_index(hist_data, df_weights, group = "generic")
      dbWriteTable(con2, "index_generic_price", df_price, append = TRUE)
      
      
      # 计算index_sum_table_Label & rating2
      index_price_old1 = dbGetQuery(
        con2, 
        sprintf("SELECT Label, cat, index_price_net AS p_net, index_price_total AS p_total FROM index_sum_table_Label WHERE date = '%s'", 
                max(index_price_date$date)))
      index_price_old2 = dbGetQuery(
        con2, 
        sprintf("SELECT rating2, cat, index_price_net AS p_net, index_price_total AS p_total FROM index_sum_table_rating2 WHERE date = '%s'", 
                max(index_price_date$date)))
      
      index_price_Label <- cal_index1(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                         group = "Label", method = "weight_wt") %>%
        arrange(cat, Label, date) %>%
        left_join(index_price_old1, by = c("cat", "Label")) %>%
        group_by(cat, Label) %>%
        mutate(index_price_net = index_price_net * p_net,
               index_price_total = index_price_total * p_total) %>%
        select(-p_net, -p_total)
      
      index_price_rating2 <- cal_index1(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                                      group = "rating2", method = "weight_wt") %>%
        arrange(cat, rating2, date) %>%
        left_join(index_price_old2, by = c("cat", "rating2")) %>%
        group_by(cat, rating2) %>%
        mutate(index_price_net = index_price_net * p_net,
               index_price_total = index_price_total * p_total) %>%
        select(-p_net, -p_total) %>%
        filter(date > max(index_price_date$date))
      
      dbWriteTable(con2, "index_sum_table_Label", index_price_Label, append = TRUE)
      dbWriteTable(con2, "index_sum_table_rating2", index_price_rating2, append = TRUE)
      }
}



# 3. Market Summary -------------------------------------------------------

bond  <- function(bonds_info, mapping_credit2) {
  rating2 <- mapping_credit2 %>%
    mutate(rating2 = as_factor(RTG2)) %>%
    select(-RTG2)
  
  bond <- bonds_info %>%
    select(ID = ID,
           Ticker = TICKER,
           Name = SECURITY_SHORT_DES,
           Category = cat,
           Rating = RTG,
           Label,
           Coupon = CPN,
           Maturity = MATURITY,
           Sector1 = INDUSTRY_SECTOR,
           Sector2 = INDUSTRY_GROUP,
           Amount = AMT_OUTSTANDING,
           Credit) %>%
    mutate(Maturity = as.Date(Maturity),
           Amount = Amount / 1000000) %>%
    filter(Amount >= 100) %>%
    left_join(rating2, by = "Credit")
}


# 
# bonds_info_latest = data_bond
# index_weights = index_wts['cat']
# hist_data_this_year = hist_data_this_year
# date_start = dates['date_wk']
# date_end = dates['date_end']
snap <- function(bonds_info_latest, index_weights, hist_data_this_year, date_start, date_end) {
  dayfrac <- yearFraction(ymd(date_start), ymd(date_end), 6)
  
  bond <- bonds_info_latest %>% mutate(cat = "real")
  coupon <- bond %>%
    select(ID, coupon = Coupon) %>%
    mutate(coupon = coupon * dayfrac)
  
  data1 <- hist_data_this_year %>%
    filter(date %in% c(date_start, date_end)) %>%
    select(date, ID, name, price, price_dirty, YTM, YTW, spread, duration) 
  
  data_chg <- data1 %>%
    select(date, ID, price, price_dirty, spread) %>%
    left_join(coupon) %>%
    group_by(ID) %>%
    arrange(ID, date) %>%
    mutate(price_chg = (price - lag(price)) / lag(price_dirty),
           coupon_chg = coupon / lag(price_dirty),
           price_chg = price_chg + coupon_chg,
           spread_chg = spread - lag(spread)) %>%
    filter(!is.na(price_chg)) %>%
    select(ID, price_chg, spread_chg) %>%
    ungroup()
  
  data <- data1 %>%
    filter(date == date_end) %>%
    select(-date) %>%
    left_join(data_chg) %>%
    distinct(ID, .keep_all = TRUE) %>%
    mutate(dur_cat = cut(duration, breaks = c(0, 1, 3, 5, 10, Inf),
                         labels = c("0~1", "1~3", "3~5", "5~10", "10+"))) %>%
    left_join(bond) %>%
    filter(cat == "real") %>%
    select(-cat) %>%
    ungroup()
  
  return(data)
}

# method: weight_wt or weight_eq for weighted method
summary <- function(index_weights, index_price, date_start, date_end, group = "Category", method = "weight_wt") {
  summary_index_price <- as_tibble(index_price) %>%
    filter(date %in% c(date_start, date_end) & cal_method == method) %>%
    group_by(!!sym(group)) %>%
    arrange(!!sym(group), date) %>%
    mutate(price_chg = index_price_net / lag(index_price_net) - 1,
           spread_chg = spread - lag(spread)) %>%
    na.omit() %>%
    select(!!sym(group), YTM, spread, duration, price_chg, spread_chg)
  
  summary <- as_tibble(index_weights) %>%
    mutate(total_mv = sum(price_dirty * Amount, na.rm = TRUE),
           mv = price_dirty * Amount) %>%
    group_by(!!sym(group)) %>%
    summarise(Amount = sum(Amount, na.rm = TRUE),
              weight_wt = sum(mv / total_mv, na.rm = TRUE),
              issues = n()) %>%
    mutate(weight_eq = issues / sum(issues)) %>%
    ungroup() %>%
    select(!!sym(group), Amount, weight = !!sym(method), issues) %>%
    left_join(summary_index_price)
  
  return(summary)
}

#index_sum_table = index_sum_table_rating2
# test <- summary1(index_sum_table_Label, bonds_info_tidy, group1 = "HY Property",
#          group2 = "Label", dates)
summary1 <- function(index_sum_table, bonds_info_tidy,
                     group1 = "HY Property", group2 = "rating2", dates) {
  price_wk <- index_sum_table %>%
    filter(Category == group1, date %in% c(dates$date_wk, dates$date_end)) %>%
    group_by(!!sym(group2)) %>%
    mutate(price_chg = index_price_total / lag(index_price_total) - 1, 
           spread_chg = spread - lag(spread)) %>%
    select(!!sym(group2), YTM, spread, duration, DTS, price_chg, spread_chg)
  price_mt <- index_sum_table %>%
    filter(Category == group1, date %in% c(dates$date_mt, dates$date_end)) %>%
    group_by(!!sym(group2)) %>%
    mutate(price_MTD = index_price_total / lag(index_price_total) - 1,
           spread_MTD = spread - lag(spread)) %>%
    select(!!sym(group2), price_MTD, spread_MTD)
  price_yr <- index_sum_table %>%
    filter(Category == group1, date %in% c(dates$date_yr, dates$date_end)) %>%
    group_by(!!sym(group2)) %>%
    mutate(price_YTD = index_price_total / lag(index_price_total) - 1,
           spread_YTD = spread - lag(spread)) %>%
    select(!!sym(group2), price_YTD, spread_YTD)
  summary <- bonds_info_tidy %>%
    filter(cat == group1) %>%
    mutate(mv = price_dirty * AMT_OUTSTANDING,
           total_mv = sum(mv)) %>%
    group_by(!!sym(group2)) %>%
    summarise(Amount = sum(mv) / 1000000,
              weight = sum(mv / total_mv),
              issues = n(),
              .groups = "keep") %>%
    left_join(price_wk) %>%
    left_join(price_mt) %>%
    left_join(price_yr) %>%
    na.omit() %>%
    select(!!sym(group2), Amount, weight, issues, YTM, spread, duration, price_chg,
           price_MTD, price_YTD, spread_chg, spread_MTD, spread_YTD)
  summary
}

# cat1 = "HY Property"
# group1 = "Label"
# group2 = "dur_cat"
#summary2(snap_data, cat1, group1, group2)
summary2 <- function(snap_data, cat1, group1, group2) {
  
  if (cat1 != "") {summary <- filter(snap_data, Category == cat1)} else{summary <- snap_data}
  summary <- summary %>%
    filter(duration >= 0.3) %>%
    filter(YTM > 0) %>%
    filter(spread > 0) %>%
    mutate(bench = percent_rank(YTM)) %>%
    filter(bench < 0.99) %>%
    mutate(total = sum(price * Amount, na.rm = TRUE),
           issues = 1) %>%
    group_by(get(group1), get(group2)) %>%
    mutate(weight = price * Amount / sum(price * Amount, na.rm = TRUE)) %>%
    summarise(spread = sum(spread * weight, na.rm = TRUE),
              duration = sum(duration * weight, na.rm = TRUE),
              YTM = sum(YTM * weight, na.rm = TRUE),
              price_chg = sum(price_chg * weight, na.rm = TRUE),
              spread_chg = sum(spread_chg * weight, na.rm = TRUE),
              weight = sum(price * Amount / total, na.rm = TRUE),
              issues = sum(issues),
              Amount = sum(Amount, na.rm = TRUE)) %>%
    select(level1 = `get(group1)`, level2 = `get(group2)`, 
           Amount, weight, issues,
           YTM, spread, duration,
           price_chg, spread_chg) %>%
    filter(weight >= 0.01) %>%
    ungroup()
  
  return(summary)
}



# 4. Market Table --------------------------------------------------------

tidytable <- function(data, type) {
  
  
  if(type %in% c("weight", "price_chg")) {
    
    data = percent(data, digits = 1)
  } else if(type %in% c("duration", "YTM", "YTC")) {
    
    data = round(data, digits = 2)
    
  } else if (type %in% c("spread", "spread_chg")) {
    
    data = round(data, digits = 0)
  } else if (type %in% c("Amount")) {
    
    data = accounting(data, digits = 0)
  }
  
  return(data)
  
}


summary_table <- function(index_weights, index_price, date_start, date_end, group = "Category", method = "weight_wt") {
  summary_data <- summary(index_weights, index_price, date_start, date_end, group, method)
  summary_table <- summary_data %>%
    mutate(weight = tidytable(weight, "weight"),
           YTM = tidytable(YTM, "YTM"),
           duration = tidytable(duration, "duration"),
           spread = tidytable(spread, "spread"),
           price_chg = tidytable(price_chg, "price_chg"),
           spread_chg = tidytable(spread_chg, "spread_chg"),
           Amount = tidytable(Amount, "Amount")) %>%
    ungroup()
  
  return(summary_table)
}


# used in summary_table_present2 to cat x rating2 & Label
summary_table1 <- function(summary_data) {
  summary_table <- summary_data %>%
    mutate(weight = tidytable(weight, "weight"),
           YTM = tidytable(YTM, "YTM"),
           duration = tidytable(duration, "duration"),
           spread = tidytable(spread, "spread"),
           price_chg = tidytable(price_chg, "price_chg"),
           price_MTD = tidytable(price_MTD, "price_chg"),
           price_YTD = tidytable(price_YTD, "price_chg"),
           spread_chg = tidytable(spread_chg, "spread_chg"),
           spread_MTD = tidytable(spread_MTD, "spread_chg"),
           spread_YTD = tidytable(spread_YTD, "spread_chg"),
           Amount = tidytable(Amount, "Amount")) %>%
    ungroup()
  
  return(summary_table)
}

# 按group1 对cat进行筛选
# group1 = "HY Property"
# group2 = "rating2"
summary_table2 <- function(snap_data, cat1, group1, group2) {
  
  summary_data <- summary2(snap_data, cat1, group1, group2)
  summary_table <- summary_data %>%
    mutate(weight = tidytable(weight, "weight"),
           YTM = tidytable(YTM, "YTM"),
           duration = tidytable(duration, "duration"),
           spread = tidytable(spread, "spread"),
           price_chg = tidytable(price_chg, "price_chg"),
           spread_chg = tidytable(spread_chg, "spread_chg"),
           Amount = tidytable(Amount, "Amount"))
}


#metric = "spread_chg"
#group1 = "Category"
#group2 = "dur_cat"
#summary_table3(snap_data, metric, "Category", "dur_cat")
# By Category price_chg
summary_table3 <- function(snap_data, metric, cat1, group1, group2) {
  
  table <- summary_table2(snap_data, cat1, group1, group2) %>%
    mutate(metric = get(metric)) %>%
    select(level1, level2, metric) %>%
    mutate(metric = spreadformatter(metric)) %>%
    spread(level2, metric) 
  
  table <- table%>%
    kable("html", escape = F, 
          align = c("l", rep("c", NCOL(table) - 1)), 
          caption = paste("By", print(group1), print(metric), sep = " ")) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    column_spec(1 : NCOL(table), color = "black") %>%
    column_spec(1 : NCOL(table), bold = "T")
}


# summary_table3_1 <- function(bonds_info_tidy, metric, group1, group2) {
#   table <- bonds_info %>%
#     filter(cat == group1)
#     
# }


# By Category spread
summary_table4 <- function(snap_data, metric, cat1, group1, group2) {
  
  table <- summary_table2(snap_data, cat1, group1, group2) %>%
    mutate(metric = get(metric)) %>%
    select(level1, level2, metric) %>%
    mutate(metric = color_tile("pink", "red")(metric)) %>%
    spread(level2, metric) 
  
  table <- table%>%
    kable("html", escape = F, 
          align = c("l", rep("c", NCOL(table) - 1)), 
          caption = paste("By", print(group1), print(metric), sep = " ")) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    column_spec(1 : NCOL(table), color = "black") %>%
    column_spec(1 : NCOL(table), bold = "T")
}


# used in summary_table_present2
spreadformatter <- function(spread_chg) {
  
  color <- ifelse(spread_chg >= 0,
                  cell_spec(spread_chg, color = "green"),
                  cell_spec(spread_chg, color = "red"))
}


snap_table <- function(snap_data, data_bond) {
  snap_table <- snap_data %>%
    left_join(data_bond) %>%
    na.omit() %>%
    mutate(YTM = tidytable(YTM, "YTM"),
           YTW = tidytable(YTW, "YTW"),
           duration = tidytable(duration, "duration"),
           spread = tidytable(spread, "spread"),
           price_chg = tidytable(price_chg, "price_chg"),
           spread_chg = tidytable(spread_chg, "spread_chg"),
           Amount = tidytable(Amount, "Amount")) %>%
    select(ID, Name, Rating, YTM, YTW, spread, duration,
           price_chg, spread_chg, Category, Label, rating2)
  
  snap_table <- formattable(snap_table, 
                            align = c(rep("c", NCOL(snap_table))),
                            list(price_chg = sign_formatter,
                                 spread_chg = sign_formatter))
}


heatmap_tree1 <- function(data){
  
  data_tree <- data %>% select(Category, Ticker, Name, Return = price_chg, Amount)
  data_tree <- treemap(data_tree, index=c("Category", "Ticker", "Name"), 
                       vSize="Amount", vColor= "Return", 
                       type="value")
}


#data = data_snap
heatmap_tree2 <- function(data){
  
  data_tree <- data %>% select(Ticker, Name, Label, Return = price_chg, Amount)
  data_tree <- treemap(data_tree, index=c("Label","Ticker", "Name"), 
                       vSize="Amount", vColor= "Return", 
                       type="value")
}

heatmap_tree3 <- function(data){
  
  data_tree <- data %>% select(Ticker, Name, rating2, Return = price_chg, Amount)
  data_tree <- treemap(data_tree, index=c("rating2","Ticker", "Name"), 
                       vSize="Amount", vColor= "Return", 
                       type="value")
}


# 计算展示表，包含Amout, weight, issues, YTM, spread, duration, price_chg, price_MTD, price_YTD, spread_chg, spread_MTD, spread_YTD
# dates包含date_end, date_wk, date_mt, date_yr
# index_weights = index_wts$rating2
# index_price = index_prices$rating2
# group = "rating2"
# method = "weight_wt"
# summary_table_present2(index_weights, index_price, dates, group = "rating2", method = "weight_wt")
# group = "Label"
summary_table_present3 <- function(index_sum_table, bonds_info_tidy, 
                                   group1 = "Financials",group2 = "rating2",
                                   dates) {
  summary <- summary1(index_sum_table, bonds_info_tidy, 
                      group1, group2, dates)
  
  summary <- summary_table1(summary)
  
  table <- summary %>%
    select(!!sym(group2), Amount, weight, issues, YTM, spread, duration,
           price_chg, price_MTD, price_YTD, spread_chg, spread_MTD, spread_YTD) %>%
    mutate_at(c("price_chg", "price_MTD", "price_YTD", "spread_chg", "spread_MTD", "spread_YTD"), spreadformatter) %>%
    kable("html", escape = F, 
          align = c("l", rep("c", NCOL(table) - 1)),
          caption = paste("By", print(group2), sep = " ")) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    column_spec(c(1, 8, 9, 10, 11, 12, 13), bold = T) %>%
    column_spec(1 : NCOL(table), color = "black")
  
  return(table)
}


summary_table_present2 <- function(index_weights, index_price, dates, group = "Category", method = "weight_wt") {
  
  table <- summary_table(index_weights, index_price, dates$date_wk, dates$date_end, group, method)
  table_MTD <- summary_table(index_weights, index_price, dates$date_mt, dates$date_end, group, method) %>%
    select(group, price_MTD = price_chg, spread_MTD = spread_chg)
  table_YTD <- summary_table(index_weights, index_price, dates$date_yr, dates$date_end, group, method) %>%
    select(group, price_YTD = price_chg, spread_YTD = spread_chg)
  
  table <- table %>%
    left_join(table_MTD) %>%
    left_join(table_YTD) 
  
  table <- table %>%
    select(group, Amount, weight, issues, YTM, spread, duration,
           price_chg, price_MTD, price_YTD, spread_chg, spread_MTD, spread_YTD) %>%
    mutate_at(c("price_chg", "price_MTD", "price_YTD", "spread_chg", "spread_MTD", "spread_YTD"), spreadformatter) %>%
    kable("html", escape = F, 
          align = c("l", rep("c", NCOL(table) - 1)),
          caption = paste("By", print(group), sep = " ")) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    column_spec(c(1, 8, 9, 10, 11, 12, 13), bold = T) %>%
    column_spec(1 : NCOL(table), color = "black")
  
  return(table)
}


# snap_data = snap_data_MTD
# caption = "MTD"
# group1 = ""

top <- function(snap_data, data_bond, caption = "", group1) {
  
  if (group1 != "") {top <- filter(snap_table(snap_data, data_bond), Category == group1)} else{top <- snap_table(snap_data, data_bond)}
  top <- top %>%  
    filter(duration >= 0.3) %>%
    filter(YTM > 0) %>%
    filter(spread > 0) %>%
    mutate(bench = percent_rank(YTM)) %>%
    filter(bench < 0.99) %>%
    select(-bench) %>%
    arrange(desc(price_chg))%>%
    slice(1:10)  %>% 
    select(-ID) %>%
    mutate(price_chg = spreadformatter(price_chg),
           spread_chg = spreadformatter(spread_chg)) %>%
    select(-Category, -Label, -rating2) %>%
    kable("html", escape = F, 
          align = c(rep("c", NCOL(summary_table))), 
          caption = paste("Top10", caption, sep = " ")) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    column_spec(c(1, 7, 8), bold = T) %>%
    column_spec(1 : 8, color = "black")
}


bottom <- function(snap_data, data_bond, caption = "", group1="") {
  
  if (group1 != "") {botm <- filter(snap_table(snap_data, data_bond), Category == group1)} else{botm <- snap_table(snap_data, data_bond)}
  botm <- botm %>%  
    filter(duration >= 0.3) %>%
    filter(YTM > 0) %>%
    filter(spread > 0) %>%
    mutate(bench = percent_rank(YTM)) %>%
    filter(bench < 0.99) %>%
    select(-bench) %>%
    arrange(-desc(price_chg))%>%
    slice(1:10)  %>% 
    select(-ID) %>%
    mutate(price_chg = spreadformatter(price_chg),
           spread_chg = spreadformatter(spread_chg)) %>%
    select(-Category, -Label, -rating2) %>%
    kable("html", escape = F, 
          align = c(rep("c", NCOL(summary_table))),
          caption = paste("Bottom10", caption, sep = " ")) %>%
    kable_styling(bootstrap_options = "striped", full_width = F) %>%
    column_spec(c(1, 7, 8), bold = T) %>%
    column_spec(1 : 8, color = "black")
}


present_table <- function(table) {
  
  as.datatable(table, class = " compact stripe",
               editable = TRUE, 
               options = list(pageLength = 20,
                              dom = 'Bfrtip',
                              initComplete = JS("function(settings, json) {","$('body').css({'font-family': 'Calibri'});","}")))
}



# 5. Market plot ----------------------------------------------------------
# index_price = index_prices$cat
# name = "AT1"
# metric = "spread"
metric_plot <- function(index_price, name, metric) {

  result <- index_price %>%
    filter(Category == name) %>%
    rename(yield = YTM) %>%
    select(date, !!sym(metric)) %>%
    mutate(mean = get(metric),
           std = sd(mean, na.rm = TRUE)) %>%
    mutate(mean = mean(mean, na.rm = TRUE),
           std1 = mean + std,
           std2 = mean - std,
           date = ymd(date),
           dif = !!sym(metric) - lag(!!sym(metric)),
           !!sym(metric) := ifelse(abs(dif) > 1.4 * std, NA, !!sym(metric))) %>%
    fill(!!sym(metric))

  metric_plot <- ggplot(data = result, aes(x = date)) +
    geom_line(aes(y = get(metric)), color = "#D70C19", size = 1) +
    geom_line(aes(y = mean), color = "blue", linetype = 2) +
    geom_line(aes(y = std1), color = "#4B4948", linetype = 2) +
    geom_line(aes(y = std2), color = "#4B4948", linetype = 2) +
    ylab(print(metric)) +
    labs(title = print(name)) +
    theme_classic()

  return(metric_plot)
}


# group = "Category"
# name1 = "Tier 2"
# name2 = "AT1"
# metric = "spread"
# index_price = index_prices$cat
metric_plot2 <- function(index_price, group, name1, name2, metric) {
  
  # ID <- data_bond %>%
  #   filter(get(group) %in% c(name1, name2)) %>%
  #   select(ID, print(group))
  
  result <- index_price %>%
    rename(yield = YTM) %>%
    select(date, !!sym(group), !!sym(metric)) %>% 
    pivot_wider(names_from = !!sym(group), values_from = !!sym(metric)) %>%
    mutate(spread = get(name2) - get(name1), 
           std = sd(spread, na.rm = TRUE), 
           mean = mean(spread, na.rm = TRUE),
           std1 = mean + std,
           std2 = mean - std,
           date = as.Date(date),
           spread = replace(spread, abs(spread - lag(spread)) > std, NA)) %>%
    fill(spread) %>%
    select(date, spread, mean, std1, std2)
  
  metric_plot <- ggplot(data = result, aes(x = date)) +
    geom_line(aes(y = spread), color = "red", size = 1) +
    geom_line(aes(y = mean), color = "blue", linetype = 2) +
    geom_line(aes(y = std1), color = "black", linetype = 2) +
    geom_line(aes(y = std2), color = "black", linetype = 2) +
    ylab(print(metric)) +
    labs(title = paste(print(name1), "vs", print(name2), sep = " ")) +
    theme_classic()
}


#name = "HY Property"
#index_price = index_prices$cat
index_plot <- function(index_price, name) {
  
  return <- index_price %>%
    filter(Category == name) %>%
    select(date, index_price_net, index_price_total) %>%
    mutate(date = ymd(date))
  
  index_plot <- ggplot(data = return, aes(x = date)) + 
    geom_line(aes(y = index_price_total), color = "#D70C19", size = 1) +
    geom_line(aes(y = index_price_net), color = "#4B4948", size = 1) +
    ylab("return") +
    labs(title = print(name)) + 
    theme_classic()
}


# size = 20
# group = "rating2"
# metric = "YTM"
dot_plot <- function(data_bond, snap_data, name, group, metric, size) {
  
  data <- data_bond %>% filter(Category == name)
  
  top <- data %>% 
    group_by(Ticker) %>%
    summarise(Amount = sum(Amount, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(desc(Amount)) %>%
    slice(1 : size)
  
  data <- snap_table(snap_data, data_bond) %>%
    left_join(data) %>%
    filter(!is.na(duration)) %>%
    select(Ticker, Name, print(group), print(metric), duration) %>%
    left_join(top) %>%
    filter(!is.na(Amount)) %>% 
    filter(duration >= 0.5)
  
  dot_plot <- ggplot(data = data, aes(x = duration, y = get(metric), 
                                      color = Ticker, label = Name,
                                      shou.legend = FALSE)) +
    geom_point(size = 3, show.legend = FALSE) +
    geom_text(check_overlap = TRUE, size = 3, show.legend = FALSE) +
    facet_wrap(vars(get(group)), scales = "free", ncol = 1) +
    ylab(print(metric)) +
    ggtitle(paste("By", print(group), sep = " ")) +
    theme_classic()
}

# index_price1 = index_prices$cat
# index_price2 = index_prices$Label
# metric = "yield"
# 根据两个共同分类画图

# index_price = index_generic_price
generic_plot <- function(index_price) {
  
  result <- index_price %>%
    rename(yield = YTM) %>%
    separate(generic, into = c("Label", "duration"), sep = "_") %>%
    filter(Label %in% c("crossover", "high beta", "low beta")) %>%
    select(date, Label, duration, yield) %>% 
    group_by(Label, duration) %>%
    mutate(mean = yield,
           std = sd(mean, na.rm = TRUE)) %>%
    mutate(mean = mean(mean, na.rm = TRUE),
           std1 = mean + std,
           std2 = mean - std,
           date = ymd(date)) %>%
    ungroup()
  
  dot_plot <- ggplot(data = result, aes(x = date)) +
    geom_line(aes(y = yield)) +
    geom_line(aes(y = mean), color = "blue", linetype = 2) +
    geom_line(aes(y = std1), color = "red", linetype = 2) +
    geom_line(aes(y = std2), color = "red", linetype = 2) +
    ylab("yield") +
    facet_grid(rows = vars(duration),
               cols = vars(Label),
               scales = "free") +
    ggtitle(paste("By", "Label", sep = " "))
}



# 6. Table Style --------------------------------------------------------

RG_bar <- function(x) {
  
  style1 <- function(x) {
    style1 <- style( display = "block",
                     padding = "0 4px",
                     "border-radius" = "4px",
                     "background-color" = csscolor(rgb(0, 1, 0, alpha = 1)))
    return(style1)
  }
  
  style2 <- function(x) {
    style2 <- style( display = "block",
                     padding = "0 4px",
                     "border-radius" = "4px",
                     "background-color" = csscolor(rgb(1, 0, 0, alpha = 1)))
    return(style2)
  }
  
  style3 <- function(x) {
    style3 <- style( display = "block",
                     padding = "0 4px",
                     "border-radius" = "4px",
                     # "color" = "white",
                     "background-color" = "white")
    return(style3)
  } 
  
  
  format <- formatter("span", style = x ~ ifelse(is.na(x), style3,
                                                 ifelse(x >= 0, style1(x), style2(x))))
  
  return(format)
}

sign_formatter <- formatter("span", 
                            style = x ~ style(color = ifelse(x > 0, "green", 
                                                             ifelse(x < 0, "red", "black"))))


# 7. Market Report --------------------------------------------------------
# name = "EM"
report1 <- function(data_bond, snap_data, index_wts, index_prices, name) {
  
  #data_bond <- bond(bonds_info, mapping_credit2) %>% filter(Category == name)
  
  data_bond <- data_bond %>% filter(Category == name)
  
  snap_data <- data$snap_data
  snap_data_MTD <- data$snap_data_MTD
  snap_data_YTD <- data$snap_data_YTD
  
  data_snap <- snap_table(snap_data, data_bond)
  
  if(name %in% c("LGFV", "Industry", "AT1", "Tier 2")) {
    data_heat <- heatmap_tree3(data_snap %>% left_join(data_bond))} else{
      data_heat <- heatmap_tree2(data_snap %>% left_join(data_bond))}
  
  data_summary1 <- summary_table_present3(index_sum_table_rating2, bonds_info_tidy,
                                          group1 = name, group2 = "rating2", dates)
  data_summary2 <- summary_table_present3(index_sum_table_Label, bonds_info_tidy,
                                          group1 = name, group2 = "Label", dates)
  data_summary3 <- summary_table3(snap_data, metric = "spread_chg", cat1 = name, "Label", "dur_cat")
  data_summary4 <- summary_table3(snap_data, metric = "price_chg", cat1 = name, "Label", "dur_cat")
  data_summary5 <- summary_table4(snap_data, metric = "spread", cat1 = name, "Label", "dur_cat")
  data_summary6 <- summary_table4(snap_data, metric = "YTM", cat1 = name, "Label", "dur_cat")
  data_summary7 <- summary_table3(snap_data, metric = "spread_chg", cat1 = name, "rating2", "dur_cat")
  data_summary8 <- summary_table3(snap_data, metric = "price_chg", cat1 = name, "rating2", "dur_cat")
  data_summary9 <- summary_table4(snap_data, metric = "spread", cat1 = name, "rating2", "dur_cat")
  data_summary10 <- summary_table4(snap_data, metric = "YTM", cat1 = name, "rating2", "dur_cat")
  data_top <- top(snap_data, data_bond, caption = "", group1 = name)
  data_top_MTD <- top(snap_data_MTD, data_bond, caption = "MTD", name)
  data_top_YTD <- top(snap_data_YTD, data_bond, caption = "YTD", name)
  data_bottom <- bottom(snap_data, data_bond, caption = "", name)
  data_bottom_MTD <- bottom(snap_data_MTD, data_bond, caption = "MTD", name)
  data_bottom_YTD <- bottom(snap_data_YTD, data_bond, caption = "YTD", name)
  
  plot_return <- index_plot(index_prices$cat, name)
  plot_spread <- metric_plot(index_prices$cat, name, "spread")
  plot_yield <- metric_plot(index_prices$cat, name, "yield")
  plot_DTS <- metric_plot(index_prices$cat, name, "DTS")
  plot_dot1 <- dot_plot(data_bond, snap_data, name, group = "rating2", metric = "YTM", size = 20)
  plot_dot2 <- dot_plot(data_bond, snap_data, name, "Label", "YTM", 20)
  plot_dot3 <- dot_plot(data_bond, snap_data, name, "rating2", "spread", 20)
  plot_dot4 <- dot_plot(data_bond, snap_data, name, "Label", "spread", 20)
  
  report <- list(bond = data_bond,
                 snap = data_snap,
                 heat = data_heat,
                 summary1 = data_summary1,
                 summary2 = data_summary2,
                 summary3 = data_summary3,
                 summary4 = data_summary4,
                 summary5 = data_summary5,
                 summary6 = data_summary6,
                 summary7 = data_summary7,
                 summary8 = data_summary8,
                 summary9 = data_summary9,
                 summary10 = data_summary10,
                 top = data_top,
                 bottom = data_bottom,
                 top_MTD = data_top_MTD,
                 bottom_MTD = data_bottom_MTD,
                 top_YTD = data_top_YTD,
                 bottom_YTD = data_bottom_YTD,
                 return = plot_return,
                 spread = plot_spread,
                 yield = plot_yield,
                 DTS = plot_DTS,
                 dot1 = plot_dot1,
                 dot2 = plot_dot2,
                 dot3 = plot_dot3,
                 dot4 = plot_dot4)
  return(report)
}

#name = ""
report2 <- function(bonds_info, mapping_credit2, data, index_wts, index_prices, name = "") {
  
  data_bond <- bond(bonds_info, mapping_credit2)
  snap_data <- data$snap_data
  snap_data_MTD <- data$snap_data_MTD
  snap_data_YTD <- data$snap_data_YTD
  
  data_snap <- snap_table(snap_data, data_bond)
  
  tree_origin <- data_snap %>% left_join(data_bond)
  data_heat <- heatmap_tree1(tree_origin)
  data_summary1 <- summary_table_present2(index_wts$cat, index_prices$cat, dates, group = "Category", method = "weight_wt")
  data_summary2 <- summary_table_present2(index_wts$rating2, index_prices$rating2, dates, group = "rating2", method = "weight_wt")
  
  data_summary3 <- summary_table3(snap_data, "price_chg", cat1 = name, "Category", "dur_cat")
  data_summary4 <- summary_table4(snap_data, "spread", cat1 = name, "Category", "dur_cat")
  data_top <- top(snap_data, data_bond, caption = "", group1 = "")
  data_top_MTD <- top(snap_data_MTD, data_bond, caption = "MTD", group1 = "")
  data_top_YTD <- top(snap_data_YTD, data_bond, caption = "YTD", group1 = "")
  data_bottom <- bottom(snap_data, data_bond, caption = "", group1 = "")
  data_bottom_MTD <- bottom(snap_data_MTD, data_bond, caption = "MTD", group1 = "")
  data_bottom_YTD <- bottom(snap_data_YTD, data_bond, caption = "YTD", group1 = "")
  plot_spread1 <- metric_plot2(index_prices$cat, group = "Category", name1 = "IG Corporate", name2 = "HY Property", metric = "spread")
  plot_spread2 <- metric_plot2(index_prices$rating2, "rating2", "A", "BBB", "spread")
  plot_spread3 <- metric_plot2(index_prices$rating2, "rating2", "BBB", "BB", "spread")
  plot_spread4 <- metric_plot2(index_prices$rating2, "rating2", "BB", "B", "spread")
  
  report <- list(bond = data_bond,
                 snap = data_snap,
                 heat = data_heat,
                 summary1 = data_summary1,
                 summary2 = data_summary2,
                 summary3 = data_summary3,
                 summary4 = data_summary4,
                 top = data_top,
                 bottom = data_bottom,
                 top_MTD = data_top_MTD,
                 bottom_MTD = data_bottom_MTD,
                 top_YTD = data_top_YTD,
                 bottom_YTD = data_bottom_YTD,
                 plot1 = plot_spread1, 
                 plot2 = plot_spread2,
                 plot3 = plot_spread3, 
                 plot4 = plot_spread4)
  
  return(report)
  
  
}

