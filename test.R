library(flextable)
library(gtools)

set_flextable_defaults(
  font.family = "Calibri", font.size = 10, font.color = "black",
  text.align = "centered", 
  table.layout = "fixed",
  theme_fun = "theme_vanilla")



ft <- flextable(head(iris)) 
ft <- set_header_labels(ft, Sepal.Length = "Sepal", 
                        Sepal.Width = "Sepal", Petal.Length = "Petal",
                        Petal.Width = "Petal" )
ft <- merge_at(ft, i=1, j=1:2, part = "header")
ft <- merge_at(ft, i=1, j=3:4, part = "header")
ft <- add_header_row(ft, values = c("Length", "Width", "Length", "Width", ""),
                     top=FALSE) %>%
  align(align="center") %>%
  add_footer_lines(values = "source: Bosera")



html_table = Market$summary3[1]

tran_kable1 <- function(html_table, page_width=7.5, mode = 1) {
  ft <- as.data.frame(read_html(html_table) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, "")))
  if (mode == 1) {
    ftout <- ft %>%
      select(-Amount, -weight, -issues) %>%
      flextable() %>%
      bold(j = c(1, 5, 6, 7, 8, 9, 10), bold = TRUE) %>%
      color(~ price_chg < 0, color = "red", ~ price_chg) %>%
      color(~ price_MTD < 0, color = "red", ~ price_MTD) %>%
      color(~ price_YTD < 0, color = "red", ~ price_YTD) %>%
      color(~ price_chg > 0, color = "#008000", ~ price_chg) %>%
      color(~ price_MTD > 0, color = "#008000", ~ price_MTD) %>%
      color(~ price_YTD > 0, color = "#008000", ~ price_YTD) %>%
      color(~ spread_chg < 0, color = "red", ~ spread_chg) %>%
      color(~ spread_MTD < 0, color = "red", ~ spread_MTD) %>%
      color(~ spread_YTD < 0, color = "red", ~ spread_YTD) %>%
      color(~ spread_chg > 0, color = "#008000", ~ spread_chg) %>%
      color(~ spread_MTD > 0, color = "#008000", ~ spread_MTD) %>%
      color(~ spread_YTD > 0, color = "#008000", ~ spread_YTD) %>%
      
      set_header_labels(Category = "分组", rating2 = "分组", 
                        YTM = "到期收益率", spread = "信用利差", duration = "久期",
                        price_chg = "价格涨跌幅", price_MTD = "价格涨跌幅",
                        price_YTD = "价格涨跌幅", spread_chg = "信用利差变动",
                        spread_MTD = "信用利差变动", spread_YTD = "信用利差变动") %>%
      merge_at(i=1, j=5:7, part = "header") %>%
      merge_at(i=1, j=8:10, part = "header") %>%
      add_header_row(values = c("", "", "", "", "周", "月", "年", "周", "月", "年"),
                     top = FALSE) %>%
      theme_vanilla() %>%
      align(align="center", part="all")
  }
  
  if (mode == 2) {
    ftout <- ft %>%
      flextable() %>%
      bold(j = c(1, 7, 8), bold = TRUE) %>%
      color(~ price_chg < 0, color = "red", ~ price_chg) %>%
      color(~ price_chg > 0, color = "#008000", ~ price_chg) %>%
      color(~ spread_chg < 0, color = "red", ~ spread_chg) %>%
      color(~ spread_chg > 0, color = "#008000", ~ spread_chg) %>%
      
      set_header_labels(Name = "债券简称", Rating = "综合评级", 
                        YTM = "到期收益率", spread = "信用利差", duration = "久期",
                        price_chg = "价格涨跌幅", spread_chg = "信用利差变动") %>%
      theme_vanilla() %>%
      align(align="center", part="all")
  }
  
  ftout
}

html_table = EM$summary9[1]

tran_kable2 <- function(html_table, page_width=7.5, mode=1) {
  ft <- as.data.frame(read_html(html_table) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, "")))

  if (mode == 1) {
    ftout <- ft %>%
      flextable() %>%
      bold(bold=TRUE) %>%
      color(~ X0.1 < 0, color = "red", ~X0.1) %>%
      color(~ X0.1 > 0, color = "#008000", ~X0.1) %>%
      color(~ X1.3 < 0, color = "red", ~X1.3) %>%
      color(~ X1.3 > 0, color = "#008000", ~X1.3) %>%
      color(~ X3.5 < 0, color = "red", ~X3.5) %>%
      color(~ X3.5 > 0, color = "#008000", ~X3.5) %>%
      color(~ X5.10 < 0, color = "red", ~X5.10) %>%
      color(~ X5.10 > 0, color = "#008000", ~X5.10) %>%
      color(~ X10. < 0, color = "red", ~X10.) %>%
      color(~ X10. > 0, color = "#008000", ~X10.) %>%
      set_header_labels(level1 = "分组", X0.1 = "0~1", X1.3 = "1~3",
                        X3.5 = "3~5", X5.10 = "5~10", X10. = "10+") %>%
      width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
      align(align = "center", part = "all") %>%
      autofit()
  }
  
  if (mode == 2) {
    f1 <- ft %>%
      pivot_longer(cols = starts_with("X"), 
                   names_to = "duration",
                   values_to = "spread") %>%
      mutate(spread = as.double(spread),
             quants = ntile(spread, n = 5)) %>%
      filter(quants >= 4) %>%
      arrange(spread)

    ftout <- ft %>%
      flextable() %>%
      bold(bold=TRUE)
    if ("X0.1" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X0.1 %in% f1$spread, j = "X0.1", color = "#FF7A81")}
    if ("X1.3" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X1.3 %in% f1$spread, j = "X1.3", color = "#FF7A81")}
    if ("X3.5" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X3.5 %in% f1$spread, j = "X3.5", color = "#FF7A81")}
    if ("X5.10" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X5.10 %in% f1$spread, j = "X5.10", color = "#FF7A81")} else{}
    if ("X10."%in% names(ft)) {ftout <- highlight(ftout, i = ~ X10. %in% f1$spread, j = "X10.", color = "#FF7A81")}
    ftout <- ftout %>%
      set_header_labels(level1 = "分组", X0.1 = "0~1", X1.3 = "1~3",
                        X3.5 = "3~5", X5.10 = "5~10", X10. = "10+") %>%
      width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
      align(align = "center", part = "all") %>%
      autofit()
  }
  ftout
}


tran_kable2(EM$summary9[1], mode=2)

















tran_kable2 <- function(html_table, page_width=6.5, mode=2) {
  ft <- as.data.frame(read_html(Market$summary4[1]) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, "")))
  ft1 <- ft %>%
    pivot_longer(cols = c(2:length(names(ft))), 
                 names_to = "duration",
                 values_to = "spread") %>%
    mutate(spread = as.double(spread),
           quants = ntile(spread, n = 5)) %>%
    filter(quants >= 4) %>%
    select(spread)
  cut_value = ft1$spread
  
  if (mode == 1) {
    ftout <- ft %>%
      flextable() %>%
      bold(bold=TRUE, part="all")
    
    if ("X0.1" %in% names(ft)) {ftout <- color(ftout ,~ X0.1 < 0, color = "red", ~X0.1) %>%
      color(~ X0.1 > 0, color = "#008000", ~X0.1)}
    if ("X1.3" %in% names(ft)) {ftout <- color(ftout ,~ X1.3 < 0, color = "red", ~X1.3) %>%
      color(~ X1.3 > 0, color = "#008000", ~X1.3)}
    if ("X3.5" %in% names(ft)) {ftout <- color(ftout ,~ X3.5 < 0, color = "red", ~X3.5) %>%
      color(~ X3.5 > 0, color = "#008000", ~X3.5)}
    if ("X5.10" %in% names(ft)) {ftout <- color(ftout ,~ X5.10 < 0, color = "red", ~X5.10) %>%
      color(~ X5.10 > 0, color = "#008000", ~X5.10)}
    if ("X10." %in% names(ft)) {ftout <- color(ftout ,~ X10. < 0, color = "red", ~X10.) %>%
      color(~ X10. > 0, color = "#008000", ~X10.)}
    
    ftout <- ftout %>%
      font(fontname = "Microsoft YaHei", part = "header") %>%
      font(fontname = "Calibri", part = "body") %>%
      fontsize(size = 10, part = "all") %>%
      set_header_labels(level1 = "", X0.1 = "0~1", X1.3 = "1~3",
                        X3.5 = "3~5", X5.10 = "5~10", X10. = "10+") %>%
      width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
      align(align = "center", part = "all")
    # autofit()
    # return(ftout)
  }
  
  if (mode == 2) {
    ftout <- ft %>%
      flextable()
    if ("X0.1" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X0.1 %in% cut_value, j = "X0.1", color = "#FF7A81")}
    if ("X1.3" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X1.3 %in% cut_value, j = "X1.3", color = "#FF7A81")}
    if ("X3.5" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X3.5 %in% cut_value, j = "X3.5", color = "#FF7A81")}
    if ("X5.10" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X5.10 %in% cut_value, j = "X5.10", color = "#FF7A81")} else{}
    if ("X10."%in% names(ft)) {ftout <- highlight(ftout, i = ~ X10. %in% cut_value, j = "X10.", color = "#FF7A81")}
    
    ftout <- ftout %>%
      font(fontname = "Microsoft YaHei", part = "header") %>%
      font(fontname = "Calibri", part = "body") %>%
      fontsize(size = 10, part = "all") %>%
      bold(bold=TRUE, part = "all") %>%
      set_header_labels(level1 = "", X0.1 = "0~1", X1.3 = "1~3",
                        X3.5 = "3~5", X5.10 = "5~10", X10. = "10+") %>%
      width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
      align(align = "center", part = "all")
    }
}


# 大类资产涨跌图
# date1, date2, date3
date_end <- "2021-01-10"
date_wk <- as.character(ymd(date_end) - 7)   # begin of last week
date_mt <-  "2020-12-31"  # begin of the month
date_yr <-  "2020-12-31" # begin of the year
date_min <- "2016-01-01"    # begin of the data
con3 <- dbConnect(SQLite(), "Macro_database.sqlite")

pct_yr <- as_tibble(dbReadTable(con3, "Equity")) %>%
  filter(date %in% c(date_yr, date_end)) %>%
  group_by(ID) %>%
  arrange("date") %>%
  mutate(pct_YTD = PX_LAST / lag(PX_LAST) - 1) %>%
  na.omit() %>%
  select(ID, PX_LAST, pct_YTD)
  
pct_mt <- as_tibble(dbReadTable(con3, "Equity")) %>%
  filter(date %in% c(date_mt, date_end)) %>%
  group_by(ID) %>%
  arrange("date") %>%
  mutate(pct_MTD = PX_LAST / lag(PX_LAST) - 1) %>%
  na.omit() %>%
  select(ID,pct_MTD)

pct_wk <- as_tibble(dbReadTable(con3, "Equity")) %>%
  filter(date %in% c(date_wk, date_end)) %>%
  group_by(ID) %>%
  arrange("date") %>%
  mutate(pct_WTD = PX_LAST / lag(PX_LAST) - 1) %>%
  na.omit() %>%
  select(ID, pct_WTD)

pct_sum <- pct_wk %>%
  left_join(pct_mt, by = "ID") %>%
  left_join(pct_yr, by = "ID")

# for (table in dbListTables(con3)) {
#   data <- dbReadTable(con3, table) %>%
#     mutate(date = as.Date(date),
#            date = as.character(date))
#   dbWriteTable(con3, table, data, overwrite = TRUE)
# }
asset_perform <- function(con3) {
  l = c()
  for (table in dbListTables(con3)) {
    data <- as_tibble(dbReadTable(con3, table))
    
    pct_yr <- as_tibble(dbReadTable(con3, "Equity")) %>%
      filter(date %in% c(date_yr, date_end)) %>%
      group_by(ID) %>%
      arrange("date") %>%
      mutate(pct_YTD = PX_LAST / lag(PX_LAST) - 1) %>%
      na.omit() %>%
      select(ID, PX_LAST, pct_YTD)
    
    pct_mt <- as_tibble(dbReadTable(con3, "Equity")) %>%
      filter(date %in% c(date_mt, date_end)) %>%
      group_by(ID) %>%
      arrange("date") %>%
      mutate(pct_MTD = PX_LAST / lag(PX_LAST) - 1) %>%
      na.omit() %>%
      select(ID, pct_MTD)
    
    pct_wk <- as_tibble(dbReadTable(con3, "Equity")) %>%
      filter(date %in% c(date_wk, date_end)) %>%
      group_by(ID) %>%
      arrange("date") %>%
      mutate(pct_WTD = PX_LAST / lag(PX_LAST) - 1) %>%
      na.omit() %>%
      select(ID, pct_WTD)
    
    pct_sum <- pct_wk %>%
      left_join(pct_mt, by = "ID") %>%
      left_join(pct_yr, by = "ID")
    l[[table]] = pct_sum
  }
  result <- bind_rows(l)
}
l = c()
for (table in dbListTables(con3)) {
  data <- as_tibble(dbReadTable(con3, table))
  
  pct_yr <- data %>%
    filter(date %in% c(date_yr, date_end)) %>%
    group_by(ID) %>%
    arrange("date") %>%
    mutate(pct_YTD = percent(PX_LAST / lag(PX_LAST) - 1)) %>%
    na.omit() %>%
    select(ID, pct_YTD)
  
  pct_mt <- data %>%
    filter(date %in% c(date_mt, date_end)) %>%
    group_by(ID) %>%
    arrange("date") %>%
    mutate(pct_MTD = percent(PX_LAST / lag(PX_LAST) - 1)) %>%
    na.omit() %>%
    select(ID, pct_MTD)
  
  pct_wk <- data %>%
    filter(date %in% c(date_wk, date_end)) %>%
    group_by(ID) %>%
    arrange("date") %>%
    mutate(pct_WTD = percent(PX_LAST / lag(PX_LAST) - 1),
           Asset = table) %>%
    na.omit() %>%
    select(Asset, Region, ID, PX_LAST, pct_WTD)
  
  pct_sum <- pct_wk %>%
    left_join(pct_mt, by = "ID") %>%
    left_join(pct_yr, by = "ID")
  l[[table]] = pct_sum
}
x <- c("Equity", "Rates", "Credit", "FX", "Commodity", "Volatility")
result <- bind_rows(l) %>%
  group_by(Asset) %>%
  arrange(desc(pct_WTD), .by_group = TRUE) %>%
  ungroup %>%
  # slice(match(x, Asset)) %>%
  flextable() %>%
  bold(part = 'all', bold = TRUE) %>%
  color(~ pct_WTD < 0, color = "red", ~ pct_WTD) %>%
  color(~ pct_MTD < 0, color = "red", ~ pct_MTD) %>%
  color(~ pct_YTD < 0, color = "red", ~ pct_YTD) %>%
  color(~ pct_WTD > 0, color = "#008000", ~ pct_WTD) %>%
  color(~ pct_MTD > 0, color = "#008000", ~ pct_MTD) %>%
  color(~ pct_YTD > 0, color = "#008000", ~ pct_YTD) %>%
  set_header_labels(ID = "指数名称", PX_LAST = "最新点位", Asset = "资产类别",
                    Region = "地区", pct_WTD = "涨跌幅", pct_MTD = "涨跌幅", 
                    pct_YTD = "涨跌幅") %>%
  merge_at(i=1:6, j=1, part = "body") %>%
  merge_at(i=7:20, j=1, part = "body") %>%
  merge_at(i=21:26, j=1, part = "body") %>%
  merge_at(i=27:33, j=1, part = "body") %>%
  merge_at(i=34:38, j=1, part = "body") %>%
  font(fontname = "Microsoft YaHei", part = "header") %>%
  font(fontname = "Calibri", part = "body") %>%
  fontsize(size = 10, part = "all") %>%
  
  theme_vanilla() %>%
  width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
  align(align="center", part="all")
  
  
con3 <- dbConnect(SQLite(), "Macro_database.sqlite")
region_map <- read_excel("map.xlsx")
dbWriteTable(con3, "map", region_map, overwrite = TRUE)
dbDisconnect(con3)
for (table in dbListTables(con3)) {
  data <- dbReadTable(con3, table) %>%
    left_join(region_map, by = "ID")
  dbWriteTable(con3, table, data, overwrite = TRUE)
}

data <- dbReadTable(con3, "Volatility") %>%
  mutate(ID = "VIX Index", Region = "VIX") %>%
  select(-region)
dbWriteTable(con3, "Volatility", data, overwrite = TRUE)





#==============================================================

con3 <- dbConnect(SQLite(), "Macro_database.sqlite")
con4 <- dbConnect(SQLite(), "Market_data.sqlite")
for (table in dbListTables(con4)) {
  data <- dbReadTable(con4, table)
  dbWriteTable(con3, table, data)
}



l = c()
for (table in c("Equity", "Rates", "Credit", "FX", "Commodity", "Volatility")) {
  data <- as_tibble(dbReadTable(con3, table))
  
  pct_yr <- data %>%
    filter(date %in% c(date_yr, date_end)) %>%
    group_by(ID) %>%
    arrange("date") %>%
    mutate(pct_YTD = percent(PX_LAST / lag(PX_LAST) - 1)) %>%
    na.omit() %>%
    select(ID, pct_YTD)
  
  pct_mt <- data %>%
    filter(date %in% c(date_mt, date_end)) %>%
    group_by(ID) %>%
    arrange("date") %>%
    mutate(pct_MTD = percent(PX_LAST / lag(PX_LAST) - 1)) %>%
    na.omit() %>%
    select(ID, pct_MTD)
  
  pct_wk <- data %>%
    filter(date %in% c(date_wk, date_end)) %>%
    group_by(ID) %>%
    arrange("date") %>%
    mutate(pct_WTD = percent(PX_LAST / lag(PX_LAST) - 1),
           Asset = table) %>%
    na.omit() %>%
    select(Asset, Region, ID, PX_LAST, pct_WTD)
  
  pct_sum <- pct_wk %>%
    left_join(pct_mt, by = "ID") %>%
    left_join(pct_yr, by = "ID")
  l[[table]] = pct_sum
}
x <- c("Equity", "Rates", "Credit", "FX", "Commodity", "Volatility")
result <- bind_rows(l) %>%
  mutate(Asset = factor(Asset, levels = x)) %>%
  # arrange(Asset)
  group_by(Asset) %>%
  arrange(desc(pct_WTD), .by_group = TRUE) %>%
  ungroup %>%
  # slice(match(x, Asset)) %>%
  flextable() %>%
  bold(part = 'all', bold = TRUE) %>%
  color(~ pct_WTD < 0, color = "red", ~ pct_WTD) %>%
  color(~ pct_MTD < 0, color = "red", ~ pct_MTD) %>%
  color(~ pct_YTD < 0, color = "red", ~ pct_YTD) %>%
  color(~ pct_WTD > 0, color = "#008000", ~ pct_WTD) %>%
  color(~ pct_MTD > 0, color = "#008000", ~ pct_MTD) %>%
  color(~ pct_YTD > 0, color = "#008000", ~ pct_YTD) %>%
  set_header_labels(ID = "指数名称", PX_LAST = "最新点位", Asset = "资产类别",
                    Region = "地区", pct_WTD = "涨跌幅", pct_MTD = "涨跌幅", 
                    pct_YTD = "涨跌幅") %>%
  merge_at(i=1:6, j=1, part = "body") %>%
  merge_at(i=7:20, j=1, part = "body") %>%
  merge_at(i=21:26, j=1, part = "body") %>%
  merge_at(i=27:33, j=1, part = "body") %>%
  merge_at(i=34:38, j=1, part = "body") %>%
  font(fontname = "Microsoft YaHei", part = "header") %>%
  font(fontname = "Calibri", part = "body") %>%
  fontsize(size = 10, part = "all") %>%
  
  theme_vanilla() %>%
  width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
  align(align="center", part="all")



# ===========================================
# fund flow
con3 <- dbConnect(SQLite(), "Macro_database.sqlite")

country_flow <- dbGetQuery(con3, "SELECT date, EM_bond, EM_ASIA_bond, CHINA_bond,
                          HONGKONG_bond, EM_EURO_bond, BRAZIL_bond, INDIA_bond,
                          RUSSIA_bond FROM Country_Flow") %>%
  mutate(date = as.Date(date)) %>%
  filter(date == max(date)) %>%
  select(-date)

t1 <- tibble(country = c("EM_bond", "EM_ASIA_bond", "CHINA_bond",
                         "EM_EURO_bond", "BRAZIL_bond", "RUSSIA_bond",
                         "INDIA_bond", "HONGKONG_bond"),
             country_cn = c("新兴市场", "新兴亚洲", "中国", "新兴欧洲",
                            "巴西", "俄罗斯", "印度", "香港"))

t <- as_tibble(cbind(country = names(country_flow), 
                     t(country_flow))) %>%
  mutate(V2 = round(as.double(V2), 1)) %>%
  arrange(desc(V2)) %>%
  left_join(t1, by = "country")



ggplot(t, aes(x=reorder(country_cn, V2), 
              y=V2,fill=country_cn)) +
  geom_bar(stat = 'identity') + 
  ylab("日期") +
  xlab("国家") +
  coord_flip() +
  theme_minimal() +
  theme(legend.position="none") +
  geom_text(aes(y=V2, label=V2), vjust=1, hjust = 0.6, 
            color="black", size=3.5)
  

plot1 <- ggplot(data = country_flow, aes(x = date)) +
  geom_bar(aes(y = EM_bond)) +
  geom_line(aes(y = EM_bond), color = "#D70C19", size = 1) +
  ylab("新兴市场资金净流入") +
  xlab("日期") +
  geom_line(aes(y = 0)) +
  ylim(-5000, 5000) +
  theme_classic() +
  theme(text=element_text(size=10,  family="Microsoft YaHei"))



line_dot_plot <- function(data, name) {
  
  return <- data %>%
    mutate(date = as.Date(date))
  
  index_plot <- ggplot(data = return, aes(x = date)) + 
    geom_line(aes(y = name), color = "#D70C19", size = 1) +
    ylab("return") +
    labs(title = print(name)) + 
    ylim(-5000, 5000) +
    theme_classic()
}



# ====================
data <- US_ETF_flow %>%
  mutate(date = as.Date(date),
         us_business_day = isBusinessDay("UnitedStates/NYSE", date)) %>%
  filter(date > as.Date("2020-12-11"), 
         ID == "HYG US Equity",
         us_business_day)

ggplot(data, aes(x = date)) +
  geom_bar(stat = "identity", aes(y = FUND_FLOW))

US_ETF_fundflow <- function(US_ETF_flow,
                            etfs = c("HYG US Equity", "EMB US Equity", 
                                     "LQD US Equity")) {
  l = c()
  for (etf in etfs) {
    data <- US_ETF_flow %>%
      mutate(date = as.Date(date),
             us_business_day = isBusinessDay("UnitedStates/NYSE", date)) %>%
      filter(date > ymd(max(date) - months(2)), 
             ID == etf,
             us_business_day)
    l[[etf]] <- ggplot(data, aes(x = date)) +
      geom_bar(stat = "identity", aes(y = FUND_FLOW)) +
      ylab(sprintf("%s 基金净流入", etf)) +
      xlab("日期")
  }
  return(l)
}




loadfonts(device = "win")
windowsFonts(Times = windowsFont("Times New Roman"))



theme_set(theme_bw(base_size=10, base_family=windowsFont("Times New Roman"))+
            theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank()))

country_flow <- country_flow %>%
  mutate(date = as.Date(date))
ggplot(data = country_flow, aes(x = date)) +
  geom_point(aes(y = EM_bond)) +
  geom_line(aes(y = EM_bond), color = "#D70C19", size = 1) +
  ylab("EM Market Bond Fund Inflow(USD million)") +
  xlab("Date") +
  geom_line(aes(y = 0)) +
  ylim(-5000, 5000) +
  theme_classic() +
  theme(text = element_text(size = 12, family = "Calibri"))






#===========================
#
result <- tran_kable1(Market$summary1[1], 6.5) %>%
  height_all(height = 1) %>%
  hrule(rule = "exact")
weekly_report <- read_docx(path = "word_reports/template1.docx") %>%
  # height_all(height = 0.25, part = "body") %>%
  body_add_flextable(value = result) %>%
  print(weekly_report, target = "word_reports/market_review.docx")



#

df <- read.csv("D:/Aldrich/Beautiful-Visualization-with-R-master/第1章 R语言编程与绘图基础/MappingAnalysis_Data.csv",
               header = TRUE)
ggplot(df, aes(x = SOD, y = tau, size = age)) +
  geom_point(shape = 21, color = "black", fill = "#FF0000",
             stroke = 0.25, alpha = 0.8) +
  scale_size(range = c(1,8))

ggplot(df, aes(SOD, tau, fill = age, size = age)) +
  geom_point(shape = 21, color = "black", stroke = 0.25, alpha = 0.8) +
  scale_size(range = c(1,8)) +
  scale_fill_distiller(palette = "Reds", direction = 0)

ggplot(df, aes(x = SOD, y = tau, fill = Class)) +
  geom_point(shape = 21, size = 3, color = "black", stroke = 0.25) +
  scale_fill_manual(values = c("#36BED9", "#FF0000", "#FBAD01")) +
  scale_shape_manual(values=c(21, 22, 23))

ggplot(df, aes(x = SOD, y = tau, fill = Class, size = age)) +
  geom_point(shape = 21, color = "black", stroke = 0.25, alpha = 0.8) +
  scale_fill_manual(values = c("#36BED9", "#FF0000", "#FBAD01")) +
  scale_size(range=c(1,7))

ggplot(data = df, aes(x = Time, y = value, group = variable)) +
  geom_line() +
  geom_point(shape = 21, size = 4, color = "black", fill = 'white') +
  theme_classic()

ggplot(data = df, aes(x = Time, y = value, fill = variable)) +
  geom_line() +
  geom_point(shape = 21, size = 4, color = "black") +
  scale_fill_manual(values = c("grey60", "grey30", "black", "white")) +
  theme_classic()

ggplot(data = df, aes(x = Time, y = value, shape = variable)) +
  geom_line() +
  geom_point(size = 4, color = "black", fill = "grey60") +
  scale_shape_manual(values = c(21, 22, 23, 24)) +
  theme_classic()

ggplot(data = df, aes(x = Time, y = value, fill = variable, 
                      shape = variable)) +
  geom_line() +
  geom_point(size = 4, color = "black") +
  scale_fill_manual(values = c("#FF9641", "#FF5B4E", "#B887C3",
                               "#38C25D")) +
  scale_shape_manual(values = c(21, 22, 23, 24)) + 
  theme_classic()

ggplot(data = df, aes(x = Time, y = value, fill = variable)) +
  geom_point(size = 4) +
  scale_fill_manual(values = c("grey60", "grey30", "black", "white")) +
  theme_classic()
