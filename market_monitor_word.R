# setup -------------------------------------------------------------------
library(DT)
library(formattable)
library(treemap)
library(d3treeR)
library(knitr)
library(lubridate)
library(officer)
library(officedown)
library(magrittr)
library(ggpubr)
library(flextable)
library(rvest)

# if (dir.exists("materials")) {} else {dir.create("materials")}

source("market monitor dependency.R")
source("word_report_dependency.R")

con2 <- dbConnect(SQLite(), "NEW_CNUSD.sqlite")

# set the dates params
date_end <- "2020-12-11"
date_wk <- as.character(ymd(date_end) - 7)   # begin of last week
date_mt <-  "2020-11-30"  # begin of the month
date_yr <-  "2020-01-02" # begin of the year
date_min <- "2016-01-01"    # begin of the data
dates = list(date_end = date_end, date_wk = date_wk, date_mt = date_mt, date_yr = date_yr, date_min = date_min)

# fetch some dfs to ref
bonds_info = dbGetQuery(con2, "SELECT * FROM bonds_info WHERE date = (SELECT MAX(date) FROM bonds_info)")
bonds_info_tidy = tidy_bonds(bonds_info, "cat")

hist_data_this_year <- dbGetQuery(con2, sprintf("SELECT * FROM hist_data WHERE strftime('%%Y', date) = '%s';", as.character(year(ymd(date_end)))))
mapping_credit2 = dbReadTable(con2, "mapping_credit2")

index_cat_wt <- dbGetQuery(con2, sprintf("SELECT date, year, month, ID, cat AS Category, price, price_dirty, CPN, Amount, weight_wt, weight_eq FROM index_cat_wt WHERE date = '%s'", date_mt))
index_cat_price <- dbGetQuery(con2, sprintf("SELECT date, cat AS Category, YTM, spread, duration, DTS, index_price_net, index_price_total, cal_method FROM index_cat_price WHERE date >= '%s' AND date <= '%s'", date_min, date_end))

index_rating2_wt <-dbGetQuery(con2, sprintf("SELECT date, year, month, ID, rating2, price, price_dirty, CPN, Amount, weight_wt, weight_eq FROM index_rating2_wt WHERE date = '%s'", date_mt))
index_rating2_price <- dbGetQuery(con2, sprintf("SELECT date, rating2, YTM, spread, duration, DTS, index_price_net, index_price_total, cal_method FROM index_rating2_price WHERE date >= '%s' AND date <= '%s'", date_min, date_end))

index_Label_wt <-dbGetQuery(con2, sprintf("SELECT date, year, month, ID, Label, price, price_dirty, CPN, Amount, weight_wt, weight_eq FROM index_Label_wt WHERE date = '%s'", date_mt))
index_Label_price <- dbGetQuery(con2, sprintf("SELECT date, Label, YTM, spread, duration, DTS, index_price_net, index_price_total, cal_method FROM index_Label_price WHERE date >= '%s' AND date <= '%s'", date_min, date_end))

index_generic_wt <- dbGetQuery(con2, sprintf("SELECT date, year, month, ID, generic, price, price_dirty, CPN, Amount, weight_wt, weight_eq FROM index_generic_wt WHERE date = '%s'", date_mt))
index_generic_price <- dbGetQuery(con2, "SELECT date, generic, YTM, spread, duration, DTS, index_price_net, index_price_total, cal_method FROM index_generic_price WHERE date >= '2019-06-30'")

index_sum_table_Label <- dbGetQuery(con2, sprintf("SELECT date, cat AS Category, Label, YTM, spread, duration, DTS, index_price_net, index_price_total FROM index_sum_table_Label WHERE date in ('%s', '%s', '%s', '%s')", date_yr, date_mt, date_wk, date_end))
index_sum_table_Label1 <- dbGetQuery(con2, "SELECT date, cat AS Category, Label, YTM, spread, duration, DTS, index_price_net, index_price_total FROM index_sum_table_Label")
index_sum_table_rating2 <- dbGetQuery(con2, sprintf("SELECT date, cat AS Category, rating2, YTM, spread, duration, DTS, index_price_net, index_price_total FROM index_sum_table_rating2 WHERE date in ('%s', '%s', '%s', '%s')", date_yr, date_mt, date_wk, date_end))
index_sum_table_rating21 <- dbGetQuery(con2, "SELECT date, cat AS Category, rating2, YTM, spread, duration, DTS, index_price_net, index_price_total FROM index_sum_table_rating2")

index_wts <- list(cat = index_cat_wt, rating2 = index_rating2_wt, Label = index_Label_wt, generic = index_generic_wt)
index_prices <- list(cat = index_cat_price, rating2 = index_rating2_price, Label = index_Label_price, generic = index_generic_price)

data_bond <- bond(bonds_info, mapping_credit2)
snap_data <- snap(data_bond, index_cat_wt, hist_data_this_year, date_wk, date_end)
snap_data_MTD <- snap(data_bond, index_cat_wt, hist_data_this_year, date_mt, date_end)
snap_data_YTD <- snap(data_bond, index_cat_wt, hist_data_this_year, date_yr, date_end)
data <- list(snap_data = snap_data,
             snap_data_MTD = snap_data_MTD,
             snap_data_YTD = snap_data_YTD)

#mode <- 2

Market <- report2(bonds_info, mapping_credit2, data, index_wts, index_prices)

property <- report1(data_bond, snap_data, index_wts, index_prices, name = "HY Property")

IG <- report1(data_bond, snap_data, index_wts, index_prices, name = "IG Corporate")
Financials <- report1(data_bond, snap_data, index_wts, index_prices, name = "Financials")
LGFV <- report1(data_bond, snap_data, index_wts, index_prices, name = "LGFV")
Industry <- report1(data_bond, snap_data, index_wts, index_prices, name = "Industry")

AT1 <- report1(data_bond, snap_data, index_wts, index_prices, name = "AT1")
Tier2 <- report1(data_bond, snap_data, index_wts, index_prices, name = "Tier 2")
EM <- report1(data_bond, snap_data, index_wts, index_prices, name = "EM")

plot1 <- metric_plot2(index_prices$rating2, group = "rating2", name1 = "BB", name2 = "B", metric = "yield")
plot2 <- metric_plot2(index_prices$Label, "Label", "low beta", "high beta", "yield")
plot4 <- metric_plot2(index_prices$cat, "Category", "Tier 2", "AT1", "spread")

plot3 <- generic_plot(index_prices$generic)



# 1. 市场表现回顾 ---------------------------------------------------------------
run_num <- run_autonum(seq_id = "fig", pre_label = "图")
run_num_t <- run_autonum(seq_id = "table", pre_label = "表")
img_width1 = 6.5
img_width2 = 3.25
table_width = 6.5
# set_flextable_defaults(
#   font.family = "Calibri", font.size = 10, font.color = "black",
#   text.align = "centered")
#   # theme_fun = "theme_vanilla")


weekly_report <- read_docx(path = "word_reports/template1.docx") %>%
  body_add_par("周会", style = "Title") %>%
  # body_add_par("目录", style = "centered") %>%
  # body_add_toc(level = 3) %>%
  # body_add_break() %>% 
  
  body_add_par("一、主要资产类别回顾", style = "heading 1") %>%
  body_add_caption(block_caption("主要资产近一周涨跌幅汇总", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  body_add_img(src = "materials/主要资产类别回顾.png", 
               width = 7.5, height = 7.65,
               style = "centered")

  
# 2. 板块回顾---------------------------------------------------------------------

## 2.1 指数回顾 -------------------------------------------------------------
weekly_report <- weekly_report %>%
  body_add_par("二. 主要板块回顾", style = "heading 1") %>%
  body_add_par("1. 全市场概览", style = "heading 2") %>%
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  add_graph(Market$plot1, Market$plot2, img_width1, 3, "centered") %>%
  add_graph(Market$plot3, Market$plot4, img_width1, 3, "centered") %>%
  
  add_table1(index_cat_price, cat="", group="Category", table_width, 
             caption_title = "分板块估值", "table1", table_num = run_num_t) %>%
  add_table1(index_rating2_price, cat="", group="rating2", table_width, 
             caption_title = "分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_Label_price, cat="", group="Label", table_width, 
             caption_title = "分类别估值", "table1", table_num = run_num_t) %>%
  
  
  body_add_par("2) 分组数据概览", style = "heading 3") %>%
  add_table(Market$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(Market$summary2[1], "", "", table_width, "按综合评级分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(Market$top[1], Market$top_MTD[1], Market$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(Market$bottom[1], Market$bottom_MTD[1], Market$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)



## 2.2 HY Property-------------------------------------------------------------------------

weekly_report <- weekly_report %>%
  body_add_par("2. HY Property", style = "heading 2") %>%
  
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  add_graph(property$return, property$spread, img_width1, 3, "centered") %>%
  add_graph(property$yield, property$DTS, img_width1, 3, "centered") %>%
  add_graph(plot1, plot2, img_width1, 3, "centered") %>%
  
  add_table1(index_sum_table_rating21, cat="HY Property", group="rating2", table_width, 
             caption_title = "分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_sum_table_Label1, cat="HY Property", group="Label", table_width, 
             caption_title = "分类别估值", "table1", table_num = run_num_t) %>%
 
  body_add_par("2) 分组数据概览", style = "heading 3") %>%
  add_table(property$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(property$summary2[1], "", "", table_width, "按综合评级分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(property$top[1], property$top_MTD[1], property$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(property$bottom[1], property$bottom_MTD[1], property$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)
  

## 2.3. IG Corporate ---------------------------------------------------------

weekly_report <- weekly_report %>%
  body_add_par("3. IG Corporate", style = "heading 2") %>%
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  
  add_graph(IG$return, IG$spread, img_width1, 3, "centered") %>%
  add_graph(IG$yield, IG$DTS, img_width1, 3, "centered") %>%
  
  add_table1(index_sum_table_rating21, cat="IG Corporate", group="rating2", table_width, 
             caption_title = "分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_sum_table_Label1, cat="IG Corporate", group="Label", table_width, 
             caption_title = "分类别估值", "table1", table_num = run_num_t) %>%
  
  
  body_add_par("2) 分组数据概览", style = "heading 3") %>%
  add_table(IG$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(IG$summary2[1], "", "", table_width, "按综合评级分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(IG$top[1], IG$top_MTD[1], IG$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(IG$bottom[1], IG$bottom_MTD[1], IG$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)



## 2.4 Financials -----------------------------------------------------------
weekly_report <- weekly_report %>%
  body_add_par("4. Financials", style = "heading 2") %>%
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  
  
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  add_graph(Financials$return, Financials$spread, img_width1, 3, "centered") %>%
  add_graph(Financials$yield, Financials$DTS, img_width1, 3, "centered") %>%
  
  add_table1(index_sum_table_rating21, cat="IG Corporate", group="rating2", table_width, 
             caption_title = "分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_sum_table_Label1, cat="IG Corporate", group="Label", table_width, 
             caption_title = "分类别估值", "table1", table_num = run_num_t) %>%
  
  
  body_add_par("2) 分组数据概览", style = "heading 3") %>%
  add_table(Financials$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(Financials$summary2[1], "", "", table_width, "按综合评级分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(Financials$top[1], Financials$top_MTD[1], Financials$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(Financials$bottom[1], Financials$bottom_MTD[1], Financials$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)

## 2.5 LGFV -----------------------------------------------------------------

weekly_report <- weekly_report %>%
  body_add_par("5. LGFV", style = "heading 2") %>%
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  add_graph(LGFV$return, LGFV$spread, img_width1, 3, "centered") %>%
  add_graph(LGFV$yield, LGFV$DTS, img_width1, 3, "centered") %>%
  
  add_table1(index_sum_table_rating21, cat="LGFV", group="rating2", table_width, 
             caption_title = "分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_sum_table_Label1, cat="LGFV", group="Label", table_width, 
             caption_title = "分类别估值", "table1", table_num = run_num_t) %>%
  
  body_add_par("2) 分组数据概览", style = "heading 3") %>%
  add_table(LGFV$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(LGFV$top[1], LGFV$top_MTD[1], LGFV$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(LGFV$bottom[1], LGFV$bottom_MTD[1], LGFV$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)


# 2.6 Industry ------------------------------------------------------------
weekly_report <- weekly_report %>%
  body_add_par("6. Industry", style = "heading 2") %>%
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  add_graph(Industry$return, Industry$spread, img_width1, 3, "centered") %>%
  add_graph(Industry$yield, Industry$DTS, img_width1, 3, "centered") %>%
  
  add_table1(index_sum_table_rating21, cat="Industry", group="rating2", table_width, 
             caption_title = "分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_sum_table_Label1, cat="Industry", group="Label", table_width, 
             caption_title = "分类别估值", "table1", table_num = run_num_t) %>%
  
  body_add_par("2) 分组数据概览", style = "heading 3") %>%
  add_table(Industry$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(Industry$top[1], Industry$top_MTD[1], Industry$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(Industry$bottom[1], Industry$bottom_MTD[1], Industry$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)


## 2.7 Bank Capital --------------------------------------------------------
weekly_report <- weekly_report %>%
  body_add_par("7. Bank Capital", style = "heading 2") %>%
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  add_graph(AT1$return, Tier2$return, img_width1, 3, "centered") %>%
  add_graph(AT1$spread, Tier2$spread, img_width1, 3, "centered") %>%
  add_graph(AT1$yield, Tier2$yield, img_width1, 3, "centered") %>%
  body_add_gg(value = plot4, style = "left", width = img_width1/2, height = 3) %>%
  add_table1(index_sum_table_rating21, cat="AT1", group="rating2", table_width, 
             caption_title = "AT1分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_sum_table_Label1, cat="AT1", group="Label", table_width, 
             caption_title = "AT1分类别估值", "table1", table_num = run_num_t) %>%
  # add_table1(index_sum_table_rating21, cat="Tier2", group="rating2", table_width, 
  #            caption_title = "Tier 2分评级估值", "table1", table_num = run_num_t) %>%
  # add_table1(index_sum_table_Label1, cat="Tier2", group="Label", table_width, 
  #            caption_title = "Tier 2分类别估值", "table1", table_num = run_num_t) %>%


  body_add_par("2) AT1分组数据概览", style = "heading 3") %>%
  add_table(AT1$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(AT1$top[1], AT1$top_MTD[1], AT1$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(AT1$bottom[1], AT1$bottom_MTD[1], AT1$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2) %>%
  
  body_add_par("3) Tier2分组数据概览", style = "heading 3") %>%
  add_table(Tier2$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(Tier2$top[1], Tier2$top_MTD[1], Tier2$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(Tier2$bottom[1], Tier2$bottom_MTD[1], Tier2$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)


## 2.8 EM -------------------------------------------------------------------
weekly_report <- weekly_report %>%
  body_add_par("8.EM", style = "heading 2") %>%
  body_add_par("1) 指数与估值", style = "heading 3") %>%
  body_add_caption(block_caption("收益与估值", 
                                 style = "chart1",
                                 autonum = run_num)) %>%
  add_graph(EM$return, EM$spread, img_width1, 3, "centered") %>%
  add_graph(EM$yield, EM$DTS, img_width1, 3, "centered") %>%
  add_table1(index_sum_table_rating21, cat="EM", group="rating2", table_width, 
             caption_title = "分评级估值", "table1", table_num = run_num_t) %>%
  add_table1(index_sum_table_Label1, cat="EM", group="Label", table_width, 
             caption_title = "分类别估值", "table1", table_num = run_num_t) %>%
  
  body_add_par("2) 分组数据概览", style = "heading 3") %>%
  add_table(EM$summary1[1], "", "", table_width, "按类别分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(EM$summary2[1], "", "", table_width, "按综合评级分组",
            table_style = "table1", table_num = run_num_t, type=1) %>%
  add_table(EM$top[1], EM$top_MTD[1], EM$top_YTD[1],
            table_width, "涨幅前十", "table1", run_num_t, 2) %>%
  add_table(EM$bottom[1], EM$bottom_MTD[1], EM$bottom_YTD[1],
            table_width, "跌幅前十", "table1", run_num_t, 2)
  

# 3. 输出 -------------------------------------------------------------------
print(weekly_report, target = "word_reports/market_review.docx")
