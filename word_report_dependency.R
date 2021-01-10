library(flextable)
library(rvest)
library(zoo)
library(formattable)

# 计算指数的估值分位数
# Category
# index_cat_price
# cat = ""
# index_price = index_Label_price
# group = "Label"
# val_quantile(index_Label_price, "", "Label")
val_quantile <- function(index_price, cat, group, table_width) {
  if (cat == "") {index_price <- index_price} else {
    index_price <- index_price %>% filter(Category == cat)
  }
  result <- index_price %>%
    group_by(!!sym(group)) %>%
    arrange(!!sym(group), date) %>%
    mutate(YTM_q_all = ntile(YTM, 1000)/1000,
           spread_q_all = ntile(YTM, 1000)/1000) %>%
    
    filter(date>ymd(max(date))-years(3)) %>%
    mutate(YTM_q_3y = ntile(YTM, 1000)/1000,
           spread_q_3y = ntile(YTM, 1000)/1000) %>%
    
    filter(date>ymd(max(date))-years(1)) %>%
    mutate(YTM_q_1y = ntile(YTM, 200)/200,
           spread_q_1y = ntile(YTM, 200)/200) %>%
    filter(date == max(date)) %>%
    arrange(!!sym(group)) %>%
    ungroup() %>%
    select(!!sym(group), YTM, YTM_q_1y, YTM_q_3y, YTM_q_all, 
           spread, spread_q_1y, spread_q_3y, spread_q_all) %>%
    mutate(YTM_q_1y = percent(YTM_q_1y, 0), YTM_q_3y = percent(YTM_q_3y, 0),
           YTM_q_all = percent(YTM_q_all, 0), spread_q_1y = percent(spread_q_1y, 0),
           spread_q_3y = percent(spread_q_3y, 0), spread_q_all = percent(spread_q_all, 0)) %>%
    
    flextable() %>%
    color(~ YTM_q_1y < 0.2, color = "#008000", ~ YTM_q_1y) %>%
    color(~ YTM_q_1y > 0.8, color = "red", ~ YTM_q_1y) %>%
    color(~ YTM_q_3y < 0.2, color = "#008000", ~ YTM_q_3y) %>%
    color(~ YTM_q_3y > 0.8, color = "red", ~ YTM_q_3y) %>%
    color(~ YTM_q_all < 0.2, color = "#008000", ~ YTM_q_all) %>%
    color(~ YTM_q_all > 0.8, color = "red", ~ YTM_q_all) %>%
    
    color(~ spread_q_1y < 0.2, color = "#008000", ~ spread_q_1y) %>%
    color(~ spread_q_1y > 0.8, color = "red", ~ spread_q_1y) %>%
    color(~ spread_q_3y < 0.2, color = "#008000", ~ spread_q_3y) %>%
    color(~ spread_q_3y > 0.8, color = "red", ~ spread_q_3y) %>%
    color(~ spread_q_all < 0.2, color = "#008000", ~ spread_q_all) %>%
    color(~ spread_q_all > 0.8, color = "red", ~ spread_q_all) %>%
    
    
    add_header_row(values = c("", "", "到期收益率历史百分位", "到期收益率历史百分位", 
                              "到期收益率历史百分位", "", "信用利差历史百分位", 
                              "信用利差历史百分位", "信用利差历史百分位")) %>%
    merge_at(i = 1, j = c(3,4,5), part = 'header') %>%
    merge_at(i = 1, j = c(7,8,9), part = 'header') %>%
    set_header_labels(Category = "分组", rating2 = "分组", Label = "分组", 
                      YTM = "到期收益率", YTM_q_1y = "1年", YTM_q_3y = "3年",
                      YTM_q_all = "全部", spread = "信用利差", spread_q_1y = "1年", spread_q_3y = "3年",
                      spread_q_all = "全部") %>%
    font(fontname = "Microsoft YaHei", part = "header") %>%
    font(fontname = "Calibri", part = "body") %>%
    fontsize(size = 10, part = "all") %>%
    
    theme_vanilla() %>%
    width(width = dim(.)$widths * table_width /(flextable_dim(.)$widths)) %>%
    align(align="center", part="all") %>%
    bold(part="all", bold=TRUE)
    # autofit()
  return(result)
}



# 将周月年的table拼一起
summary_wmy <- function(html_w, html_m, html_y) {
  w <- as.data.frame(read_html(html_w) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, ""))) %>%
    select(Name, price_chg, spread_chg)
  m <- as.data.frame(read_html(html_m) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, ""))) %>%
    select(Name, price_MTD = price_chg, spread_MTD = spread_chg)
  y <- as.data.frame(read_html(html_y) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, ""))) %>%
    select(Name, price_YTD = price_chg, spread_YTD = spread_chg)
  result <- w %>% bind_cols(m) %>% bind_cols(y)
  return(result)
}


# html_table = property$summary1[1]
# html_table = Market$top[1]
# tran_kable1(html_table, 6.5, 1)
# 转换market$summary，mode1是单分组统计表，
# mode1是summary1，mode2是top&bottom
tran_kable1 <- function(html_table, page_width=6.5) {
  ft <- as.data.frame(read_html(html_table) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, ""))) %>%
    select(1, price_chg, price_MTD, price_YTD, spread_chg, spread_MTD, 
           spread_YTD)
  ftout <- ft %>%
    flextable() %>%
    bold(part = 'all', bold = TRUE) %>%
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

    set_header_labels(Category = "", rating2 = "", Label = "",
                      price_chg = "涨跌幅", price_MTD = "涨跌幅",
                      price_YTD = "涨跌幅", spread_chg = "信用利差变动",
                      spread_MTD = "信用利差变动", spread_YTD = "信用利差变动") %>%
    add_header_row(values = c("分组", "本周", "本月", "本年", "本周", "本月", "本年"),
                   top = FALSE) %>%
    merge_at(i=1, j=2:4, part = "header") %>%
    merge_at(i=1, j=5:7, part = "header") %>%
    font(fontname = "Microsoft YaHei", part = "header") %>%
    font(fontname = "Calibri", part = "body") %>%
    fontsize(size = 10, part = "all") %>%

    theme_vanilla() %>%
    width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
    align(align="center", part="all")
    # autofit()
  return(ftout)
}


# 把涨跌幅排名周月年合并后的表格转化成flextable
# html_w = Market$bottom[1]
# html_m = Market$bottom_MTD[1]
# html_y = Market$bottom_YTD[1]
tran_kable_top_bot <- function(html_w, html_m, html_y, page_width) {
  ftout <- summary_wmy(html_w, html_m, html_y) %>%
    flextable() %>%
    color(~ price_chg < 0, color = "red", ~ price_chg) %>%
    color(~ price_chg > 0, color = "#008000", ~ price_chg) %>%
    color(~ price_MTD < 0, color = "red", ~ price_MTD) %>%
    color(~ price_MTD > 0, color = "#008000", ~ price_MTD) %>%
    color(~ price_YTD < 0, color = "red", ~ price_YTD) %>%
    color(~ price_YTD > 0, color = "#008000", ~ price_YTD) %>%
    
    color(~ spread_chg < 0, color = "red", ~ spread_chg) %>%
    color(~ spread_chg > 0, color = "#008000", ~ spread_chg) %>%
    color(~ spread_MTD < 0, color = "red", ~ spread_MTD) %>%
    color(~ spread_MTD > 0, color = "#008000", ~ spread_MTD) %>%
    color(~ spread_YTD < 0, color = "red", ~ spread_YTD) %>%
    color(~ spread_YTD > 0, color = "#008000", ~ spread_YTD) %>%
    
    set_header_labels(Name...1 = "债券简称", Name...4 = "债券简称", Name...7 = "债券简称",
                      price_chg = "涨跌幅", price_MTD = "涨跌幅", price_YTD = "涨跌幅",
                      spread_chg = "信用利差变化", spread_MTD = "信用利差变化", spread_YTD = "信用利差变化") %>%
    add_header_row(values = c("", "本周", "本周", "", "本月", "本月", "", "本年", "本年"), top = FALSE) %>%
    merge_at(i = 2, j = 2:3, part = "header") %>%
    merge_at(i = 2, j = 5:6, part = "header") %>%
    merge_at(i = 2, j = 8:9, part = "header") %>%
    font(fontname = "Microsoft YaHei", part = "header") %>%
    font(fontname = "Calibri", part = "body") %>%
    fontsize(size = 10, part = "all") %>%  
    bold(j = c(2, 3, 5, 6, 8, 9), bold = TRUE) %>%
    bold(part = "header", bold = TRUE) %>%
    theme_vanilla() %>%
    width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
    align(align="center", part="all")
    # autofit()
  return(ftout)
}
  
  


# 转换duration matrix
# html_table = Market$summary4[1]
# html_table = property$summary6[1]
# mode = 2
tran_kable2 <- function(html_table, page_width=6.5, mode=2) {
  ft <- as.data.frame(read_html(html_table) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, "")))
  
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
  } else {
    ft1 <- ft %>%
      pivot_longer(cols = c(2:length(names(ft))), 
                   names_to = "duration",
                   values_to = "spread") %>%
      mutate(spread = as.double(spread),
             quants = ntile(spread, n = 5)) %>%
      filter(quants >= 4) %>%
      select(spread)
    cut_value <<- ft1$spread
    # print(cut_value)
    
    ftout <- ft %>%
      flextable()
    if ("X0.1" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X0.1 %in% cut_value, j = "X0.1", color = "#FF7A81")}
    if ("X1.3" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X1.3 %in% cut_value, j = "X1.3", color = "#FF7A81")}
    if ("X3.5" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X3.5 %in% cut_value, j = "X3.5", color = "#FF7A81")}
    if ("X5.10" %in% names(ft)) {ftout <- highlight(ftout, i = ~ X5.10 %in% cut_value, j = "X5.10", color = "#FF7A81")}
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
      # autofit()
    # return(ftout)
  }
  return(ftout)
}



# 向word中插入图表的函数 -----------------------------------------------------------

# 新增双图
add_graph <- function(weekly_report, plot1, plot2, width, height, style) {
  weekly_report <- weekly_report %>%
    body_add_gg(value = ggarrange(plot1, plot2, ncol = 2, nrow = 1), 
              style = style, width = width, height = height)
}

# 新增type1是summary类表格，type2是matrix类表格
add_table <- function(weekly_report, html_w, html_m, html_y, table_width, caption_title, 
                      table_style, table_num, type=1) {
  
  if (type==1) {ftout <- tran_kable1(html_w, table_width)}
  if (type==2) {ftout <- tran_kable_top_bot(html_w, html_m, html_y, table_width)}
  
  weekly_report <- weekly_report %>%
    body_add_caption(block_caption(caption_title, 
                                   style = table_style,
                                   autonum = table_num)) %>%
    body_add_flextable(value = ftout)
}

# 新增matrix类表格
add_table1 <- function(weekly_report, index_price, cat, group, table_width, caption_title,
                       table_style, table_num) {
  ftout <- val_quantile(index_price, cat, group, table_width)
  weekly_report <- weekly_report %>%
    body_add_caption(block_caption(caption_title, 
                                   style = table_style,
                                   autonum = table_num)) %>%
    body_add_flextable(value = ftout)
}
