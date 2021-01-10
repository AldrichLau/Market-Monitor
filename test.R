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

