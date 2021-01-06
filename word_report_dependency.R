library(flextable)
library(rvest)

# html_table = Market$summary1[1]
# ×ª»»market$summary
tran_kable1 <- function(html_table, page_width=7.5) {
  ftout <- as.data.frame(read_html(html_table) %>% html_table(fill=TRUE)) %>%
    flextable() %>%
    autofit() %>%
    width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
    align(align = "center", part = "all")
  ftout
}


# ×ª»»duration matrix
# html_table = LGFV$summary8[1]
tran_kable2 <- function(html_table, page_width=7.5) {
  ftout <- as.data.frame(read_html(html_table) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, "")))
  
  if ("X0.1" %in% names(ftout)) {rename(ftout, "0~1" = "X0.1")}
  if ("X1.3" %in% names(ftout)) {rename(ftout, "1~3" = "X1.3")}
  if ("X3.5" %in% names(ftout)) {rename(ftout, "3~5" = "X3.5")}
  if ("X5.10" %in% names(ftout)) {rename(ftout, "5~10" = "X5.10")}
  if ("X10." %in% names(ftout)) {rename(ftout, "10+" = "X10.")}
  
  ftout <- ftout %>%
    flextable() %>%
    autofit() %>%
    width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
    align(align = "center", part = "all")
  ftout
}


# 
tran_kable3 <- function(html_table, page_width=7.5) {
  ftout <- as.data.frame(read_html(html_table) %>% html_table(fill=TRUE)) %>%
    mutate(across(everything(), ~replace_na(.x, "")))
  names(ftout) <- c("level1", "0~1", "1~3", "3~5")
  ftout <- ftout %>%
    flextable() %>%
    autofit() %>%
    width(width = dim(.)$widths * page_width /(flextable_dim(.)$widths)) %>%
    align(align = "center", part = "all")
  ftout
}