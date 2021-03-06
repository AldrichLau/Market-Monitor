source("market monitor dependency.R")

con1 <- dbConnect(RSQLite::SQLite(), "CNUSD.sqlite")
con2 <- dbConnect(RSQLite::SQLite(), "NEW_CNUSD.sqlite")

# 1. 数据库转换 -------------------------------------------------------------------

# 将原数据库创建一个新的数据库，名为NEWCNUSD
transfer_database(con1, con2) # 大约需要36秒

# 计算全部区间的价格数据并写入数据库，大约需要2分钟
cal_write_index(con2, bonds_info, hist_data, 
                cats = c("cat", "rating2", "Label"), method = "weight_wt")
cal_write_index1(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                 group = "rating2", method = "weight_wt", append = FALSE)
cal_write_index1(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                 group = "Label", method = "weight_wt", append = FALSE)



# 2. 数据库更新 -------------------------------------------------------------------

# 将hist_data, bonds_info等原始数据更新，大约需要3.8秒
update_newcnusd(con1, con2)

# 更新指数成分和价格数据，大约20-30秒
start_time = Sys.time()
update_index(con2, cats = c("cat", "rating2", "Label"), method = "weight_wt")
end_time = Sys.time()
end_time - start_time


dbDisconnect(con1)
dbDisconnect(con2)

