source("market monitor dependency.R")

con1 <- dbConnect(RSQLite::SQLite(), "CNUSD.sqlite")
con2 <- dbConnect(RSQLite::SQLite(), "NEW_CNUSD.sqlite")

# 1. ���ݿ�ת�� -------------------------------------------------------------------

# ��ԭ���ݿⴴ��һ���µ����ݿ⣬��ΪNEWCNUSD
transfer_database(con1, con2) # ��Լ��Ҫ36��

# ����ȫ������ļ۸����ݲ�д�����ݿ⣬��Լ��Ҫ2����
cal_write_index(con2, bonds_info, hist_data, 
                cats = c("cat", "rating2", "Label"), method = "weight_wt")
cal_write_index1(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                 group = "rating2", method = "weight_wt", append = FALSE)
cal_write_index1(con2, bonds_info, hist_data, cats = c("EM", "IG Corporate", "Financials", "Tier 2", "LGFV", "AT1", "Industry", "HY Property"),
                 group = "Label", method = "weight_wt", append = FALSE)



# 2. ���ݿ���� -------------------------------------------------------------------

# ��hist_data, bonds_info��ԭʼ���ݸ��£���Լ��Ҫ3.8��
update_newcnusd(con1, con2)

# ����ָ���ɷֺͼ۸����ݣ���Լ20-30��
start_time = Sys.time()
update_index(con2, cats = c("cat", "rating2", "Label"), method = "weight_wt")
end_time = Sys.time()
end_time - start_time


dbDisconnect(con1)
dbDisconnect(con2)
