# -*- coding: utf-8 -*-
"""
Spyder Editor
Aldrich
To extract data from CICC Excel document
"""

import pandas as pd
import sqlite3
import os
import glob
import shutil

wd = "C:/Users/ex-qliu/Documents/GitHub/Market-Monitor"
file_path = "materials/EPFR_Weekly report_CICC FI_*.xlsx"
database = "Market_data.sqlite"
os.chdir(wd)
fname = glob.glob(file_path)[0]

## 操作表Fund Flow
df = pd.read_excel(fname, sheet_name = "Fund Flow", skiprows=2, 
                   engine='openpyxl')
df = df.loc[:, [i for i in df.columns if not ("Unnamed" in str(i))]]
df.dropna(inplace=True, how='all')
df = df.set_index(df.columns[0])
df = df.T
df = df.loc[:, df.columns != '单位：百万美元']

nm1 = ['合计', '新兴市场', '中国', '中国香港', '金砖四国', '巴西', '印度', 
       '俄罗斯', '发达国家', '美国', '日本', '西欧'] 
cols = [(i + "_equity") for i in nm1] + ['债券市场资金流向（mn USD）'] + \
    [(i + "_bond") for i in nm1] + ['资金流向（mn USD）'] + [(i + "_total") for i in nm1]

df.columns = cols
df.index = [i.strftime("%Y-%m-%d") for i in df.index]
df['单位'] = "百万美元"

con = sqlite3.connect(database)
df.to_sql(name = "Fund_Flow", con = con, index_label = "date", 
          if_exists = "replace")


## 操作表CF主要国家对比
df1 = pd.read_excel(fname, sheet_name = "CF主要国家对比", skiprows=1, 
                    usecols = "B:T", engine='openpyxl', index_col = 0)
df1 = df1.iloc[1:,:]
df1.dropna(how="all", inplace=True)
df1.index = [i.strftime("%Y-%m-%d") for i in df1.index]
df1['单位'] = "百万美元"
df1.sort_index(inplace=True)
df1.to_sql(name = "Net_CF_By_Country", con = con, index_label = "date", 
          if_exists = "replace")

con.close()


# 把用过的文件放入文件夹归档
shutil.move(fname, os.path.split(fname)[0] + "/used/" + os.path.split(fname)[1])
