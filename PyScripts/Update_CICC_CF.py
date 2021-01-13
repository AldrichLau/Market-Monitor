# -*- coding: utf-8 -*-
"""
Spyder Editor
Aldrich
To update data from CICC Excel document
"""

import pandas as pd
import sqlite3
import os
import glob
import shutil

wd = "C:/Users/ex-qliu/Documents/GitHub/Market-Monitor"
file_path = "materials/EPFR_Weekly report_CICC FI_*.xlsx"
database = "Macro_database.sqlite"
os.chdir(wd)
fname = glob.glob(file_path)[0]

## 操作表Fund Flow
df = pd.read_excel(fname, sheet_name = "Country Flow", skiprows=2, 
                   engine='openpyxl')
df = df.loc[:, [i for i in df.columns if not ("Unnamed" in str(i))]]
df.dropna(inplace=True, how='all')
df = df.set_index(df.columns[0])
df = df.T
df = df.loc[:, df.columns != '单位：百万美元']
df.dropna(inplace=True)

nm1 = ['total', 'EM', 'EM_ASIA', 'CHINA', 'HONGKONG', 'EM_EURO', 
       'BRIC', 'BRAZIL', 'INDIA', 'RUSSIA', 'DEVELOPED', 
       'US', 'JAPAN', 'DEVELOPED_EURO', 'GERMANY', 'UK', 'OTHERS'] 
cols = [(i + "_equity") for i in nm1] + ['BOND_CF（mn USD）'] + \
    [(i + "_bond") for i in nm1] + ['FUND_FLOW（mn USD）'] + [(i + "_total") for i in nm1]

df.columns = cols
df.index = [i.strftime("%Y-%m-%d") for i in df.index]
df['UNIT'] = "MM USD"

con = sqlite3.connect(database)
start_date = pd.read_sql_query("SELECT Max(date) FROM Country_Flow", con)['Max(date)'].values[0]
df = df.loc[df.index>start_date, ]
df.to_sql(name = "Country_Flow", con = con, index_label = "date", 
          if_exists = "append")


## 操作表CF主要国家对比
df1 = pd.read_excel(fname, sheet_name = "CF主要国家对比", skiprows=1, 
                    usecols = "B:T", engine='openpyxl', index_col = 0)
df1 = df1.iloc[1:,:]
df1.dropna(how="all", inplace=True)
df1.index = [i.strftime("%Y-%m-%d") for i in df1.index]
cols = ['bond_net_inflow_CHINA', 'bond_net_inflow_US',
        'bond_net_inflow_EURO', 'bond_net_inflow_JAPAN',
        'equity_net_inflow_CHINA', 'equity_net_inflow_HOKONG',
        'equity_net_inflow_US', 'equity_net_inflow_EURO', 
        'equity_net_inflow_JAPAN']
df1.columns = (cols + [i + "_4WMA" for i in cols])
df1['UNIT'] = "MM USD"
df1.sort_index(inplace=True)
start_date = pd.read_sql_query("SELECT Max(date) FROM Net_CF_By_Country", con)['Max(date)'].values[0]
df1 = df1.loc[df1.index>start_date, ]

df1.to_sql(name = "Net_CF_By_Country", con = con, index_label = "date", 
          if_exists = "append")

con.close()


# 把用过的文件放入文件夹归档
shutil.move(fname, os.path.split(fname)[0] + "/used/" + os.path.split(fname)[1])
