# Market-Monitor
## The framework
"market monitor dependency.R" contains all the functions used to manipulate the database and generate the report.
The file has been separated into several parts, each part take a kind of function.

## 数据库的操作
1. 更新数据，打开"manipulate database.R"，将source和CNUSD database放入working directory。按照需要执行数据库转换或者数据库更新操作即可。数据库转换是基于现有CNUSD.sqlite生成NEWCNUSD.sqlite。数据库更新是基于CNUSD.sqlite中的最新数据，对NEWCNUSD进行更新。

2. "market_monitor.Rmd"是用于生成html版本的market_report。

3. "market_monitor_word.R"用于输出word版本的report，source文件为"market monitor dependency.R" and "word_report_dependency.R"。使用时需要将word_reports文件夹及其中的template放在working directory下。
