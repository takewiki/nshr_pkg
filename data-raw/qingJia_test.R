library(nshrpkg);
data <- deal_qingJia(file="data-raw/input/05-ns_qingjia_rds.xlsx");
data <- qingJia_NameTransfer(data);
View(data);
