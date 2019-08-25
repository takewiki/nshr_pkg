library(nshrpkg);

#加班取整
data <- deal_jiaBan(file="data-raw/input/04-ns_jiaban_rds.xlsx");
data <- jiaBan_NameTransfer(data);
View(data);
