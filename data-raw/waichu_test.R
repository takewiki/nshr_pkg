library(nshrpkg);

data <- deal_waiChu();
data <- waiChu_NameTransfer(data);
data <- waiChu_splitIntoMultiRows(data = data);

#class(data$FHours);
View(data);
