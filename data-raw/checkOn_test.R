
library(nshrpkg);
data1 <- checkOn_all(file = "./data-raw/input/");
openxlsx::write.xlsx(data1,"./1907checkOn.xlsx")

