library(tsdo);
library(tsda);
buka <- readExcelDf('data-raw/input/02-ns_buKa.xlsx');
buka_name <- names(buka);
buka_name_selected <-c("审批编号","发起人姓名" ,"发起人部门" , "补卡班次" , "缺卡原因");
buka_sel <- buka %>%df_selectCol(buka_name_selected);
buka_shift <- buka_sel$补卡班次;
shiftdate <- left(buka_shift,10);
shiftNum <- mid(buka_shift,18,2);
buka_billno <- buka_sel$审批编号;
buka_name <- buka_sel$发起人姓名;
buka_dept <- buka_sel$发起人部门;
buka_reason <- buka_sel$缺卡原因;
data_buka <- data.frame(buka_billno,buka_name,buka_dept,shiftdate,shiftNum,buka_reason,
                        stringsAsFactors = F);

View(data_buka);

