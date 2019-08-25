library(tsdo);
library(tsda);
#如果当天有出差记录，不取当天考勤记录

cc <- readExcelDf("data-raw/input/03-ns_chuchai.xlsx");
cc_name <- names(cc);
cc_name_selected <-c("审批编号","审批状态" ,"发起人姓名" , "发起人部门" ,"开始时间"  ,"结束时间","时长"  , "出差事由" )
cc_selected <- cc %>% df_selectCol(cc_name_selected);
names(cc_selected) <- c('cc_billno','cc_status','cc_name','cc_dept',
                        'cc_startDate','cc_endDate','cc_days','cc_reason')
cc_selected <- cc_selected[cc_selected$cc_status =='完成',]
View(cc_selected);
