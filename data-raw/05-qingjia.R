library(tsdo);
library(tsda);
#  超过1就算2，向上取整
#  区分事假与病假
qingj<- readExcelDf("data-raw/input/05-ns_qingjia.xlsx")
qingj_name <- names(qingj)
qingj_name_sel <-c("审批编号", "审批状态"   ,"发起人姓名","发起人部门",
                  "类型"     ,"开始时间",       "结束时间" ,      "时长","请假事由"  )
qingj_sel <- qingj %>% df_selectCol(qingj_name_sel);
names(qingj_sel) <- c('qj_billno','qj_status','qj_name','qj_dept',
                      'qj_type','qj_startTime','qj_endTime','qj_hours',
                      'qj_reason');
qingj_sel <- qingj_sel[qingj_sel$qj_status !='已撤销',]
startTime <- as.POSIXct(qingj_sel$qj_startTime);
endTime <- as.POSIXct(qingj_sel$qj_endTime);
time_diff_calc <- round(as.numeric(endTime-startTime)/3600,2);
qingj_sel$time_diff_cal <-time_diff_calc;

View(qingj_sel);
