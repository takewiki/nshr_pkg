library(tsdo);
library(tsda);
jiab <- readExcelDf("data-raw/input/04-ns_jiaban.xlsx");
jia_name <- names(jiab);
jia_name_sel <-c("审批编号","审批状态" ,"发起人姓名"   ,"发起人部门" , "开始时间"  ,
                 "结束时间" ,"加班时长（1小时起）" ,"是否法定假日","加班核算方式" ,
                 "加班原因"
                 )
jiab_sel <- jiab %>% df_selectCol(jia_name_sel);
names(jiab_sel) <-c('jb_billno','jb_status','jb_name','jb_dept',
                    'jb_startTime','jb_endtime','jb_hours','jb_legal','jb_acct','jb_reason')
jiab_sel <- jiab_sel[jiab_sel$jb_status !='已撤销',];
jb_startTime <- as.POSIXct(jiab_sel$jb_startTime);
jb_endTime <- as.POSIXct(jiab_sel$jb_endtime);
time_diff_calc <- round(as.numeric(jb_endTime-jb_startTime)/3600,2);
jiab_sel$time_diff_calc <-time_diff_calc;

View(jiab_sel)
