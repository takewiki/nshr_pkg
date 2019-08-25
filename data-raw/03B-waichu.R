library(tsdo);
library(tsda);
waichu <- readExcelDf("data-raw/input/06-ns_waichu.xlsx")
waichu_name <- names(waichu);
waichu_name_sel <-c("审批编号", "审批状态","发起人姓名","发起人部门",
                    "开始时间","结束时间","外出时间","外出事由")
waichu_sel <- waichu %>%df_selectCol(waichu_name_sel);
names(waichu_sel) <-c('wc_billno','wc_status','wc_name','wc_dept',
                      'wc_startDate','wc_endDate','wc_hours','wc_reason')
waichu_sel$wc_hours_calc <- date_diff(waichu_sel$wc_startDate,waichu_sel$wc_endDate,unit = 'h',hourPerDay = 8,is.hr = TRUE);

View(waichu_sel);
