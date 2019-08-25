
library(nshrpkg);
library(tsda);
library(tsdo);
library(reshape2);
#base_dir <-"./data-raw/input/";

#  考勤数据 ------
# data1 <- checkOn_all(file = nshr("01-kaoQing1907.xlsx"));
# View(data1)
data1a <- checkOn_all(file = nshr("01-kaoQing1907_817.xlsx"),
                      AmCheckTime = '8:00:00',
                      AmStartTime = '7:00:00',
                      pmCheckTime = '17:00:00'
                      );
#View(data1a)
data1b <- checkOn_all(file = nshr("01-kaoQing1907_918.xlsx"),
                      AmCheckTime = '9:00:00',
                      pmCheckTime = '18:00:00');
#View(data1b)
data1c <- checkOn_all(file = nshr("01-kaoQing1907_9p18p.xlsx"),
                      AmCheckTime = '9:30:00',
                      pmCheckTime = '18:30:00',
                      pmEndTime = '19:00:00');
#View(data1c)
#存在BUG
data1d <- checkOn_all(file = nshr("01-kaoQing1907_923.xlsx"),
                      AmCheckTime = '9:00:00',
                      pmCheckTime = '23:00:00',
                      pmEndTime = '23:30:00');
#View(data1d)

data1e <- checkOn_all(file = nshr("01-kaoQing1907_1019.xlsx"),
                      AmCheckTime = '10:00:00',
                      pmCheckTime = '19:00:00',
                      pmEndTime = '19:30:00');
#View(data1e)

data1f <- checkOn_all(file = nshr("01-kaoQing1907_10p19p.xlsx"),
                      AmCheckTime = '10:30:00',
                      pmCheckTime = '19:30:00',
                      pmEndTime = '20:00:00');
#View(data1f)

data1 <-list(data1a,data1b,data1c,data1d,data1e,data1f);
data1 <- tsdo::listAsDF_rows(data1);
#View(data1);
#unique(data1$FEmpName);


data1$FNickName <-getNickNames(data1$FEmpName);
name_checkOn <-c("FDeptName","FNickName", "FEmpName","FDate","FCheckTime", "FMinDiff","FErrCount",
                  "FType");
#names(data1);
data1 <- data1[ ,name_checkOn];
class(data1) <- 'data.frame';
#View(data1);



# 补卡数据 -------

# 02-buKa_1907.xlsx

data2 <- deal_buKa(file = nshr('02-buKa_1907.xlsx'));
# View(data2);
data2 <- buKa_NameTransfer(data2);
data2$FNickName <-getNickNames(data2$FEmpName);
data2 <- data2[ ,name_checkOn];
#View(data2);

#合并打卡与并卡数据-----


daKa_data <- rbind(data1,data2);




data12_sum <- checkOn_sum(daKa_data,startDate = '2019-07-01',EndDate = '2019-07-31');
#针对数据进行处理，小于15分钟的数据变为0
data12_sum$FTotalValue <- -data12_sum$FTotalValue
data12_sum$FTotalValue <- replaceValue_lessThan_target(data12_sum$FTotalValue,targetValue = 15,replaceValue = 0)
#data1_sum <- data1_sum[data1_sum$FTotalMinDiff>0, ];

#View(data12_sum);


# 出差数据------
# 03-chuChai1907.xlsx


data3 <- deal_chuChai(file=nshr("03-chuChai1907.xlsx"));
data3 <- chuChai_NameTransfer(data3);
data3 <- chuChai_splitIntoMultiRows(data=data3);
data3$FNickName <-getNickNames(data3$FEmpName);
#names(data3);
name_chuChai <- c("FDeptName","FNickName","FEmpName","FStartDate","FEndDate","FDays", "FHours",
"FType");
data3 <- data3[ ,name_chuChai];
#View(data3)
data3_sum <- chuChai_sum(data3,startDate = '2019-07-01',endDate = '2019-07-31');
#View(data3_sum)

# 加班------
#04-jiaBan_1907.xlsx
data4 <- deal_jiaBan(file=nshr("04-jiaBan_1907.xlsx"));
 #View(data4)
data4 <- jiaBan_NameTransfer(data4);
data4$FNickName <-getNickNames(data4$FEmpName);
name_jiaBan <-c("FDeptName","FNickName","FEmpName","FStartDate","FStartTime","FEndDate","FEndTime",
                "FHours", "FType");
data4 <- data4[ ,name_jiaBan];
#View(data4);

data4_sum <- jiaBan_sum(data4,startDate = '2019-07-01',endDate = '2019-07-31');
#View(data4_sum);


#请假------
# 05-qingJia_1907.xlsx

data5 <- deal_qingJia(file=nshr("05-qingJia_1907.xlsx"));
#View(data5);
data5 <- qingJia_NameTransfer(data5);
data5$FNickName <-getNickNames(data5$FEmpName);
name_qingJia <-c("FDeptName","FNickName","FEmpName","FStartDate","FStartTime","FEndDate","FEndTime",
                "FHours", "FType");
data5 <- data5[ ,name_qingJia];
#View(data5);
data5_sum <-qingJia_sum(data5,startDate = '2019-07-01',endDate = '2019-07-31');
#View(data5_sum);

# 外出 ------
#

data6 <- deal_waiChu(file=nshr("06-waiChu1907.xlsx"));
#View(data6);
data6 <- waiChu_NameTransfer(data6);
data6 <- waiChu_splitIntoMultiRows(data = data6);
data6$FNickName <-getNickNames(data6$FEmpName);
name_waiChu <- c("FDeptName","FNickName", "FEmpName", "FStartDate", "FEndDate",
                 "FHours",     "FType");
data6 <- data6[ ,name_waiChu];

#View(data6);

data6_sum <- waiChu_sum(data6,startDate = '2019-07-01',endDate = '2019-07-31');
#View(data6_sum);


hr_sum <- rbind(data12_sum,data3_sum,data4_sum,data5_sum,data6_sum);
hr_sum$FNickName <-getNickNames(hr_sum$FEmpName);
hr_sum <- hr_sum[ ,c("FDeptName","FNickName","FEmpName","FType","FTotalValue")];
hr_sum <- dcast(hr_sum,FDeptName+FNickName+FEmpName~FType,sum);


#View(hr_sum);
names(data1) <- c("部门","花名","姓名", "日期","打卡时间", "迟到分钟数",
                 "迟到次数" ,"类型")

names(data2) <- c("部门","花名","姓名", "日期","打卡时间", "迟到分钟数",
                 "迟到次数" ,"类型");
names(data3) <- c("部门","花名","姓名", "开始日期","结束日期","天数", "小时数",
                 "类型");
names(data4) <- c("部门","花名","姓名", "开始日期","开始时间","结束日期","结束时间", "小时数",
                  "类型");
names(data5) <-c("部门","花名","姓名", "开始日期","开始时间","结束日期","结束时间", "小时数",
                 "类型");
names(data6) <-c("部门","花名","姓名", "开始日期","结束日期", "小时数",
                 "类型");
name_hr_sum <- names(hr_sum);
names(hr_sum) <-c("部门","花名","姓名",name_hr_sum[-c(1:3)]);

res <- list(data1,data2,data3,data4,data5,data6,hr_sum);
names(res) <- c('打卡明细','补卡明细','出差明细','加班明细',
                '请假明细','外出明细','HR汇总表')


openxlsx::write.xlsx(res,"hr201907.xlsx")





