library(tsdo);
library(tsda);

#  加班时间    5.75 = 6  其他算5
#    凌晨 0：00-  5：00
#    每天算一早一晚；






# 读取考勤数据，自动保存到df
data_CheckOn <- readExcelDf("data-raw/input/01-ns_kaoQin.xlsx");
# class(data_CheckOn);
# View(data_CheckOn);

#针对列进行重命名
names(data_CheckOn) <-c('FDeptName','FEmpName','FCheckNum','FCheckOnDateTime','FMachineId',
                        'FIndex','FCheckStyle','FCardNumber');
#选择字段列名
fieldSel_checkon <- c('FDeptName','FEmpName','FCheckOnDateTime');
#data_CheckOn <- df_selectCol(data_CheckOn,fieldSel_checkon);
data_CheckOn <- data_CheckOn %>% df_selectCol(fieldSel_checkon);
#View(data_CheckOn);
#data_CheckOn <- data_CheckOn[,fieldSel_checkon]

datetime <- data_CheckOn$FCheckOnDateTime;
FDate <- left(datetime,10);
FTime <-substr(datetime,12,nchar(datetime));
FTime <-hms(FTime);
#判断是否为N+1加班;
morningStartTime <-'0:00:00';
moringEndTime <-'6:00:00';
FIsMorning <- is.Morning(x = FTime,
                         morningStartTime = morningStartTime,
                         morningEndTime = moringEndTime
                         );
#FIsMorning;

# #判断是否为上午
AmStartTime <-'6:00:00';
AmEndTime <- '12:00:00';
FIsAm <- is.Am(FTime,AmStartTime =AmStartTime, AmEndTime = AmEndTime);
#FIsAm;

#判断是否为下午班
pmStartTime ='12:00:00';
pmEndTime  ='18:30:00';
FIsPm <-is.Pm(FTime,startTime = pmStartTime,endTime = pmEndTime)
#FIsPm;

#判断是否加班
nightStartTime <-'18:30:00';
FIsNight <-is.Night(FTime,nightStartTime = nightStartTime);
#FIsNight;
#判断上午处理时间差
#以 上午9:00 作为标准
FAmDiff <- timeDiff(FTime,'9:00:00',unit = 'm',digit = 1);
#处理下午的处理
FPmDiff <-timeDiff(FTime,'18:00:00',unit = 'm',digit = 1);
#处理加班时间
FNightDiff <-timeDiff(FTime,'18:00:00',unit = 'h',digit = 2);
#处理凌晨的加班时间处理
FMorningDiff <-timeDiff(FTime,'0:00:00',unit = 'h',digit = 2);
FDeptName <-data_CheckOn$FDeptName;
FEmpName <- data_CheckOn$FEmpName
data_CheckOn<-data.frame(FDeptName,FEmpName,FDate,FTime,FIsMorning,FMorningDiff,FIsAm,FAmDiff,FIsPm,FPmDiff,
                         FIsNight,FNightDiff,
                         stringsAsFactors = F);
#修复数据
data_CheckOn$FAmDiff[data_CheckOn$FIsAm == FALSE] <- 0
data_CheckOn$FAmDiff[data_CheckOn$FAmDiff > 0] <- 0
#修复下午的数据
data_CheckOn$FPmDiff[data_CheckOn$FIsPm == FALSE] <- 0;
data_CheckOn$FPmDiff[data_CheckOn$FPmDiff < 0] <- 0;
data_CheckOn$FPmDiff <- -data_CheckOn$FPmDiff;
#修复晚上数据
data_CheckOn$FNightDiff[data_CheckOn$FIsNight == FALSE] <- 0;
data_CheckOn$FNightDiff <- -data_CheckOn$FNightDiff
#修复凌晨数据
data_CheckOn$FMorningDiff[data_CheckOn$FIsMorning == FALSE] <- 0;
data_CheckOn$FMorningDiff <- -data_CheckOn$FMorningDiff;


View(data_CheckOn);
