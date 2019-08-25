library(tsdo);
library(tsda);


#' 处理网商考勤数据
#'
#' @param file 考勤数据文件
#' @param morningStartTime 凌晨开始时点
#' @param moringEndTime    凌晨结束时点
#' @param AmStartTime      上午开始时间
#' @param AmCheckTime      上午打卡时间
#' @param AmEndTime        上午结束时间
#' @param pmStartTime       下午开始时间
#' @param pmCheckTime        下午打卡时间
#' @param pmEndTime         下午结束时间
#' @param nightStartTime    晚上开始时间
#' @param nightCheckTime    晚上结束时间
#'
#' @return 返回数据框
#' @import hms
#' @export
#'
#' @examples
#' deal_CheckOn();
deal_CheckOn <- function(file="data-raw/input/01-ns_kaoQin.xlsx",
                         morningStartTime ='0:00:00' ,
                         moringEndTime = '6:00:00',
                         AmStartTime  = '8:00:00',
                         AmCheckTime = '9:00:00',
                         AmEndTime  = '12:00:00',
                         pmStartTime ='12:00:00',
                         pmCheckTime="18:00:00",
                         pmEndTime  ='18:30:00',
                         nightStartTime = '18:30:00',
                         nightCheckTime = '19:00:00'
                         ) {
  # 读取考勤数据，自动保存到df
  data_CheckOn <- readExcelDf(file = file);

  #针对列进行重命名
  names(data_CheckOn) <-c('FDeptName','FEmpName','FCheckNum','FCheckOnDateTime','FMachineId',
                          'FIndex','FCheckStyle','FCardNumber');
  #选择字段列名
  fieldSel_checkon <- c('FDeptName','FEmpName','FCheckOnDateTime');
  data_CheckOn <- data_CheckOn %>% df_selectCol(fieldSel_checkon);
  datetime <- data_CheckOn$FCheckOnDateTime;
  FDate <- left(datetime,10);
  FTime <-substr(datetime,12,nchar(datetime));
  FTime <- tsdo::round_time(FTime,'min');
  FTime <-as.hms(FTime);

  FIsMorning <- is.Morning(x = FTime,
                           morningStartTime = morningStartTime,
                           morningEndTime = moringEndTime );

  # 判断是否为上午

  FIsAm <- is.Am(FTime,AmStartTime =AmStartTime, AmEndTime = AmEndTime);


  #判断是否为下午班

  FIsPm <-is.Pm(FTime,startTime = pmStartTime,endTime = pmEndTime)


  #判断是否加班

  FIsNight <-is.Night(FTime,nightStartTime = nightStartTime);

  #判断上午处理时间差
  #以 上午9:00 作为标准
  FAmDiff <- timeDiff(FTime,AmCheckTime,unit = 'm',digit = 1);
  #处理下午的处理
  FPmDiff <-timeDiff(FTime,pmCheckTime,unit = 'm',digit = 1);
  #处理加班时间
  FNightDiff <-timeDiff(FTime,nightCheckTime,unit = 'h',digit = 2);
  #处理凌晨的加班时间处理
  FMorningDiff <-timeDiff(FTime,morningStartTime,unit = 'h',digit = 2);
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


  return(data_CheckOn);
}


#' 提供数据中的早上数据
#'
#' @param data_CheckOn 考勤数据
#' @param AmCheckTime  上班打卡时间
#' @param AmLateMins   迟到分钟数
#'
#' @return 返回值
#' @import hms
#' @import dplyr
#' @export
#'
#' @examples
#' checkOn_AM()
checkOn_AM <- function(data_CheckOn,AmCheckTime="9:00:00",AmLateMins=5) {

  am_fields <- c("FDeptName","FEmpName","FDate","FTime")
  res<-data_CheckOn[data_CheckOn$FIsAm == TRUE, am_fields];
  #针对早上打卡数据进行去重，取最小值；
  res <- res %>% group_by(FDeptName,FEmpName,FDate) %>% summarise(FCheckTime = min(FTime))
  res$FCheckTime <- as.hms(res$FCheckTime)
  res$FMinDiff <- timeDiff(res$FCheckTime,AmCheckTime,unit = 'm',digit = 1);
  res$FErrCount <-0 ;
  #计算迟到次数
  res$FErrCount[res$FMinDiff < -AmLateMins] = 1
  #针对没有迟到的数据进行清空处理
  res$FMinDiff[res$FMinDiff >=0 ] <- 0;
  res$FType <-"上班打卡"

  return(res);
}


#' 处理下午的时间
#'
#' @param data_CheckOn 考勤数据
#' @param PmCheckTime 下午打卡时间
#' @param PmLeakMins  提前时间
#'
#' @return 返回值
#' @export
#'
#' @examples
#' checkOn_PM();
checkOn_PM <- function(data_CheckOn,PmCheckTime="18:00:00",PmLeakMins=5) {
  pm_fields <- c("FDeptName","FEmpName","FDate","FTime")
  res<-data_CheckOn[data_CheckOn$FIsPm == TRUE, pm_fields];
  #针对早上打卡数据进行去重，取最小值；
  res <- res %>% group_by(FDeptName,FEmpName,FDate) %>% summarise(FCheckTime = max(FTime))
  res$FCheckTime <- as.hms(res$FCheckTime)
  res$FMinDiff <- -timeDiff(res$FCheckTime,PmCheckTime,unit = 'm',digit = 1);
  res$FErrCount <-0 ;
  #计算迟到次数
  res$FErrCount[res$FMinDiff < -PmLeakMins] = 1
  #针对没有迟到的数据进行清空处理
  res$FMinDiff[res$FMinDiff >=0 ] <- 0;
  res$FType <- "下班打卡";

  return(res);


}



#' 考勤数据入口函数
#'
#' @param file 文件名
#' @param morningStartTime   凌晨开始时间
#' @param moringEndTime     凌晨结束时间
#' @param AmStartTime    上午开始
#' @param AmCheckTime    上午打卡
#' @param AmEndTime      上午结束
#' @param AmLateMins     上午偏置
#' @param pmStartTime    下午开始
#' @param pmCheckTime    下午打卡
#' @param pmEndTime      下午结束
#' @param PmLeakMins     下午偏置
#' @param nightStartTime  晚上开始
#' @param nightCheckTime   晚上打卡
#'
#' @return 返回值
#' @import tsdo
#' @export
#'
#' @examples
#' CheckOn_all();
checkOn_all <- function(file="data-raw/input/01-ns_kaoQin.xlsx",
                         morningStartTime ='0:00:00' ,
                         moringEndTime = '6:00:00',
                         AmStartTime  = '8:00:00',
                         AmCheckTime = '9:00:00',
                         AmEndTime  = '12:00:00',
                         AmLateMins=0,
                         pmStartTime ='12:00:00',
                         pmCheckTime="18:00:00",
                         pmEndTime  ='18:30:00',
                         PmLeakMins=0,
                         nightStartTime = '18:30:00',
                         nightCheckTime = '19:00:00'

){
res <-deal_CheckOn(file=file,
                   morningStartTime = morningStartTime,
                   moringEndTime = moringEndTime,
                   AmStartTime = AmStartTime,
                   AmCheckTime = AmCheckTime ,
                   AmEndTime = AmEndTime,
                   pmStartTime = pmStartTime,
                   pmCheckTime = pmCheckTime,
                   pmEndTime = pmEndTime,
                   nightStartTime = nightStartTime,
                   nightCheckTime = nightCheckTime);
res_am <- checkOn_AM(data_CheckOn = res,
                     AmCheckTime = AmCheckTime,
                     AmLateMins = AmLateMins);
res_pm <- checkOn_PM(data_CheckOn = res,
                     PmCheckTime = pmCheckTime,
                     PmLeakMins = PmLeakMins);
res <- rbind(res_am,res_pm);
res <- res[order(res$FDeptName,res$FEmpName,res$FDate,res$FType),]
return(res);


}

#' 针对数据进行汇总处理
#'
#' @param checkOn_all_data 考勤数据
#' @param startDate   月度开始
#' @param EndDate    月度结束
#' @param ErrorOnly  是否只统计异常数据
#'
#' @return 返回值
#' @export
#'
#' @examples
#' checkOn_sum();
checkOn_sum <- function(checkOn_all_data,
                        startDate='2019-06-01',
                        EndDate='2019-06-30',
                        ErrorOnly=FALSE) {
  data <-checkOn_all_data;
  #只显示异常数据
  if (ErrorOnly == TRUE){
    data <- data[data$FErrCount == 1, ];
  }
  #只显示过滤时间范围内数据
  data <- data[data$FDate >= startDate & data$FDate <= EndDate, ];
  data$FType <-'迟到'
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType) %>% summarise(FTotalValue = sum(FMinDiff))
  return(res);

}





