library(tsdo);
library(tsda);
#  超过1就算2，向上取整
#  区分事假与病假
#' 处理请假业务
#'
#' @param file 文件名
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' deal_qingJia();
deal_qingJia <- function(file="data-raw/input/05-ns_qingjia.xlsx") {
  res<- readExcelDf(file);

  qingj_name_sel <-c( "审批状态","发起人部门","发起人姓名",
                      "开始时间", "结束时间" , "时长", "类型" )
  res <- res %>% df_selectCol(qingj_name_sel);
  names(res) <- c('FStatus','FDeptName','FEmpName',
                        'FStartTime','FEndTime','FHours',
                        'FType');
  res <- res[res$FStatus !='已撤销',c('FDeptName','FEmpName',
                                           'FStartTime','FEndTime','FHours',
                                           'FType')]
  #分离出日期与时间
  res$FStartDate <- left(res$FStartTime,10);
  res$FStartTime <- substr(res$FStartTime,12,nchar(res$FStartTime));
  res$FEndDate <- left(res$FEndTime,10);
  res$FEndTime <- substr(res$FEndTime,12,nchar(res$FEndTime));
  # startTime <- as.POSIXct(qingj_sel$qj_startTime);
  # endTime <- as.POSIXct(qingj_sel$qj_endTime);
  # time_diff_calc <- round(as.numeric(endTime-startTime)/3600,2);
  # qingj_sel$time_diff_cal <-time_diff_calc;
  res$FHours <- round(as.numeric(res$FHours),0)
  return(res);

}


#' 处理请假的名称转换
#'
#' @param data deal_qingJia处理后的数据
#'
#' @return 返回值
#' @include utility.R
#' @export
#'
#' @examples
#' qingJia_NameTransfer();
qingJia_NameTransfer <- function(data){
  #获取相应的姓名对照表
  nameTable <- ns_NameTable();
  #进行匹配连接
  res <-left_join(data,nameTable,by=c("FEmpName"="FDingDing"));
  #选择字段，表已经匹配后
  name_selected <-c("FDeptName","FName","FStartDate","FStartTime","FEndDate","FEndTime","FHours"
                    , "FType")
  #选择字段
  res <- res[ ,name_selected];
  #针对字段进行重命名
  names(res) <-c("FDeptName","FEmpName","FStartDate","FStartTime","FEndDate","FEndTime","FHours"
                 , "FType")
  #返回结果
  return(res);
}

#' 处理请假的汇总数据
#'
#' @param data 请假明细数据
#' @param startDate 开始日期
#' @param endDate 结束日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' qingJia_sum();
qingJia_sum <- function(data,startDate='2019-07-01',
                        endDate='2019-07-31') {
  #只显示过滤时间范围内数据
  data <- data[data$FStartDate >= startDate & data$FStartDate <= endDate, ];
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType) %>% summarise(FTotalValue = sum(FHours))
  return(res);

}



#' Title
#'
#' @param data 数据
#' @param startDate 开始日期
#' @param endDate 结束日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' qingJia_sum_byDay();
qingJia_sum_byDay <- function(data,startDate='2019-07-01',
                        endDate='2019-07-31') {
  #只显示过滤时间范围内数据
  data <- data[data$FStartDate >= startDate & data$FStartDate <= endDate, ];
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType,FStartDate) %>% summarise(FTotalValue = sum(FHours)*60)
  names(res) <- c('FDeptName','FEmpName','FType','FDate','FTotalValue')
  return(res);

}

