#如果当天有出差记录，不取当天考勤记录

#' 处理网商出差数据
#'
#' @param file 文件名
#'
#' @return 返回值
#' @import tsdo
#' @import tsda
#' @export
#'
#' @examples
#' deal_chuChai();
deal_chuChai <- function(file="data-raw/input/03-ns_chuchai.xlsx") {
  res <- readExcelDf(file);
  # cc_name <- names(cc);
  cc_name_selected <-c("发起人部门" ,"发起人姓名" ,"审批状态" , "开始时间"  ,"结束时间","时长"  )
  res <- res %>% df_selectCol(cc_name_selected);

  names(res) <- c('FDeptName','FEmpName','FStatus',
                          'FStartDate','FEndDate','FDays')
  res <- res[res$FStatus =='完成', c('FDeptName','FEmpName','FStartDate','FEndDate','FDays')];
  res$FDays <-left(res$FDays,len(res$FDays)-1);
  res$FHours <- as.numeric(res$FDays)*8;
  res$FType <-"出差"
  return(res);
}

#' 针对出差数据进行名称转换
#'
#' @param data 出差deal_chuChai处理后数据
#'
#' @return 返回值
#' @include utility.R
#' @export
#'
#' @examples
#' chuChai_NameTransfer();
chuChai_NameTransfer <- function(data) {
  #获取相应的姓名对照表
  nameTable <- ns_NameTable();
  #进行匹配连接
  res <-left_join(data,nameTable,by=c("FEmpName"="FDingDing"));
  #选择字段，表已经匹配后
  name_selected <-c("FDeptName","FName","FStartDate","FEndDate","FDays","FHours"
                    , "FType")
  #选择字段
  res <- res[ ,name_selected];
  #针对字段进行重命名
  names(res) <-c("FDeptName","FEmpName","FStartDate","FEndDate","FDays","FHours"
                 , "FType")
  #返回结果
  return(res);

}

#' 针对出差数据进行处理
#'
#' @param data 数据
#' @param startDateName 日期
#' @param endDateName  结束日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' chuChai_splitIntoMultiRows();
chuChai_splitIntoMultiRows <- function(data,
                                       startDateName='FStartDate',
                                       endDateName='FEndDate') {
  nrow <- nrow(data);
  #所有的列名
  header_all <- names(data);
  #日期列名
  sel_dates <- c(startDateName,endDateName);
  #非日期列名
  sel_ot <- header_all[!header_all %in% sel_dates];
  #用于存储结果
  res <- list();
  # 处理每一行数据
  for ( i in 1:nrow){
    row <- data[i, ];
    dateVec <-date_minus(startDate = row[ ,startDateName,drop=TRUE],
                         endDate = row[ ,endDateName,drop=TRUE]);
    row_not_dates <- row[ ,sel_ot];
    di<-df_rowRepMulti(row_not_dates,row$FDays);
    di$FStartDate <- dateVec;
    di$FEndDate <- dateVec;
    di$FDays <-1;
    di$FHours <-8;
    res[[i]] <-di;

  }
  res <- do.call('rbind',res);
  res <- res[ ,header_all];
  return(res);

}


#' 针对出差数据进行处理
#'
#' @param data 出差明细数据
#' @param endDate 结束日期
#' @param startDate 开始日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' chuChai_sum();
chuChai_sum <- function(data,startDate='2019-07-01',
                        endDate='2019-07-31') {
  #只显示过滤时间范围内数据
  data <- data[data$FStartDate >= startDate & data$FStartDate <= endDate, ];
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType) %>% summarise(FTotalValue = sum(FHours))
  return(res);

}

#' 出差数据按天汇总
#'
#' @param data  数据
#' @param startDate 开始日期
#' @param endDate 结束日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' chuChai_sum_byDay();
chuChai_sum_byDay <- function(data,startDate='2019-07-01',
                        endDate='2019-07-31') {
  #只显示过滤时间范围内数据
  data <- data[data$FStartDate >= startDate & data$FStartDate <= endDate, ];
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType,FStartDate) %>% summarise(FTotalValue = sum(FHours)*60)
  names(res) <-c('FDeptName','FEmpName','FType','FDate','FTotalValue');
  return(res);

}


