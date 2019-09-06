#' 处理外出数据
#'
#' @param file 文件名
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' deal_waiChu();
deal_waiChu <- function(file="data-raw/input/06-ns_waichu.xlsx") {
  res <- readExcelDf(file);
  # waichu_name <- names(waichu);
  waichu_name_sel <-c( "审批状态","发起人部门","发起人姓名",
                      "开始时间","结束时间","外出时间")
  res <- res %>%df_selectCol(waichu_name_sel);
  names(res) <-c('FStatus','FDeptName','FEmpName',
                        'FStartDate','FEndDate','FHours')
  res$FHours_calc <- date_diff(res$FStartDate,res$FEndDate,unit = 'h',hourPerDay = 8,is.hr = TRUE);


  res <- res[res$FStatus != '已撤销', ];
  #使用计算值替代小时数为NA的值，其他保持不变
  res$FHours[is.na(res$FHours)] <- res$FHours_calc[is.na(res$FHours)]
  res <- res[,c('FDeptName','FEmpName',
                'FStartDate','FEndDate','FHours')]
  res$FHours <- as.numeric(res$FHours);
  res$FType <- "外出"
  return(res);

}

#' 针对外出数据进行名称转换
#'
#' @param data 外出数据deal_waiChu处理后结果
#'
#' @return 返回值
#' @include utility.R
#' @export
#'
#' @examples
#' waiChu_NameTransfer();
waiChu_NameTransfer <- function(data) {
  #获取相应的姓名对照表
  nameTable <- ns_NameTable();
  #进行匹配连接
  res <-left_join(data,nameTable,by=c("FEmpName"="FDingDing"));
  #选择字段，表已经匹配后
  name_selected <-c("FDeptName","FName","FStartDate","FEndDate","FHours"
                    , "FType")
  #选择字段
  res <- res[ ,name_selected];
  #针对字段进行重命名
  names(res) <-c("FDeptName","FEmpName","FStartDate","FEndDate","FHours"
                 , "FType")
  #返回结果
  return(res);

}


#' 外出数据处理
#'
#' @param data 数据
#' @param startDateName  开始日期
#' @param endDateName 结束日志
#'
#' @return 返回值
#' @export
#'
#' @examples
#' waiChu_splitIntoMultiRows();
waiChu_splitIntoMultiRows <- function(data,
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
    #处理数据；

    di<-df_rowRepMultiByDate(data = row,startDateName = 'FStartDate',
                             endDateName = 'FEndDate');
    di$FStartDate <- dateVec;
    di$FEndDate <- dateVec;

    if (as.numeric(row$FHours) <= 8){
      di$FHours <-as.numeric(row$FHours);

    }else{
      di$FHours <- 8;
    }

    res[[i]] <-di;

  }
  res <- do.call('rbind',res);
  #针对数据进行处理
  #res$FHours[res$FHours >= 8] <- 8;
  res <- res[ ,header_all];
  return(res);

}


#' 处理将外出数据进行汇总
#'
#' @param data 外出明细数据
#' @param startDate 开始日期
#' @param endDate  结束日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' waiChu_sum();
waiChu_sum <- function(data,startDate='2019-07-01',
                       endDate='2019-07-31') {
  #只显示过滤时间范围内数据
  data <- data[data$FStartDate >= startDate & data$FStartDate <= endDate, ];
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType) %>% summarise(FTotalValue = sum(FHours))
  return(res);

}

#' 针对外出数据按天进行汇总
#'
#' @param data 外出数据
#' @param startDate  开始日期
#' @param endDate 结束日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' waiChu_sum_byDay();
waiChu_sum_byDay <- function(data,startDate='2019-07-01',
                       endDate='2019-07-31') {
  #只显示过滤时间范围内数据
  data <- data[data$FStartDate >= startDate & data$FStartDate <= endDate, ];
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType,FStartDate) %>% summarise(FTotalValue = sum(FHours)*60)
  names(res) <-c('FDeptName','FEmpName','FType','FDate','FTotalValue')
  res$FType <-'外出补时'
  return(res);

}
