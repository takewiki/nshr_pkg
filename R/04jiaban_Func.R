#' 处理网商的加班记录
#'
#' @param file 文件名
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' deal_jiaBan();
deal_jiaBan <- function(file="data-raw/input/04-ns_jiaban.xlsx") {

  res <- readExcelDf(file);
  print('1')
  #jia_name <- names(jiab);
  jia_name_sel <-c("审批状态" ,"发起人部门" ,"发起人姓名" , "开始时间"  ,
                   "结束时间" ,"加班时长（1小时起）" ,"是否法定假日"
  )
  print('2')
  res <- res %>% df_selectCol(jia_name_sel);
  names(res) <-c('FStatus','FDeptName','FEmpName',
                      'FStartTime','FEndTime','FHours','FLegal')
  print('3')
  #处理没有填写的空值数据
  res$FLegal[is.na(res$FLegal)] <- "否";
  print('4')

  res <- res[res$FStatus !='已撤销', c('FDeptName','FEmpName',
                                    'FStartTime','FEndTime','FHours')];
  print('5')
  res$FStartDate <- left(res$FStartTime,10);
  res$FStartTime <-substr(res$FStartTime,12,nchar(res$FStartTime));
  print('6')
  res$FEndDate <- left(res$FEndTime,10);
  res$FEndTime <-substr(res$FEndTime,12,nchar(res$FEndTime));
  res$FHours <- as.numeric(res$FHours);
  print('7')
  #不再计算相应的时长，没有意义
  #FStartTime <- as.POSIXct(res$FStartTime);
  #FEndTime <- as.POSIXct(res$FEndtime);
  #res$FTimeDiff_calc <- round(as.numeric(FEndTime-FStartTime)/3600,2);

  res$FType <- "加班"
  print('8')
  return(res);

}


#' 处理用于加班时间的统计
#'
#' @param data deal_jiaBan处理后的数据
#'
#' @return 返回值
#' @include utility.R
#' @export
#'
#' @examples
#' jiaBan_NameTransfer();
jiaBan_NameTransfer <- function(data) {
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

#' 处理加班汇总数据
#'
#' @param data 加班明细数据
#' @param startDate 开始日期
#' @param endDate 结束日期
#'
#' @return 返回值
#' @export
#'
#' @examples
#' jiaBan_sum();
jiaBan_sum <- function(data,startDate='2019-07-01',
                       endDate='2019-07-31') {

  #只显示过滤时间范围内数据
  data <- data[data$FStartDate >= startDate & data$FStartDate <= endDate, ];
  #针对数据进行汇总
  res <- data %>% group_by(FDeptName,FEmpName,FType) %>% summarise(FTotalValue = sum(FHours))
  return(res);

}
