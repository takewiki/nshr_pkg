#' 网商补卡记录
#'
#' @param file 补卡钉钉导出文件
#'
#' @return 返回值
#' @import tsda
#' @import tsdo
#' @export
#'
#' @examples
#' deal_buKa();
deal_buKa <- function(file='data-raw/input/02-ns_buKa.xlsx',
                      AmCheckTime = '9:00:00',
                      pmCheckTime="18:00:00"
                      ) {
  buka <- readExcelDf(file);
  buka_name <- names(buka);
  buka_name_selected <-c("审批编号","发起人姓名" ,"发起人部门" , "补卡班次");
  buka_sel <- buka %>%df_selectCol(buka_name_selected);
  FShift <- buka_sel$补卡班次;
  FDate <- left(FShift,10);
  #处理补打印记录
  FShift <- mid(FShift,18,2);
  shiftCount <- length(FShift);
  suffix <- rep('补卡',shiftCount);
  FType <- paste(FShift,suffix,sep="");
  FBillNo <- buka_sel$审批编号;
  FEmpName <- buka_sel$发起人姓名;
  FDeptName <- buka_sel$发起人部门;
  res<- data.frame(FDeptName,FEmpName,FDate,FType,
                          stringsAsFactors = F);
  res$FCheckTime <- AmCheckTime;
  res$FCheckTime[res$FType == '下班补卡'] <- pmCheckTime;
  res$FMinDiff <- 0;
  res$FErrCount <- 0 ;

  return(res);

}

#' 针对补卡数据进行处理
#'
#' @param data  deal_buKa得到的数据
#'
#' @return 返回值
#' @include utility.R
#' @export
#'
#' @examples
#' buKa_NameTransfer();
buKa_NameTransfer <- function(data) {

  #获取相应的姓名对照表
  nameTable <- ns_NameTable();
  #进行匹配连接
  res <-left_join(data,nameTable,by=c("FEmpName"="FDingDing"));
  #选择字段，表已经匹配后
  name_selected <-c("FDeptName","FName","FDate","FCheckTime","FMinDiff"
                    ,"FErrCount", "FType")
  #选择字段
  res <- res[ ,name_selected];
  #针对字段进行重命名
  names(res) <-c("FDeptName","FEmpName","FDate","FCheckTime","FMinDiff"
                 ,"FErrCount", "FType")
  #返回结果
  return(res);
}



