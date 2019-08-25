

#' 获取网商日历
#'
#' @param file 文件名
#'
#' @return 返回值
#' @import readxl
#' @import tsdo
#' @export
#'
#' @examples
#' ns_Calendar();
ns_Calendar <- function(file="data-raw/public/ns_calendar.xlsx") {
  #library(readxl)
  res <- read_excel(file);
  res <- tbl_as_df(res);
  return(res);
}



#' 提供名称对照表
#'
#' @param file  对照表文件名
#' @param sheet 页签mapping
#'
#' @return 返回值
#' @import readxl
#' @export
#'
#' @examples
#' ns_NameTable();
ns_NameTable <- function(file="data-raw/public/ns_NameTable.xlsx",
                         sheet="mapping") {
  #library(readxl)
  res<- read_excel(file,sheet = sheet);
  res <- tbl_as_df(res);
  names(res) <- c("FDingDing","FNickName","FName");
  return(res);
}




#' 用于处理nshr文件
#'
#' @param file 文件名称
#' @param base_dir 基本路径
#'
#' @return 返回值
#' @export
#'
#' @examples
#' nshr();
nshr <- function(file,base_dir="./data-raw/input/"){
  res <- paste(base_dir,file,sep="");
  return(res);

}

#' 获取花名
#'
#' @param FName 真实姓名
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getNickName();
getNickName <- function(FName) {

   data <-ns_NameTable();
   res <- data[data$FName == FName,'FNickName',drop =TRUE];

   if(length(res) == 0){
     res <-""
   }else{
     res <- res[1];
   }
  return(res);
}

#' 批量获取花名
#'
#' @param FNames 真实姓名向量
#'
#' @return 返回值
#' @export
#'
#' @examples
#' getNickNames();
getNickNames <- function(FNames) {
  res <- lapply(FNames,getNickName);
  res <- as.character(unlist(res));
  return(res);

}

