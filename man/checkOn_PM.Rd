% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/01checkOn_Func.R
\name{checkOn_PM}
\alias{checkOn_PM}
\title{处理下午的时间}
\usage{
checkOn_PM(data_CheckOn, PmCheckTime = "18:00:00", PmLeakMins = 5)
}
\arguments{
\item{data_CheckOn}{考勤数据}

\item{PmCheckTime}{下午打卡时间}

\item{PmLeakMins}{提前时间}
}
\value{
返回值
}
\description{
处理下午的时间
}
\examples{
checkOn_PM();
}
