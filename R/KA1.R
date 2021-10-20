#' KT1
#'
#' launcher function
#' @param wd  set working directory.
#' @param ...  more args used in runApp, eg. launch.browser=T
#'
#'
#' @export
KT1 <- function(wd=paste0(getwd(),'/'),Encod='utf8-8',...) {
  # sd=paste(path.package('KDAT1'),'/app/',sep='')
  # 
  #   readLines(paste0(sd,'appCN.R'),encoding = Encod)->app
  #   c(paste("wd=",paste('\"',wd,'\"',sep=''),sep=''),app)->appNew
  #   writeLines(appNew,paste0(sd,'app.R'))
  library(shiny)
  runApp(system.file("app", package = "KDAT1"), ...)
}

