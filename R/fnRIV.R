#' fnRIV
#' 
#' @export

fnRIV<-function(xvar,grpvar,noGrp,meanGrp){
  meanAll<-mean(xvar,na.rm=T)
  SST<-sum((xvar-meanAll)^2,na.rm=T)
  SSG<-sum((meanGrp-meanAll)^2,na.rm=T)
  return(SSG/SST)
}