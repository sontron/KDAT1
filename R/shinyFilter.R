#' shinyFilter
#' 
#' automaticly generate filter inputs
#' 
#' @export

shinyFilter<-function(dt,filter=Filter){
  renderUI({
    lapply(filter,function(i){
      if(class(dt[,i])%in%c('character','factor')){
        pickerInput(i,i,choices = unique(dt[,i]),selected = unique(dt[,i]),multiple = T,options = list(`actions-box` = T))
      } else {
        numericRangeInput(i,i,value=c(min(dt[,i],na.rm=T),max(dt[,i],na.rm=T)))
      }
    })
  })
}