#' createVars
#' 
#' creating new vars
#' 
#' @export


createVars<-function(data,varNames='',Formulas='',byVars='',subSets='',batch=F,batchVars='',batchVarsTail='_new'){
  require('data.table')
  require('stringi')
  if(is.character(data)){eval(as.name(data))->data}
  
  as.data.table(data)->dt
  
  if(subSets%in%c('',NA,'NA','NULL')){
    sub<-'T'
  } else {
    # subset(dt,eval(parse(text=subSets)))->dt
    sub<-subSets
  }
  
  if(!batch){
    # unlist(stri_split_fixed(Formulas,'&'))->FormulasAll
    dt[eval(parse(text=sub)),c(varNames):=lapply(Formulas,function(i)eval(parse(text=i))),by=byVars]
  } else {
    dt[,paste0(batchVars,batchVarsTail):=lapply(.SD,eval(parse(text=Formulas))),by=byVars,.SDcols=batchVars]
  }
  
  return(as.data.frame(dt))
  
}

