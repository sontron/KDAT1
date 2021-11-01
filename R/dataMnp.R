#' dataMnp
#' 
#' a function provides data manipulating procedures, mainly based on data.table.
#' 
#' 
#' @export


dtMnp<-function(data,
                  subSets='',
                  byVars='',
                  formulas
                  
){
  require('data.table')
  require('stringi')
  if(is.character(data)){eval(as.name(data))->data}
  
  
  as.data.table(data)->dt
  unlist(stri_split_fixed(formulas,';'))->Formulas
  paste('.(',paste(Formulas,collapse=','),')')->FormulasAll
  
  
  if(subSets%in%c('',NA,'NA','NULL')){
    dt<-dt
  } else {
    subset(dt,eval(parse(text=subSets)))->dt
  }
  
  dt[,eval(parse(text=FormulasAll)),
     by=byVars
  ]->res
  
  return(res)
  
  
}

