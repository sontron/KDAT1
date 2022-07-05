#' dtMnpdb
#' @export

dtMnpdb<-function(
  Host='127.0.0.1',
  userName='',
  passWd='',
  dbName='',
  tblName='',
  query='',
  subSets='',
  byVars='',
  formulas
                
){
  require('RMySQL')
  require('data.table')
  require('stringi')
  con<-dbConnect(MySQL(),host=Host,user=userName,password=passWd,dbname=dbName)
  dbSendQuery(con,query)->fetch
  dbFetch(fetch,n=-1)->data
  dbDisconnect(con)
  # dbReadTable(con,tblName)->data
  as.data.table(data)->dt
  # if(is.character(data)){eval(as.name(data))->data}
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

