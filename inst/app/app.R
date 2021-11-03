library(skimr)
library(shinyAce)
library(shinythemes)
library(shinyWidgets)
library(stringi)
library(DT)
library(lubridate)
library(data.table)



options(shiny.maxRequestSize = 100000*1024^2)

environment()->envKDAT1
lstKDAT1<-list()


assign('lstKDAT1',lstKDAT1,env=envKDAT1)




server<-function(input,output,session){
  
  ###### 所有有可能引起数据变化的input ######
  change_data<-reactive({
    input$go_dataImpt
    input$go_varClass
    input$go_varMnp
  })
  
  
  ###### 数据导入功能(data_Impt) ######
  data_dataImpt<-reactive({
    input$go_dataImpt
    if(is.null(input$file_dataImpt)) {
      Data<-read.csv(text=input$text_dataImpt,
                     sep="\t",
                     na.strings=input$nastr_dataImpt,
                     stringsAsFactors = input$strAsFac_dataImpt,
                     header=input$header_dataImpt,
                     
                     fileEncoding = input$encod_dataImpt)
    } else {
      inFile<-input$file_dataImpt
      if(input$argsMore_dataImpt=='') {
        
        
        Data<-read.csv(inFile$datapath,
                       na.strings=input$nastr_dataImpt,
                       stringsAsFactors = input$strAsFac_dataImpt,
                       header=input$header_dataImpt,
                       fileEncoding = input$encod_dataImpt,
                       
                       sep=input$sep_dataImpt)
      } else {
        textfun_dataImpt<-paste("read.csv(",paste("file=inFile$datapath","header=input$header_dataImpt","na.strings=input$nastr_dataImpt","stringsAsFactors = input$strAsFac_dataImpt","sep=input$sep_dataImpt","fileEncoding=input$encod_dataImpt",input$argsMore_dataImpt,sep=','),")",sep='')
        eval(parse(text=textfun_dataImpt))->Data
      }
    }
    return(Data)
  })
  
  
  
  
  data_dataImptHead<-reactive({
    
    if(is.null(input$file_dataImpt)) {
      Data<-read.csv(text=input$text_dataImpt,
                     sep="\t",
                     na.strings=input$nastr_dataImpt,
                     stringsAsFactors = input$strAsFac_dataImpt,
                     header=input$header_dataImpt,
                     fileEncoding = input$encod_dataImpt,nrows=10)
    } else {
      inFile<-input$file_dataImpt
      if(input$argsMore_dataImpt=='') {
        
        
        Data<-read.csv(inFile$datapath,
                       na.strings=input$nastr_dataImpt,
                       stringsAsFactors = input$strAsFac_dataImpt,
                       header=input$header_dataImpt,
                       fileEncoding = input$encod_dataImpt,
                       sep=input$sep_dataImpt,nrows=10)
      } else {
        textfun_dataImpt<-paste("read.csv(",paste("file=inFile$datapath","header=input$header_dataImpt","na.strings=input$nastr_dataImpt","stringsAsFactors = input$strAsFac_dataImpt","sep=input$sep_dataImpt","nrows=10","fileEncoding=input$encod_dataImpt",input$argsMore_dataImpt,sep=','),")",sep='')
        eval(parse(text=textfun_dataImpt))->Data
      }
    }
    return(Data)
  })
  
  
  
  output$args_dataImpt<-renderUI({
    list(
      panel(status='primary',
            heading='数据读取参数设定',
            flowLayout(
              pickerInput(
                inputId='sep_dataImpt',
                label='文本分隔符',
                choices=c(
                  '逗号分隔'=',',
                  '制表分隔符'='\t',
                  '空格分隔'=''
                ),
                selected=',',
                multiple = FALSE,
                options = list(`actions-box` = FALSE)
              ),
              pickerInput(
                inputId='nastr_dataImpt',
                label='缺失值类型',
                choices=c(
                  '空白'='',
                  '空格'=' ',
                  'NA'='NA',
                  '.'='.'
                ),
                selected='NA',
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              
              textInputAddon(inputId = 'encod_dataImpt',label = '文件编码格式',value = 'gb18030',placeholder = 'eg:utf8',addon = icon("pencil")),
              awesomeCheckbox('header_dataImpt','数据包含变量名',TRUE),
              awesomeCheckbox('strAsFac_dataImpt','是否将字符串转换成因子',FALSE),
              awesomeCheckbox('deleteUnique','是否值唯一的变量',TRUE)
            ),
            textInputAddon(inputId = "argsMore_dataImpt", label = "更多参数设定", placeholder = "eg:nrows=10",value='',addon = icon("pencil")),
            helpText('在更多参数设置一栏，可以自定义参数，在此是read.table函数的参数，若无则留空，多个参数设定，用","隔开')
      )
      
    )
  })
  
  
  output$more1_dataImpt<-renderUI({
    list(
      panel(status='primary',
            heading='变量筛选及数据名设定',
            pickerInput(
              inputId = "varsKeep_dataImpt",
              label = "选定需要保留的变量",
              choices = names(data_dataImpt()),
              selected =names(data_dataImpt()),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),
            textInputAddon(inputId = "dataName_dataImpt", label = "输入保存对象的名称", placeholder = "eg:mydata",value='data1',addon = icon("pencil"))
      )
      
    )
  })
  
  assign_dataImpt<-observeEvent(input$go_dataImpt,{
    isolate({
      data_dataImpt()->dat
      dat[,input$varsKeep_dataImpt]->dat
      sapply(dat,function(i)length(unique(i)))->lenI
      names(lenI)[which(lenI>1)]->namesNotUnique
      if(input$deleteUnique){
        dat[,namesNotUnique]->dat
      } else {
        dat->dat
      }
      assign(input$dataName_dataImpt,dat,envKDAT1)
      lstKDAT1$Data[[input$dataName_dataImpt]]<-dat
      assign('lstKDAT1',lstKDAT1,envir=envKDAT1)
    })
    
  })
  
  output$dataHead<-renderPrint({
    data_dataImptHead()
  })
  
  output$varClass_dataImpt<-renderPrint({
    input$go_dataImpt
    req(input$go_dataImpt)
    isolate({
      cat('当前数据共：',nrow(data_dataImpt()),'观测(行)','\n')
      cat('当前数据共：',ncol(data_dataImpt()),'变量(列)','\n')
      cat('数据各变量类型如下：','\n')
      sapply(data_dataImpt(),class)
    })
  })
  
  
  output$head_dataImpt<-renderPrint({
    input$go_dataImpt
    req(input$go_dataImpt)
    isolate({
      skim(data_dataImpt())
      
    })
  })
  
  
  
  
  
  ###### 变量类型转换(varClass) ######
  
  output$more1_varClass<-renderUI({
    
    change_data()
    #?#
    list(
      panel(status='primary',
            heading='选择数据集',
            pickerInput(
              inputId = "dataSel_varClass",
              label = "选择数据集",
              choices = ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))],
              selected =ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))][1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
            #selectInput('dataSel_varClass','选择数据集',ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))])
      )
    )
  })
  
  data_varClass<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_varClass,envKDAT1)->data_varClass1
    return(data_varClass1)
  })
  
  output$more2_varClass<-renderUI({
    list(
      panel(status='primary',
            heading='变量类型转换',
            awesomeCheckbox('auto_varClass','是否进行自动判断？',TRUE),
            conditionalPanel(
              condition="input['auto_varClass']",
              numericInput('lengthTab','唯一元素数目',min=1,max=1000,value=10),
              numericInput('threshold','阈值',min=0,max=1,value=0.8)
              
            ),
            conditionalPanel(
              condition="!input['auto_varClass']",
              pickerInput(
                inputId='varsNum_varClass',
                label='转换为数值型变量',
                choices=names(data_varClass()),
                #selected =names(data_varClass())[1],
                multiple=TRUE,
                options = list(`actions-box` = TRUE)
              ),
              pickerInput(
                inputId='varsChar_varClass',
                label='转换为字符型变量',
                choices=names(data_varClass()),
                #selected =names(data_varClass())[1],
                multiple=TRUE,
                options = list(`actions-box` = TRUE)
              ),
              panel(status='primary',
                    heading='日期时间转换设置',
                    pickerInput(
                      inputId='varsDate_varClass',
                      label='转换为日期型变量',
                      choices=names(data_varClass()),
                      #selected =names(data_varClass())[1],
                      multiple=TRUE,
                      options = list(`actions-box` = TRUE)
                    ),
                    pickerInput(
                      inputId='dateFormat',
                      label='日期格式',
                      choices=c(
                        '年'='y',
                        '年月'='ym',
                        "年月日"="ymd",
                        '月日年'='mdy',
                        '日月年'='dmy'
                      ),
                      selected ='yyyymmdd',
                      multiple=FALSE,
                      options=list(`actions-box` = FALSE)
                    ),
                    
                    pickerInput(
                      inputId='timeFormat',
                      label='时间格式',
                      choices=c(
                        '无'='',
                        '时'='H',
                        "时分"="HM",
                        '时分秒'='HMS'
                      ),
                      selected ='yyyymmdd',
                      multiple=FALSE,
                      options=list(`actions-box` = FALSE)
                    )
              )#,
              
              
              # pickerInput(
              #   inputId='varsOrder_varClass',
              #   label='转换为有序型变量',
              #   choices=c('无'='',names(data_varClass())),
              #   #selected =names(data_varClass())[1],
              #   multiple=FALSE,
              #   options = list(`actions-box` = FALSE)
              # )
            )
      )
    )
  })
  
  output$more3_varClass<-renderUI({
    # if(input$varsOrder_varClass!='') {
    #   chcs<-unique(data_varClass()[,input$varsOrder_varClass])} else {chcs<-''}
    list(
      # conditionalPanel(
      #   condition = "input['varsOrder_varClass']!=''",
      #   selectizeInput(
      #     inputId="order_varsOrder",
      #     label='有序变量各水平排序',
      #     choices=chcs,
      #     multiple=TRUE
      #   )
      # ),
      panel(status='primary',
            heading='保存数据集',
            textInputAddon(inputId='dataName_varClass',label='保存的数据名称',value='',placeholder = 'eg:data_newVarType',addon=icon('pencil'))
      )
    )
  })
  
  res_varClass<-reactive({
    input$go_varClass
    req(input$go_varClass)
    isolate({
      data_varClass()->dat
      if(input$auto_varClass){
        autoVarClass(data=dat,lenTab=input$lengthTab,thresh=input$threshold)->modeVars
        colnames(modeVars)->nameVars
        as.vector(modeVars)->modes
        for(i in 1:ncol(dat)){
          if(modeVars[i]=='char'){
            as.character(as.vector(dat[,i]))->dat[,i]
          }
          if(modeVars[i]=='num'){
            as.numeric(as.vector(dat[,i]))->dat[,i]
          }
        }
      }
      
      if(!input$auto_varClass){
        if(length(input$varsNum_varClass)>0){
          for(i in input$varsNum_varClass){
            as.numeric(as.vector(dat[,i]))->dat[,i]
          }
        }
        
        if(length(input$varsChar_varClass)>0){
          for(i in input$varsChar_varClass){
            as.character(as.vector(dat[,i]))->dat[,i]
          }
        }
        
        # if(input$varsOrder_varClass!=''){
        #   
        #   ordered(as.vector(dat[,input$varsOrder_varClass]),levels=input$order_varsOrder)->dat[,input$varsOrder_varClass]
        #   
        # }
        
        if(length(input$varsDate_varClass)>0){
          for(i in input$varsDate_varClass){
            parse_date_time(as.vector(dat[,i]),orders=paste(input$dateFormat,input$timeFormat,sep=''))->dat[,i]
          }
        }
        
        if(all(is.null(c(input$varsNum_varClass,input$varsDate_varClass,input$varsDate_varClass)))) {dat->dat}
      }
      if(input$dataName_varClass==''){
        assign(input$dataSel_varClass,dat,env=envKDAT1)
        lstKDAT1$Data[[input$dataSel_varClass]]<-dat
      } else {
        assign(input$dataName_varClass,dat,env=envKDAT1)
        lstKDAT1$Data[[input$dataName_varClass]]<-dat
      }
      
      assign('lstKDAT1',lstKDAT1,envir=envKDAT1)
      
      return(dat)
    })
  })
  
  output$summary_varClass<-renderPrint({
    input$go_varClass
    isolate({
      res_varClass()->dt
      sapply(dt,class)
      tryCatch(print(pander(head(dt))),error=function(e)print(head(dt)))
      sapply(dt,class)->y
      unique(y)->x
      sapply(x,function(i)names(y)[which(y==i)])->res
      tryCatch(print(pander(res)),error=function(e)print(res))
      
      skim(dt)
    })
    
  })
  
  
  
  
  
  ###### 生成新变量(varMnp) ######
  output$more1_varMnp<-renderUI({
    
    change_data()
    list(
      panel(status='primary',
            heading='选择数据集',
            pickerInput(
              inputId = "dataSel_varMnp",
              label = "选择数据集",
              choices = ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))],
              selected =ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))][1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
      ),
      panel(
        status='primary',
        heading='填写筛选条件',
        textInputAddon(
          'subset1',label='逻辑表达式',value='',addon=icon('pencil'),placeholder='eg:age<=100&age>=0'
        )
      )
    )
  })
  
  data_varMnp<-reactive({
    change_data()
    
    get(input$dataSel_varMnp,envKDAT1)->data_varMnp1
    return(data_varMnp1)
    
  })
  
  
  ### position here for filter
  
  # output$more0_Filter1<-renderUI({
  #   list(
  #     pickerInput(
  #       'Filter1',
  #       'choose filter vars',
  #       choices = c(names(data_varMnp())),
  #       multiple=T,
  #       options = list(`actions-box` = T))
  #   )
  #   
  # })
  # 
  # output$more1_Filter1<-renderUI({
  #   list(
  #     if(length(setdiff(input$Filter1,''))==0){
  #       NULL
  #     } else {
  #       shinyFilter(data_varMnp(),filter=setdiff(input$Filter1,''))
  #     }
  #   )
  # })
  
  
  ### position here for filter
  
  output$more2_varMnp<-renderUI({
    data_varMnp()->dat
    list(
      panel(status='primary',
            heading='选择操作类型',
            pickerInput(
              inputId='type_varMnp',
              label='创建变量的方式',
              choices=c(
                '多变量批量同一处理'='Batch',
                '生成单个变量'='Single'
              ),
              selected ='Single',
              multiple=FALSE,
              options= list(`actions-box` = TRUE)
            )
      ),
      
      panel(status='primary',
            heading='设置各类参数',
            textInputAddon(
              'newFormula',label='计算公式',value='',addon=icon('pencil'),placeholder='eg:mean(age,na.rm=T) or function(i){mean(i)}'
            ),
            
            pickerInput(
              inputId = "varSel_Mnp",
              label = "选择分类处理变量",
              c(names(data_dataImpt())),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            )
      ),
      
      conditionalPanel(
        condition="input['type_varMnp']=='Single'",
        panel(status='primary',
              heading='设置变量名',
              textInputAddon(
                'newVarName',label='变量名',value='',addon=icon('pencil'),placeholder='eg:newvar1'
              )
        )
      ),
      
      conditionalPanel(
        condition="input['type_varMnp']=='Batch'",
        
        
        panel(status='primary',
              heading='设置批量处理参数',
              pickerInput(
                inputId = "varSelBatch_Mnp",
                label = "批量处理的变量",
                names(data_dataImpt()),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
              ),
              
              textInputAddon('newTail',label='新变量后缀',value='_New',addon=icon('pencil'))
        )
      )
    )
  })
  
  
  res_varMnp<-reactive({
    input$go_varMnp
    req(input$go_varMnp)
    isolate({
      data_varMnp()->dat
      
      
      # if(length(setdiff(input$Filter1,''))==0){
      #   Ind<-T
      # } else {
      #   
      #   indMat<-sapply(setdiff(input$Filter1,''),function(i){
      #     
      #     if(class(dat[,i])%in%c('character','factor')){
      #       dat[,i]%in%input[[i]]
      #     } else {
      #       dat[,i]>=input[[i]][1]&dat[,i]<=input[[i]][2]
      #       
      #     }
      #     
      #   })
      #   
      #   apply(indMat,1,all)->Ind
      # }
      
      
      
      if(input$type_varMnp=='Single'){
        
        tryCatch(createVars(dat,
                   varNames=input$newVarName,
                   Formulas=input$newFormula,
                   byVars=input$varSel_Mnp,
                   subSets=input$subset1,batch=F),error=function(e)dat)->dat
      }
      
      if(input$type_varMnp=='Batch'){
        
        tryCatch(createVars(dat,
                   Formulas=input$newFormula,
                   byVars=input$varSel_Mnp,
                   subSets=input$subset1,
                   batch=T,
                   batchVars=input$varSelBatch_Mnp,
                   batchVarsTail=input$newTail),error=function(e)dat)->dat
        
      }
      
      
    })
    
    assign(input$dataSel_varMnp,dat,env=envKDAT1)
    lstKDAT1$Data[[input$dataSel_varMnp]]<-dat
    
    
    assign('lstKDAT1',lstKDAT1,envir=envKDAT1)
    
    return(dat)
  })
  
  observeEvent(input$go_varMnp,{  #### newly added for update picker input values.
    updatePickerInput(session,inputId = 'varSel_Mnp',choices = names(res_varMnp()))
    
    updatePickerInput(session,inputId = 'varSelBatch_Mnp',choices = names(res_varMnp()))
    
    updatePickerInput(session,inputId = 'Filter1',choices = names(res_varMnp()))
    
  })
  
  output$summary_varMnp<-renderPrint({
    res_varMnp()->dt
    tryCatch(print(pander(head(dt))),error=function(e)print(head(dt)))
    
  })
  
  
  ###### 数据导出(dataExpt) ######
  
  output$more1_dataExpt<-renderUI({
    
    change_data()
    #?#
    list(
      panel(status='primary',
            heading='选择处理的数据集',
            pickerInput(
              inputId = "dataSel_dataExpt",
              label = "选择数据集",
              choices = ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))],
              selected =ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))][1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
            #selectInput('dataSel_dataExpt','选择数据集',ls(envMadis)[-which(ls(envMadis)%in%c('envMadis','server','ui','LstMadis'))])
      )
    )
  })
  
  data_dataExpt<-reactive({
    
    change_data()
    #?#
    get(input$dataSel_dataExpt,envKDAT1)->dataExpt
    return(dataExpt)
  })
  
  output$more2_dataExpt<-renderUI({
    list(
      panel(status='primary',
            heading='保存的数据格式',
            pickerInput(
              inputId='dataType_dataExpt',
              label='选择数据格式',
              choices = c(
                '文本数据'='txtFile_dataExpt',
                'csv数据'='csvFile_dataExpt',
                'R数据文件'='RData_dataExpt'
              ),
              selected ='csvFile_dataExpt',
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
      ),
      panel(status='primary',
            heading='设定参数',
            conditionalPanel(
              condition = "input['dataType_dataExpt']=='txtFile_dataExpt'||input['dataType_dataExpt']=='csvFile_dataExpt'",
              awesomeCheckbox('quote_dataExpt','字符类型是否带引号',FALSE),
              pickerInput(
                inputId='sep_dataExpt',
                label='文件分隔符',
                choices = c(
                  '逗号分隔'=',',
                  '制表符分隔'='\t',
                  '空格分隔'=' '
                ),
                selected =',',
                multiple = FALSE,
                options = list(`actions-box` = FALSE)
              ),
              flowLayout(
                awesomeCheckbox('rowNames_dataExpt','是否保留行名',FALSE),
                awesomeCheckbox('colNames_dataExpt','是否保留列名',TRUE) 
              ),
              pickerInput(
                inputId='fileEncoding_dataExpt',
                label='字符集编码',
                choices = c(
                  '国标(GB18030)'='GB18030',
                  'UTF8编码'='utf8'
                ),
                selected ='GB18030',
                multiple = FALSE,
                options = list(`actions-box` = FALSE)
              )
            ),
            
            conditionalPanel(
              condition = "input['dataType_dataExpt']=='RData_dataExpt'",
              awesomeCheckbox('ascii_dataExpt','是否保存为ASCII格式？',FALSE)
            )
      ),
      panel(status='primary',
            heading='设定文件名称',
            textInputAddon(inputId='fileName_dataExpt','保存的文件名称',value='',placeholder = 'eg:myData',addon = icon('pencil'))
      )
    )
  })
  
  output$downloadData <- downloadHandler(
    
    filename=function(){
      if(input$dataType_dataExpt=='txtFile_dataExpt'){
        return(ifelse(input$fileName_dataExpt!='',paste0(input$fileName_dataExpt,'.txt'),paste0(input$dataSel_dataExpt,'.txt')))
      }
      
      if(input$dataType_dataExpt=='csvFile_dataExpt'){
        return(ifelse(input$fileName_dataExpt!='',paste0(input$fileName_dataExpt,'.csv'),paste0(input$dataSel_dataExpt,'.csv')))
      }
      
      if(input$dataType_dataExpt=='RData_dataExpt'){
        return(ifelse(input$fileName_dataExpt!='',paste0(input$fileName_dataExpt,'.RData'),paste0(input$dataSel_dataExpt,'.RData')))
      }
      
    },
    content = function(File) {
      if(input$dataType_dataExpt!='RData_dataExpt'){
        write.table(data_dataExpt(),file=File,sep=input$sep_dataExpt,quote=input$quote_dataExpt,
                    row.names=input$rowNames_dataExpt,col.names=input$colNames_dataExpt,
                    fileEncoding = input$fileEncoding_dataExpt)
      } else {
        assign(input$fileName_dataExpt,data_dataExpt())
        save(list=input$fileName_dataExpt,file=File,ascii=input$ascii_dataExpt)
      }
    }
  )
  
  output$summary_dataExpt<-renderPrint({
    print(head(data_dataExpt()))
  })
  
  
  
  
  ##### datatable ####
  
  
  output$more1_DT<-renderUI({
    change_data()
    list(
      panel(status='primary',
            heading='选择处理的数据集',
            pickerInput(
              inputId = "dataSel_DT",
              label = "选择数据集",
              choices = ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))],
              selected =ls(envKDAT1)[-which(ls(envKDAT1)%in%c('envKDAT1','server','ui','lstKDAT1'))][1],
              multiple = FALSE,
              options = list(`actions-box` = FALSE)
            )
      ),
      panel(status='primary',
            heading='填写筛选条件',
            textInputAddon(
              inputId='subset2',label='逻辑表达式',value='',addon=icon('pencil'),placeholder='eg:age<=100&age>=0'
            )
      )
    )
  })
  
  
  data_DT<-reactive({
    change_data()
    get(input$dataSel_DT,envKDAT1)->dataDT
    return(dataDT)
    
  })
  
  
  output$more0_Filter<-renderUI({
    list(
      pickerInput(
        'Filter',
        'choose filter vars',
        choices = c(names(data_DT())),
        multiple=T,
        options = list(`actions-box` = T))
    )
    
  })
  
  output$more1_Filter<-renderUI({
    list(
      if(length(setdiff(input$Filter,''))==0){
        NULL
      } else {
        shinyFilter(data_DT(),filter=setdiff(input$Filter,''))
      }
    )
  })
  
  
  output$more2_DT<-renderUI({
    list(
      panel(status='primary',
            pickerInput(
              inputId = "meanDT",
              label = "纳入计算均值的变量",
              choices = c(names(data_DT())),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),
            
            pickerInput(
              inputId = "sumDT",
              label = "纳入计算求和的变量",
              choices = c(names(data_DT())),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),
            
            pickerInput(
              inputId = "medianDT",
              label = "纳入计算中位数的变量",
              choices = c(names(data_DT())),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            ),
            
            pickerInput(
              inputId = "sdDT",
              label = "纳入计算标准差的变量",
              choices = c(names(data_DT())),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            )
      ),
      checkboxInput(inputId='count',label='是否包含当前条数?',value=TRUE),
      
      textInputAddon(inputId='otherMethod',label='自定义函数',value='',addon=icon('pencil'),placeholder='eg:meanAge=mean(age,na.rm=T)'),
      
      panel(status='primary',
            heading='选择维度变量',
            pickerInput(
              inputId = "byVarsMnp",
              label = "维度变量",
              choices = c(names(data_DT())),
              multiple = TRUE,
              options = list(`actions-box` = TRUE)
            )
            
      )
      
    )
  })
  
  
  
  res_DT<-eventReactive(input$go_DT,{
    
    data_DT()->dt
    
    
    if(length(setdiff(input$Filter,''))==0){
      dt<-dt
    } else {
      
      indMat<-sapply(setdiff(input$Filter,''),function(i){
        
        
        if(class(dt[,i])%in%c('character','factor')){
          dt[,i]%in%input[[i]]
        } else {
          dt[,i]>=input[[i]][1]&dt[,i]<=input[[i]][2]
          
        }
        
        
        
        
      })
      
      apply(indMat,1,all)->Ind
      dt<-dt[Ind,]
      
    }
    
    if(is.null(input$meanDT)){
      meanMethod=''
    } else {
      paste('mean_',input$meanDT,sep='')->meanVars
      paste("mean(",input$meanDT,",na.rm=T)")->meanForms
      paste(meanVars,'=',meanForms)->meanMethod
    }
    
    
    if(is.null(input$sumDT)){
      sumMethod=''
    } else {
      paste('sum_',input$sumDT,sep='')->sumVars
      paste("sum(",input$sumDT,",na.rm=T)")->sumForms
      paste(sumVars,'=',sumForms)->sumMethod
    }
    
    
    if(is.null(input$sdDT)){
      sdMethod=''
    } else {
      paste('sd_',input$sdDT,sep='')->sdVars
      paste("sd(",input$sdDT,",na.rm=T)")->sdForms
      paste(sdVars,'=',sdForms)->sdMethod
    }
    
    if(is.null(input$medianDT)){
      medianMethod=''
    } else {
      paste('median_',input$medianDT,sep='')->medianVars
      paste('median(',input$medianDT,',na.rm=T)')->medianForms
      paste(medianVars,'=',medianForms)->medianMethod
    }
    
    if(input$count){
      countMethod="count=.N"
    } else {
      countMethod=''
    }
    
    
    setdiff(c(countMethod,meanMethod,medianMethod,sdMethod,sumMethod,input$otherMethod),'')->methodAll
    
    if(methodAll==''){
      formsAll='count=.N'
    } else {
      paste(methodAll,collapse=';')->formsAll
    }
    
    tryCatch(dtMnp(
      data=dt,
      subSets=input$subset2,
      formulas=formsAll,
      byVars=input$byVarsMnp
    ),error=function(e)mtcars)->res
    return(res)
  })
  
  
  output$summary_dt<-renderPrint({
      data_DT()->dt
      sapply(dt,class)
      tryCatch(print(pander(head(dt))),error=function(e)print(head(dt)))
      sapply(dt,class)->y
      unique(y)->x
      sapply(x,function(i)names(y)[which(y==i)])->res
      tryCatch(print(pander(res)),error=function(e)print(res))
      # skim(dt)
  })
  
  output$resMnp<-renderDT(
    res_DT(),filter='top',extensions = 'Buttons',server=T
  )
  
  output$downloadRes <- downloadHandler(
    
    filename = function() {
      paste('myResult', Sys.Date(), sep = '.', 'xlsx')
    },
    content = function(File) {
        rio::export(res_DT(),file=File)
    }
  )
  
  
  
}


###### uiHeader ######

ui<-fluidPage(
  shinythemes::themeSelector(),
  tags$head(
    tags$style(
      type="text/css", "
      #loadmessage {
      position: fixed;
      top: 0px;
      left: 0px;
      width: 100%;
      padding: 10px 0px 10px 0px;
      text-align: center;
      font-weight: bold;
      font-size: 100%;
      color: #000000;
      background-color: #CCFF66;
      z-index: 105;
      }
      ")
  ),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",tags$div("请耐心等待...",id="loadmessage")),
  
  
  
  
  navbarPage(
    # strong('Kindo Data Analysis Toolbox-1'),
    title=div(icon("r-project"), strong("Kindo Data Analysis Toolbox-1")),
    windowTitle = 'Kindo',
    ###### 导入数据功能(data_Impt)######
    
    tabPanel(
      icon=icon('file-import'),
      '导入本地数据',
      sidebarLayout(
        position='left',
        sidebarPanel(
          panel(status='primary',
                heading='导入数据',
                fileInput(
                  'file_dataImpt', 
                  '选择本地数据',
                  accept = c(
                    '.csv',
                    '.tsv',
                    '.txt'
                  )
                ),
                helpText('注意：数据需为txt或csv格式文件，或复制表格数据(excel，csv等文件)到下面的窗口中'),
                aceEditor("text_dataImpt", value=readr:::format_tsv(mtcars), mode="r", theme="chrome",height="150px")
                
          ),
          
          uiOutput('args_dataImpt'),
          uiOutput('more1_dataImpt'),
          
          actionBttn('go_dataImpt','确定')
        ),
        
        mainPanel(
          panel(status='primary',
                heading='载入小部分数据试错',
                verbatimTextOutput('dataHead')
          ),
          panel(
            heading='原始数据变量描述',
            verbatimTextOutput('varClass_dataImpt'),
            status='primary'
          ),
          #hr(),
          panel(
            heading='载入数据查看',
            verbatimTextOutput('head_dataImpt'),
            status='primary'
          )
          
        )
        
      )
    ), ## 数据读取
    
    
    ###### 数据处理 ######
    navbarMenu(
      '数据处理',
      icon=icon('wrench'),
      
      
      ###### 数据处理-变量类型转换 ######
      tabPanel(
        icon=icon('retweet'),
        strong('变量类型转换'),
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_varClass'),
            uiOutput('more2_varClass'),
            uiOutput('more3_varClass'),
            actionBttn('go_varClass','确定')
          ),
          mainPanel(
            panel(
              heading='数据查看',
              status='primary',
              verbatimTextOutput('summary_varClass')
            )
            
          )
        )
        
      ),
      
      
      ###### 数据处理-生成变量 ######
      tabPanel(
        icon=icon('plus'),
        '生成新变量',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_varMnp'),
            # panel(
            #   heading = '自动配置筛选条件',
            #   status='primary',
            #   uiOutput('more0_Filter1'),
            #   uiOutput('more1_Filter1')
            # ),
            uiOutput('more2_varMnp'),
            
            actionBttn('go_varMnp','确定')
            
            
            
          ),
          mainPanel(
            panel(
              heading='数据查看',
              status='primary',
              verbatimTextOutput('summary_varMnp')
            )
            
          )
        )
        
      ),
      
      ###### 数据处理-导出数据集 ######
      tabPanel(
        icon=icon('download'),
        '数据导出',
        sidebarLayout(
          sidebarPanel(
            uiOutput('more1_dataExpt'),
            uiOutput('more2_dataExpt'),
            downloadButton("downloadData", "Download")
          ),
          
          mainPanel(
            verbatimTextOutput('summary_dataExpt')
          )
        )
      )
      
      
    ),
    
    
    
    ####### datatable ###### 
    
    tabPanel(
      icon=icon('th'),
      '分析统计',
      sidebarLayout(
        sidebarPanel(
          uiOutput('more1_DT'),
          panel(
            heading = '自动配置筛选条件',
            uiOutput('more0_Filter'),
            uiOutput('more1_Filter'),
            status = 'primary'
          ),
          uiOutput('more2_DT'),
          actionBttn('go_DT','确定')
          
        ),
        mainPanel(
          panel(
            heading='查看数据',
            status='primary',
            verbatimTextOutput('summary_dt')
          ),
          panel(
            heading = '结果',
            status='primary',
            
            DTOutput('resMnp'),
            
            downloadButton("downloadRes", "Download")
          )
          
        )
      )
    )
    
    
  )
)


shinyApp(ui = ui, server = server)
