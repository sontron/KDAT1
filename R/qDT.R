#' qDT
#' 
#' quick DT function 
#' 
#' render a DT in shiny
#' 
#' @examples 
#' fnDT(mtcars)
#' 
#' @export


qDT<-function(dt,...){
  require(DT)
  require(shiny)
  require(shinyWidgets)
  as.data.frame(dt)->dt
  if (interactive()) {
    shinyApp(options=list(...),
             ui = fluidPage(
               sidebarLayout(
                 sidebarPanel(
                   panel(
                     heading = 'vars to keep',
                     uiOutput('more1'),
                     status='primary'
                   )
                   
                 ),
                 mainPanel(
                   panel(
                     heading = 'DataTable Outputs',
                     DTOutput('tbl'),
                     status='primary'
                   )
                   
                 )
               )
             ),
             server = function(input, output) {
               
               output$more1<-renderUI({
                 list(
                   pickerInput(inputId = 'varsKeep',
                               label = 'choose vars to show',
                               choices = names(dt),
                               selected = names(dt),
                               multiple = T,
                               options = list(`actions-box` = TRUE))
                 )
               })
               
               output$tbl = renderDT(
                 dt[,input$varsKeep,drop=F],
                 server=T,filter='top'
               )
             }
    )
  }
}

