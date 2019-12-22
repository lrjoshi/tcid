library(shiny)
library(rhandsontable)
library(shinyWidgets)

#function to calculate tcid50
#this function takes a table format
#first column name and second column no of wells

tcid50 <- function (table,inoculum,dilution,logdilution){
  table$intermediate= ((0-((table$Wells)-dilution))/dilution)
  
  table$calcs= table$intermediate*(1-table$intermediate)
  
  
  sum_int=sum(table$intermediate)
  sum_cals=sum(table$calcs)
  tcid= ((logdilution)*(0.5-sum_int))+(10-log10(inoculum))
  return(tcid)
  
}


did_recalc <- FALSE

ui <- fluidPage(
  theme = "bootstrap.css",
  setBackgroundColor("ghostwhite"),
  fluidRow(
    column(8, align="center", offset = 2,
  titlePanel("TCID50 Calculator")),
  
  br(),
  br(),
  
  fluidRow(
    column(10,offset=4,
           numericInput("inoculum","Amount of inoculum added per well (mL)",min=0,max=10,value=0.1))
  ),

  fluidRow(
    column(10,offset=4,
           numericInput("dilution","How many wells used per dilution",value=4))     
  ),  
  
  fluidRow(
    column(10,offset=4,
           numericInput("logdilution","Log fold dilution (Log10=1)",value=1))      
  ),   
  
 
  fluidRow(
    column(10,offset=4,
           rHandsontableOutput('table'))         
  ), 
  
 br(),
 
  fluidRow(
    column(10,offset=4,
           textOutput('result'))          
  ),
  
  br(),
  
  fluidRow(
    column(10,offset=4,
           actionButton("recalc", "Reset"))              
  ) ,
 
 
 br(),
 br(),
 fluidRow(
   column(10,offset=4,
          textOutput("message"))            
 ) 
 
 
 
)
)
server <- function(input,output,session)({
  values <- reactiveValues(data=data.frame("Dilution"=c("Undiluted","-1","-2","-3","-4",
                                                        "-5","-6","-7","-8",
                                                        "-9","-10"),
                                           "Wells"=c(4,4,4,4,4,0,0,0,0,0,0)))
  
  
  observe({
    input$recalc
    values$data <- data.frame("Dilution"=c("Undiluted","-1","-2","-3","-4",
                                           "-5","-6","-7","-8",
                                           "-9","-10"),
                              "Wells"=c(4,4,4,4,4,0,0,0,0,0,0))
  })
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data)
  })
  
  
  output$message <- renderText ({
    method ="Spearmann-Karber Titer Calculator."
    source= "Translated from D Gerstner."
    paste(method,source,sep  ="\n")
                                         
  })
  
  output$result <- renderText({ 
    inoculum=input$inoculum
    dilution=input$dilution
    logdilution=input$logdilution
   paste("Titer (log10 TCID50/mL) : ", tcid50(values$data,inoculum,dilution,logdilution))
  })
}) 

shinyApp(ui = ui, server = server)