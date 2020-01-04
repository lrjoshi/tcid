library(shiny)
library(rhandsontable)
library(shinyWidgets)
library (flexdashboard)
library (waiter)

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
  
  use_waiter(),
  waiter_show_on_load(spin_wandering_cubes()), # show on load
  
  theme = "bootstrap.css",
  setBackgroundColor("ghostwhite"),
  fluidRow(
    column(8, align="center", offset = 2,
           span(titlePanel("TCID50 Calculator"),style="color:DarkRed")),
    
    br(),
    br(),
    
    fluidRow(
      column(4,offset=3,
             numericInput("inoculum","Amount of inoculum added per well (mL)",min=0,max=10,value=0.1))
    ),
    
    fluidRow(
      column(4,offset=3,
             numericInput("dilution","How many wells used per dilution",value=4))     
    ),  
    
    fluidRow(
      column(4,offset=3,
             numericInput("logdilution","Log fold dilution (Log10=1)",value=1))      
    ),   
    
    
    
    splitLayout(
      #style = "border: 1px solid silver;",
      cellWidths = 700,
      cellArgs = list(style = "padding: 2px"),
      column(4,offset=8,
             rHandsontableOutput('table')),         
      
      column(4,offset=4,
             gaugeOutput("gauge"))         
    ) , 
    
    
    br(),
    
    fluidRow(
      column(10,offset=3,
             span(textOutput('result'),style="color:DarkBlue"))          
    ),
    
    br(),
    
    fluidRow(
      column(10,offset=3,
             actionButton("recalc", "Reset"))              
    ) ,
    
    
    br(),
    fluidRow(
      column(10,offset=1,
             span( textOutput("usage"),style="color:grey")),  
      fluidRow(
        column(10,offset=5,
               span( uiOutput("tab"),style="color:blue"))
      )
    )
  )  
  
)

server <- function(input,output,session)({
  
  Sys.sleep(1) # do something that takes time
  hide_waiter()
  
  
  values <- reactiveValues(data=data.frame("Dilution"=c("Undiluted","-1","-2","-3","-4",
                                                        "-5","-6","-7","-8",
                                                        "-9","-10"),
                                           "Wells"=c(4,4,4,1,0,0,0,0,0,0,0)))
  
  
  
  
  observe({
    input$recalc
    values$data <- data.frame("Dilution"=c("Undiluted","-1","-2","-3","-4",
                                           "-5","-6","-7","-8",
                                           "-9","-10"),
                              "Wells"=c(4,4,4,1,0,0,0,0,0,0,0))
  })
  
  observe({
    if(!is.null(input$table))
      values$data <- hot_to_r(input$table)
  })
  
  
  output$table <- renderRHandsontable({
    rhandsontable(values$data) %>% 
      hot_col("Dilution",readOnly = T)
  })
  
  
  output$usage <- renderText ({
    
    paste( "Example: Suppose you made 10 fold dilution of your inoculum,added 100 ul per well and \n
  you used four wells per dilution. If there were four out of four wells positive \n
  in minus one, minus two ond minus three dilution, and only one out of four wells \n
  positive in minus -3 dilution, then the titer of your stock would be 3.75 TCID50/mL.\n
  Usually, for undiluted inoculum all the four wells can be considered positive.Based on Spearmann-Karber Method.
  Translated from D Gerstner. Developed by Lok Raj Joshi.")
    
  })
  
  
  url <- a("click here ", href="https://www.lokraj.me/")
  output$tab <- renderUI({
    tagList("To visit my website", url)
  })
  
  
  
  
  output$result <- renderText({ 
    inoculum=input$inoculum
    dilution=input$dilution
    logdilution=input$logdilution
    paste("Titer (log10 TCID50/mL) : ", round(tcid50(values$data,inoculum,dilution,logdilution),digits=2))
    
  })
  
  
  output$gauge <- renderGauge({ 
    inoculum=input$inoculum
    dilution=input$dilution
    logdilution=input$logdilution
    titer= round(tcid50(values$data,inoculum,dilution,logdilution),digits=2)
    gauge(titer,
          min=0,
          max=12,
          sectors = gaugeSectors(success = c(0.0, 4), 
                                 warning = c(5, 7),
                                 danger = c(8, 12)))
  }) 
  
  
}) 

shinyApp(ui = ui, server = server)