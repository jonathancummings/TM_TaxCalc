############################################################
## Shiny App for Nottingham tax assessment
## J.W. Cummings
## 2/17/2019
################################################

# Load libraries
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)

arts<-read_csv("Articles.csv")
maxCost<-sum(arts$Cost)

ui <- fluidPage(
  theme = shinytheme("readable"),
  headerPanel("Nottingham, NH Tax Calculation App"),# title of the page
  hr(),
  p("This app calculate your estimated property taxes resulting from articles
    up for a vote at Town Meeting"),
  sidebarLayout( # page will have a side bar and then a main section on the page 
    sidebarPanel( # format sidebar inputs
      width = 4,
      a(href="https://www.axisgis.com/NottinghamNH/", "(link to Nottingham tax maps)"),
      h4("Property Value"),
      p("Enter your assessed property value:"),      
            numericInput(inputId="AssessedValue", label="Assessed Value", value=285000, step=5000),
      hr(),
      a(href="https://www.nottingham-nh.gov/election-voting-information/pages/2019-ballot-town-meeting-documents", "(link to warrant articles)"),
      h4("Article Status"),
      p("Select the status for each article (proposed, passed, rejected) below:"),
      selectInput(inputId="Art1", label=arts$Name[1], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art2", label=arts$Name[2], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art3", label=arts$Name[3], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art4", label=arts$Name[4], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art5", label=arts$Name[5], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art6", label=arts$Name[6], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art7", label=arts$Name[7], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art8", label=arts$Name[8], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art9", label=arts$Name[9], choices=c("Proposed","Passed","Rejected")),
      selectInput(inputId="Art10", label=arts$Name[10], choices=c("Proposed","Passed","Rejected"))
    ),
    mainPanel( # setup for creating output plots
      width=8,
      DT::dataTableOutput("Taxes"),
      textOutput("Text1"),
      htmlOutput("Text2")
    )
  )
)

server <- function(input, output) {   # code to create output using render
  # Update tax tibble based on voting results
  arts.calc<-reactive({
    arts<-arts
    status_list<-lapply(1:nrow(arts), function(i) {input[[paste0('Art',i)]]})
    for (i in 1:nrow(arts)) {
      if (status_list[[i]] == "Passed") {
        arts$Status[i]="Passed"
        arts$Use.P[i]<-0
      }
      else if (status_list[[i]] == "Rejected") {
        arts$Status[i]="Rejected"
        arts$Use.P[i]<-0
        arts$Use.R[i]<-1
      }
    }
    arts<-arts %>%
      mutate(Tax=Cost/1000*input$AssessedValue)
    maxTax<-maxCost/1000*input$AssessedValue
    R.Tax<-sum(arts$Cost/1000*arts$Use.R*input$AssessedValue)
    Pro.Tax<-sum(arts$Cost/1000*arts$Use.P*input$AssessedValue)
    TTax<-maxTax-R.Tax
    Pass.Tax<-abs(maxTax-R.Tax-Pro.Tax)
    return(list(arts=arts,maxTax=maxTax,R.Tax=R.Tax,Pro.Tax=Pro.Tax,TTax=TTax,Pass.Tax=Pass.Tax))
  })# Code to do all calculations for subsequent output here, not working currently

  
  output$Taxes <- DT::renderDataTable({
    arts<-arts.calc()$arts
    arts<-arts[,-grep("Use",names(arts))]
    arts
  },colnames=c("Cost (per $1000)"= "Cost"))
  output$Text1 <- renderText({
    paste0(c("Based on your assessed property value of = $",
            sprintf("%.2f",input$AssessedValue)," your tax impact would be: $",
            sprintf("%.2f",arts.calc()$TTax)))})
    output$Text2<-renderUI({
      HTML(
        paste0('<br/>',
               "Total potential impact = $" , sprintf("%.2f",arts.calc()$TTax),'<br/>',
               "Remaining proposed impact = $" , sprintf("%.2f",arts.calc()$Pro.Tax),'<br/>',
               '<br/>',
               tags$strong("Passed tax impact = $",sprintf("%.2f",arts.calc()$Pass.Tax)),'<br/>',
               "Rejected impact = $" , sprintf("%.2f",arts.calc()$R.Tax))
      )
    })
  }

shinyApp(ui, server)