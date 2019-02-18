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
    # for(i in 1:nrow(arts)){
    #   inputuse<-paste0("input$Art",as.character(i))
    #   if(inputuse=="Passed"){
    #     arts$Status[i]="Passed"
    #     arts$Use.P[i]<-0
    #   }
    #   if(inputuse=="Rejected"){
    #     arts$Status[i]="Rejected"
    #     arts$Use.P[i]<-0
    #     arts$Use.R[i]<-1
    #   }
    # }
    if(input$Art1=="Passed"){
      arts$Status[1]="Passed"
      arts$Use.P[1]<-0
    }
    if(input$Art2=="Passed"){
      arts$Status[2]="Passed"
      arts$Use.P[2]<-0
    }
    if(input$Art3=="Passed"){
      arts$Status[3]="Passed"
      arts$Use.P[3]<-0
    }
    if(input$Art4=="Passed"){
      arts$Status[4]="Passed"
      arts$Use.P[4]<-0
    }
    if(input$Art5=="Passed"){
      arts$Status[5]="Passed"
      arts$Use.P[5]<-0
    }
    if(input$Art6=="Passed"){
      arts$Status[6]="Passed"
      arts$Use.P[6]<-0
    }
    if(input$Art7=="Passed"){
      arts$Status[7]="Passed"
      arts$Use.P[7]<-0
    }
    if(input$Art8=="Passed"){
      arts$Status[8]="Passed"
      arts$Use.P[8]<-0
    }
    if(input$Art9=="Passed"){
      arts$Status[9]="Passed"
      arts$Use.P[9]<-0
    }
    if(input$Art10=="Passed"){
      arts$Status[10]="Passed"
      arts$Use.P[10]<-0
    }
    if(input$Art1=="Rejected"){
      arts$Status[1]="Rejected"
      arts$Use.P[1]<-0
      arts$Use.R[1]<-1
    }
    if(input$Art2=="Rejected"){
      arts$Status[2]="Rejected"
      arts$Use.P[2]<-0
      arts$Use.R[2]<-1
    }
    if(input$Art3=="Rejected"){
      arts$Status[3]="Rejected"
      arts$Use.P[3]<-0
      arts$Use.R[3]<-1
    }
    if(input$Art4=="Rejected"){
      arts$Status[4]="Rejected"
      arts$Use.P[4]<-0
      arts$Use.R[4]<-1
    }
    if(input$Art5=="Rejected"){
      arts$Status[5]="Rejected"
      arts$Use.P[5]<-0
      arts$Use.R[5]<-1
    }
    if(input$Art6=="Rejected"){
      arts$Status[6]="Rejected"
      arts$Use.P[6]<-0
      arts$Use.R[6]<-1
    }
    if(input$Art7=="Rejected"){
      arts$Status[7]="Rejected"
      arts$Use.P[7]<-0
      arts$Use.R[7]<-1
    }
    if(input$Art8=="Rejected"){
      arts$Status[8]="Rejected"
      arts$Use.P[8]<-0
      arts$Use.R[8]<-1
    }
    if(input$Art9=="Rejected"){
      arts$Status[9]="Rejected"
      arts$Use.P[9]<-0
      arts$Use.R[9]<-1
    }
    if(input$Art10=="Rejected"){
      arts$Status[10]="Rejected"
      arts$Use.P[10]<-0
      arts$Use.R[10]<-1
    }
    arts<-arts %>%
      mutate(Tax=Cost/1000*input$AssessedValue)
    # arts$Cost <- sprintf("%.3f",arts$Cost)
    # arts$Tax_If_Passed <- sprintf("%.2f",arts$Tax_If_Passed)
    maxTax<-maxCost/1000*input$AssessedValue
    R.Tax<-sum(arts$Cost/1000*arts$Use.R*input$AssessedValue)
    Pro.Tax<-sum(arts$Cost/1000*arts$Use.P*input$AssessedValue)
    TTax<-maxTax-R.Tax
    Pass.Tax<-abs(maxTax-R.Tax-Pro.Tax)
    #arts.calc<-(list(arts=arts,maxTax=maxTax,R.Tax=R.Tax,Pro.Tax=Pro.Tax,TTax=TTax,Pass.Tax=Pass.Tax))
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