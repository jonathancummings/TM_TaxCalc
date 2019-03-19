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

taxHist<-read_csv("History - Nottingham.csv")
arts<-read_csv("Articles.csv")
maxCost<-sum(arts$Cost)

ui <- navbarPage(title = "Town Tax App",
  tabPanel("Documentation",
    p("The tabs in this Shiny application help calculate and communicate local tax rates and
      tax impacts."),
    p("If you find this application useful please consider a donation to compensate me for development
      of this application and to offset the web hosting costs required to keep the application 
      running:",a(href="https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=KQLVYHWWYVS9S&source=url",
      "(Donate)")),
    p("The", strong("Nottingham, NH Tax History"), "tab displays tax rates and the tax impact of
      those tax rates given your assessed value in past tax years and the estimated tax impact for
      the current year.  As of 3/18/2019 the town and local school rates are based on the Nottingham
      election results while the state school rate and county rate are copied from the prior year and
      therefore are likely incorrect estimates."),
    p("The", strong("Nottingham TM-Day"), "tab provides the estimated tax rates and impacts given
      an assessed value for the warrant articles voted upon on town meeting day.  As of 3/18/2019
      the rates displayed are for the articles voted on at the 3/12/2019 town meeting."),
    p("* The estimated taxes come from town reporting providing a comparative guide to the cost
      of each article if passed and therefore are estimated rates and will likely differ from
      the realized rates.")
  ), # end tabPanel 1
  tabPanel("Nottingham, NH Tax History",
    h1("Nottingham, NH Tax History and Current Year Estimated Taxes"),# title of the page
    p("The application on this tab displays prior year tax rates in the town of Nottingham, NH and
    calculates your estimated property taxes (2019E) by tax category given your assessed value.  As of
    3/18/2019 the town and local school rates are based on the Nottingham election results while
    the state school rate and county rate are copied from the prior year and therefore are likely
    incorrect estimates."),
    hr(),
    sidebarLayout(
      sidebarPanel( # format sidebar inputs
        width = 4,
        a(href="https://www.axisgis.com/NottinghamNH/", "(link to Nottingham tax maps)"),
        h4("Property Value"),
        p("Enter your assessed property value:"),      
        numericInput(inputId="AssessedValuet1", label="Assessed Value", value=285000, step=5000),
        textOutput("textTotal")
      ), # end sidbarPanel
      mainPanel( # setup for creating output plots
        h4("Tax history and estimated taxes for the current year"),
        p("Tax rates per $1000 of assessed value:"), 
        tableOutput("taxRateHist"),
        p("Estimated tax history based on assessed value:"),
        tableOutput("taxHist"),
        p("* The 2019E values are only a comparative guide based on town estimates supplied
        with the articles. The 2019E values don't account for many factors likely to 
        result in rates differing from the estimates. The town passed articles increasing
        both the tax credits provided to veterans and the number of individuals eligible for
        those credits. The total taxable assessed value used to create the estimates use
        last year's values, but this year's tax base may differ with new buildings and
        property improvements or depreciation. Other unknowns are the amount of offsetting
        revenues, state aid, grants, and selectmen contributing money from unassigned funds,
        as well as the state school and county rates.
        Setting the tax rate is a complicated process administered by the State of New
        Hampshire each fall. (Reference the MS-1 in the Town Report)")
      ) # end mainPanel
    ) # end sidebarLayout
  ), # end tabPanel
  tabPanel("Nottingham TM-Day",
  theme = shinytheme("readable"),
  h1("Nottingham, NH Town Meeting Tax Calculation App"),# title of the page
  p("This app calculate your estimated* property taxes resulting from articles
    up for a vote at Town Meeting."),
  hr(),
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
    ), # end sidebarPanel
    mainPanel( # setup for creating output plots
      width=8,
      DT::dataTableOutput("Taxes"),
      textOutput("Text1"),
      htmlOutput("Text2"),
      p("* The estimate on the warrant articles is only a comparative guide to the
        impact of each article if passed. The actual impact may differ due to changes
        in the tax base and other state and local budget decisions.")
      ) # end mainPanel
    ) # end sidebarLayout
  ) # end tabPanel
) # end navbarPage

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
  })

  output$taxRateHist<-renderTable(taxHist)
  output$taxHist<-renderTable({
    taxHist[,2:ncol(taxHist)]<-taxHist[,2:ncol(taxHist)]/1000*input$AssessedValuet1
    taxHist
  })
  output$textTotal <- renderText({
    paste0(c("Based on your assessed property value your estimated taxes will be: $",
             sprintf("%.2f",taxHist[nrow(taxHist),ncol(taxHist)]/1000*input$AssessedValuet1)))})
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