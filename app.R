# IRT Sims App


# The UI
library(shinythemes)


ui <- fluidPage(
  #theme = shinytheme("superhero"),
                
  titlePanel(
    div(style="margin-bottom:50px;",
      h1("Rasch Model Simulations", align="center")
    )),
  
  fluidRow(
    column(4, 
           tags$h4("Sample Sizes"),
#           selectInput("np", label = "Number of People", c(500,750,1000)),
           sliderInput("np", "Number of People", value = 500, min = 500, max = 1000, step=50),
           sliderInput("ni", "Number of Items", value = 50, min = 10, max = 1000, step=10),
#           numericInput("ni", label = "Number of Items", value = 50, min = 1),
    ),
    column(4, 
           tags$h4("Item Distribution"),
           selectInput("hard", label = "Hardness", c(0,.25,.5,1,2,4,16,256), selected=4),
           selectInput("easy", label = "Easiness", c(0,.25,.5,1,2,4,16,256), selected=1)
#           numericInput("hard", label = "Hardness", value = 2, min = 0, step = 0.1),
#           numericInput("easy", label = "Easiness", value = 2, min = 0, step = 0.1)
    ),
    column(4,
           tags$h4("Display Settings"),
           actionButton("simulate", "Run Simulation")
    )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Test Info Curve", plotOutput("testinfo")),
      tabPanel("Model Details", verbatimTextOutput("raschmodel"))
    )
  )
)



# Server side

server <- function(input, output, session) {
  
  # supporting libraries, functions
  library(mirt)
  library(ggmirt)
  library(psych)
  library(dplyr)
  library(ggplot2)
  
  logf <- function(theta=0,b=0) {
    1 / (1 + exp(-(theta-b)))
  }
  
  sim_test_dat <- function(np,ni,theta_true,b_true) {
    corr <- matrix(0,np,ni)
    
    for (p in 1:np) {
      for (i in 1:ni) {
        pcor <- logf(theta_true[p],b_true[i])
        corr[p,i] <- rbinom(1,1,pcor)
      }
    }
    return(data.frame(corr))
  }
  
  # output rendering  
  v <- reactiveValues(x=NULL)
  observeEvent(input$np, {v$x <- as.numeric(input$np)} )
  observeEvent(v$x, {if(v$x != as.numeric(input$ni)){
    updateSliderInput(session,"ni", max = .1*v$x)
  }  })
    
  np <- eventReactive(input$simulate, as.numeric(input$np))
  ni <- eventReactive(input$simulate, as.numeric(input$ni))
  hard <- eventReactive(input$simulate, as.numeric(input$hard))
  easy <- eventReactive(input$simulate, as.numeric(input$easy))
  
  theta_true <- eventReactive(input$simulate, { rnorm(np(),0,1) })
  b_true <- eventReactive(input$simulate, {
    6*(rbeta(ni(),hard(),easy())-.5) })
  
  df <- eventReactive(input$simulate, {
    sim_test_dat(np(),ni(),theta_true(),b_true()) })
  
  rasch.m <- eventReactive(input$simulate, {
    mirt(df(), 1, itemtype = "Rasch", verbose = F) })
  
  output$testinfo <- renderPlot({
    testInfoPlot(rasch.m(), adj_factor = 2)
  }, res = 96)
  
  output$raschmodel <- renderPrint({
    rasch.m()
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
