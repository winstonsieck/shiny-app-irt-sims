# IRT Sims App


# The UI

ui <- fluidPage(
  fluidRow(
    column(4, 
           "Test Taker Distribution",
           numericInput("np", label = "Number of People", value = 500, min = 1),
    ),
    column(4, 
           "Item Distribution",
           numericInput("ni", label = "Number of Items", value = 50, min = 1),
           numericInput("hard", label = "Hardness", value = 2, min = 0, step = 0.1),
           numericInput("easy", label = "Easiness", value = 2, min = 0, step = 0.1)
    ),
    column(4,
           "Plots",
# placeholder for plot controls
    )
  ),
  fluidRow(
    column(9, plotOutput("testinfo")),
    column(3, verbatimTextOutput("raschmodel"))
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
  theta_true <- reactive(rnorm(input$np,0,1))
  b_true <- reactive(6*(rbeta(input$ni,input$hard,input$easy)-.5))
  
  df <- reactive(sim_test_dat(input$np,input$ni,theta_true(),b_true()))
  
  rasch.m <- reactive(mirt(df(), 1, itemtype = "Rasch", verbose = F))
  
  output$testinfo <- renderPlot({
    testInfoPlot(rasch.m(), adj_factor = 2)
  }, res = 96)
  
  output$raschmodel <- renderPrint({
    rasch.m()
  })
}



# Run the application 
shinyApp(ui = ui, server = server)
