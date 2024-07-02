library(shiny)

ui <- fluidPage(
  titlePanel("Bias-Variance Tradeoff Visualization"),
  fluidRow(
    
    column(3,
           selectInput(inputId = "true_model",
                       label = "Select a 'true' model",
                       choices = c("Linear", "Non-linear"),
                       selected = "Linear")),
    
    column(3,
           sliderInput(inputId = "epsilon",
                       label = "Select variability",
                       min = 0,
                       max = 5,
                       value = 1,
                       step = 0.5)),
    
    column(3,
           sliderInput(inputId = "flex",
                       label = "Select flexibility",
                       min = 0.1,
                       max = 10,
                       value = 1,
                       step = 1)),
    
    
    column(3, plotOutput("myLegend"))
    
    
    
  ),
  fluidRow(
    column(4, plotOutput("truePlot")),
    column(4, plotOutput("Plot1")),
    column(4, plotOutput("Plot2"))
  ),
  
  fluidRow(
    column(4, plotOutput("Plot3")),
    column(4, plotOutput("Plot4")),
    column(4, plotOutput("Plot5"))
  ),
  fluidRow(
    column(4, plotOutput("Plot6")),
    column(4, plotOutput("Plot7")),
    column(4, plotOutput("Plot8"))
  ),
  fluidRow(
    column(4, plotOutput("Plot9")),
    column(4, plotOutput("Plot10"))
  )
)

server <- function(input, output) {
  n <- 100
  a <- 5
  b <- 0.5
  c <- 0.76
  degree <- 1
  
  df <- reactive({
    
    if(input$true_model == "Linear")
    {
      set.seed(2024)
      
      # simulate data
      x <- runif(n , min = 30, max = 60)   # input/predictor
      
      e1 <- rnorm(n, mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n, mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n, mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n, mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n, mean = 0, sd = input$epsilon)  # error
      e6 <- rnorm(n, mean = 0, sd = input$epsilon)
      e7 <- rnorm(n, mean = 0, sd = input$epsilon)
      e8 <- rnorm(n, mean = 0, sd = input$epsilon)
      e9 <- rnorm(n, mean = 0, sd = input$epsilon)
      e10 <- rnorm(n, mean = 0, sd = input$epsilon)
      
      fx <- a + (b * x)  # true function
      
      y1 <- fx + e1    # observed responses
      y2 <- fx + e2    # observed responses
      y3 <- fx + e3    # observed responses
      y4 <- fx + e4    # observed responses
      y5 <- fx + e5    # observed responses
      y6 <- fx + e6
      y7 <- fx + e7
      y8 <- fx + e8
      y9 <- fx + e9
      y10 <- fx + e10
      
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5, response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10 = y10)  
    }
    else 
    {
      set.seed(2024)
      
      # simulate data
      x <- runif(n, min = 30, max = 60)   # input/predictor
      
      e1 <- rnorm(n , mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n , mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n , mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n , mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n , mean = 0, sd = input$epsilon)  # error
      e6 <- rnorm(n , mean = 0, sd = input$epsilon)
      e7 <- rnorm(n , mean = 0, sd = input$epsilon)
      e8 <- rnorm(n , mean = 0, sd = input$epsilon)
      e9 <- rnorm(n , mean = 0, sd = input$epsilon)
      e10 <- rnorm(n , mean = 0, sd = input$epsilon)
      
      fx = a + b * x + c * x^2
      
      #function with noise added:
      y1 <- fx+e1
      y2 <- fx+e2
      y3 <- fx+e3
      y4 <- fx+e4
      y5 <- fx+e5
      y6 <- fx+e6
      y7 <- fx+e7
      y8 <- fx+e8
      y9 <- fx+e9
      y10 <- fx+e10
      
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5, response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10 = y10)}
   

  
    #use sd= 2, n = 100, degree = 1, RMSE for toy datasets for box plot
    
    
