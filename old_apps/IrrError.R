library(tidyverse)
library(shiny)

ui <- fluidPage(

  # Application title
  titlePanel("Irreducible/Unaccountable Error"),
  
  fluidRow(
    
    column(3,
           selectInput(inputId = "true_model",
                        label = "Select a model",
                        choices = c("Linear", "Non-linear"),
                        selected = "Linear")),
    

    column(3,
           sliderInput(inputId = "epsilon",
                       label = "Select error variability",
                       min = 0,
                       max = 10,
                       value = 1,
                       step = 0.5,
                       animate = TRUE))
  ),

  fluidRow(
    column(4, 
           plotOutput("truePlot")),
    column(4,
           plotOutput("observedPlot"))
  )
)


server <- function(input, output) {
  
  df <- reactive({
    
    if(input$true_model == "Linear")
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      a <- 3
      b <- 0.87
      c <- 0.5
      fx <- a + (b * x)  # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }

    else 
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      a <- 3
      b <- 0.87
      c <- 0.5
      fx <- a + (b * sqrt(x)) + (c * sin(x))   # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }
    
    return(toy_data)
  })
  
  
  output$truePlot <- renderPlot({
    
    ggplot(data = df(), aes(x = inp, y = true_form)) + 
      geom_point() + labs(title = "True (unknown) relationship without error", y = "f(x)", x = "x")
    
  })
  
  
  
  output$observedPlot <- renderPlot({
    
    ggplot(data = df(), aes(x = inp, y = response)) + 
      geom_point() + labs(title = "Observed relationship", y = "y", x = "x")
    
  })
  
  
}


# Run the application
shinyApp(ui = ui, server = server)
