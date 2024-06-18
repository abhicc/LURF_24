library(tidyverse)
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Model Flexibility"),

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
  
  
  a <- 3
  b <- 0.87
  c <- 0.5
  
  df <- reactive({
    
    if(input$true_model == "Linear")
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
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

      fx <- a + (b * sqrt(x)) + (c * sin(x))   # true function
      
      y <- fx + e    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response = y)  
    }
    
    return(toy_data)
  })
  
  
  output$truePlot <- renderPlot({
    
    ggplot(data = df(), aes(x = inp, y = true_form)) + 
      geom_point() + 
      labs(title = "True relationship without error", y = "f(x)", x = "x") +
      ylim(c(min(df()$response), max(df()$response)))
    
  })
  
  
  
  output$observedPlot <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = response)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model")) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model")) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        theme(legend.title = element_blank()) +
        labs(title = "Comparing two models", y = "y", x = "x")
    }
    
    else
    {
      ggplot(data = df(), aes(x = inp, y = response)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model")) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model")) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        theme(legend.title = element_blank()) +
        labs(title = "Comparing two models", y = "y", x = "x")
    }
    
    
  })
  
  
  

}


# Run the application
shinyApp(ui = ui, server = server)
