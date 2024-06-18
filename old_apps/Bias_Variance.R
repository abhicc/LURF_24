library(tidyverse)
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("Model Bias and Variance"),
  
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
      
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      fx <- a + (b * x)  # true function
      
      y1 <- fx + e1    # observed responses
      y2 <- fx + e2    # observed responses
      y3 <- fx + e3    # observed responses
      y4 <- fx + e4    # observed responses
      y5 <- fx + e5    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5)  
    }
    
    else 
    {
      set.seed(208)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      fx <- a + (b * sqrt(x)) + (c * sin(x))   # true function
      
      y1 <- fx + e1    # observed responses
      y2 <- fx + e2    # observed responses
      y3 <- fx + e3    # observed responses
      y4 <- fx + e4    # observed responses
      y5 <- fx + e5    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5)  
    }
    
    return(toy_data)
  })
  
  
  output$truePlot <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = true_form)) + 
        geom_point() + 
        labs(title = "True relationship without error", y = "f(x)", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
    else
    {
      ggplot(data = df(), aes(x = inp, y = true_form)) + 
        geom_point() + 
        labs(title = "True relationship without error", y = "f(x)", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    }

  })
  
  
  
  output$Plot1 <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = response1)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 1", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
    else
    {
      ggplot(data = df(), aes(x = inp, y = response1)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 1", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
  })
  


  output$Plot2 <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = response2)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 2", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
    else
    {
      ggplot(data = df(), aes(x = inp, y = response2)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 2", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
  })

  
  
  output$Plot3 <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = response3)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 3", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
    else
    {
      ggplot(data = df(), aes(x = inp, y = response3)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 3", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
  })

  
  
  output$Plot4 <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = response4)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 4", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
    else
    {
      ggplot(data = df(), aes(x = inp, y = response4)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 4", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
  })

  
  
  output$Plot5 <- renderPlot({
    
    if(input$true_model == "Linear")
    {
      ggplot(data = df(), aes(x = inp, y = response5)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*x), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 5", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
    else
    {
      ggplot(data = df(), aes(x = inp, y = response5)) + 
        geom_point() +
        geom_function(fun = function(x) a+(b*sqrt(x))+(c*sin(x)), aes(color = "true model"), linewidth = 1.5, show.legend = FALSE) +
        geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE) +
        # geom_smooth(formula = y ~ sqrt(x) + sin(x), se = FALSE, aes(color = "non-linear model")) +
        geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE) +
        scale_color_manual(values = c("true model" = "red", "linear model" = "blue", "non-linear model" = "green")) +
        # theme(legend.title = element_blank()) +
        labs(title = "Training Data 5", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    }
    
  })
  
  
  
  output$myLegend <- renderPlot({
    par(mai=rep(0.01,4))
    # plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,.1), ylim=c(0,.1))
    legend("center", legend=c("true model","linear model", "non-linear model"), lty=c(1,1,1), lwd=c(4,4,4), col=c("red", "darkblue", "green"))
  },height=50)
  

  
  
}


# Run the application
shinyApp(ui = ui, server = server)
