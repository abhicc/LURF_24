library(tidyverse)
library(shiny)

ui <- fluidPage(
  # Application title
  titlePanel("Model Bias and Variance"),
  
  fluidRow(
    column(2,
           selectInput(inputId = "dataset",
                       label = "Select a dataset",
                       choices = c("Data set 1", "Data set 2","Data set 3"),
                       selected = "Linear")),
    column(2,
           selectInput(inputId = "model_name",
                       label = "Select model",
                       choices = c("Linear", "Non-Linear","Polynomial"),
                       selected = "Linear")),
    column(2,
           sliderInput(inputId = "epsilon",
                       label = "Variability",
                       min = 0,
                       max = 8,
                       value = 1,
                       step = 0.5)),
    column(2,
           conditionalPanel(
             condition = "input.model_name == 'Non-Linear'",
             sliderInput(inputId = "flex",
                         label = "Flexibility",
                         min = 0.1,
                         max = 10,
                         value = 1,
                         step = 1)
           )),
    column(2,
           conditionalPanel(
             condition = "input.model_name == 'Polynomial'",
             sliderInput(inputId = "degree",
                         label = "Polynomial degree",
                         min = 1,
                         max = 10,
                         value = 1,
                         step = 1)
           )),
    column(2, plotOutput("myLegend"))
  ),
  
  fluidRow(
    column(4, plotOutput("Plot1")),
    column(4, plotOutput("Plot2")),
    column(4, plotOutput("Plot3")),
    column(4, plotOutput("Plot4"))
  ),
  
  fluidRow(
    column(12, plotOutput("BarGraph"))
  )
)

server <- function(input, output) {
  
  a <- 3
  b <- 0.87
  c <- 0.5
  
  df <- reactive({
    if(input$dataset == "Data set 1") {
      set.seed(2024)
      x <- runif(n = 100, min = 20, max = 40)
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e6 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e7 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e8 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e9 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e10 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      fx <- a + (b * x)
      y1 <- fx + e1
      y2 <- fx + e2
      y3 <- fx + e3
      y4 <- fx + e4
      y5 <- fx + e5
      y6 <- fx + e6
      y7 <- fx + e7
      y8 <- fx + e8
      y9 <- fx + e9
      y10 <- fx + e10
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5, response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10 = y10)
    } else if(input$dataset == "Data set 2") {
      set.seed(2024)
      x <- runif(n = 100, min = 20, max = 40)
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e6 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e7 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e8 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e9 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e10 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      fx <- a + (b * sqrt(x)) + (c * sin(x))
      y1 <- fx + e1
      y2 <- fx + e2
      y3 <- fx + e3
      y4 <- fx + e4
      y5 <- fx + e5
      y6 <- fx + e6
      y7 <- fx + e7
      y8 <- fx + e8
      y9 <- fx + e9
      y10 <- fx + e10
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5, response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10 = y10)
    } else if(input$dataset == "Data set 3") {
      set.seed(2024)
      x <- runif(n = 100, min = -5, max = 5)
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e6 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e7 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e8 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e9 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      e10 <- rnorm(n = 100, mean = 0, sd = input$epsilon)
      fx <- a + (b * x^2) + (c * x^3)
      y1 <- fx + e1
      y2 <- fx + e2
      y3 <- fx + e3
      y4 <- fx + e4
      y5 <- fx + e5
      y6 <- fx + e6
      y7 <- fx + e7
      y8 <- fx + e8
      y9 <- fx + e9
      y10 <- fx + e10
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5, response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10 = y10)
    }
    return(toy_data)
  })
  
  output$Plot1 <- renderPlot({
    if (input$dataset == "Data set 1") {
      p <- ggplot(data = df(), aes(x = inp, y = response1)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 1", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
      
    
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
  
  else if(input$dataset == "Data set 2") {
    p <- ggplot(data = df(), aes(x = inp, y = response1)) + 
      geom_point() +
      scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
      labs(title = "Sample 1", y = "y", x = "x") +
      scale_y_continuous(limits = c(3, 13)) +
      scale_x_continuous(limits = c(20, 40))
    
    if (input$model_name == "Linear") {
      p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
    } else if (input$model_name == "Non-Linear") {
      p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
    } else if (input$model_name == "Polynomial") {
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
    }
    print(p)
  }
  else if(input$dataset == "Data set 3") 
    
  {
    p <- ggplot(data = df(), aes(x = inp, y = response1)) + 
      geom_point() +
      scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
      labs(title = "Sample 1", y = "y", x = "x") +
      scale_y_continuous(limits = c(-30, 30)) +
      scale_x_continuous(limits = c(-6, 6))
    
    if (input$model_name == "Linear") {
      p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
    } else if (input$model_name == "Non-Linear") {
      p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
    } else if (input$model_name == "Polynomial") {
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
    }
    print(p)
  }
  
})
  
  
  output$Plot2 <- renderPlot({
    
    if (input$dataset == "Data set 1") {
      p <- ggplot(data = df(), aes(x = inp, y = response2)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 2", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
    
    else if(input$dataset == "Data set 2") {
      p <- ggplot(data = df(), aes(x = inp, y = response2)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 2", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
    
    
    else if(input$dataset == "Data set 3") 
      
    {
      p <- ggplot(data = df(), aes(x = inp, y = response2)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 2", y = "y", x = "x") +
        scale_y_continuous(limits = c(-30, 30)) +
        scale_x_continuous(limits = c(-6, 6))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
    
  })
  
  
  
  output$Plot3 <- renderPlot({
    
    if (input$dataset == "Data set 1") {
      p <- ggplot(data = df(), aes(x = inp, y = response3)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 3", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
    
    else if(input$dataset == "Data set 2") {
      p <- ggplot(data = df(), aes(x = inp, y = response3)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 3", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
    
    
    else if(input$dataset == "Data set 3") 
      
    {
      p <- ggplot(data = df(), aes(x = inp, y = response3)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 3", y = "y", x = "x") +
        scale_y_continuous(limits = c(-30, 30)) +
        scale_x_continuous(limits = c(-6, 6))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
    
  })
  
  output$Plot4 <- renderPlot({
    # Get the current value of reactive data frame df
    df_data <- df()
    
    # Initialize a matrix to store predictions
    predictions <- matrix(NA, nrow = 100, ncol = 10)
    
    # Fit models and obtain predictions using a for loop
    if (input$model_name == "Linear") {
      for (i in 1:10) {
        model <- lm(df_data[, i + 2] ~ df_data$inp, data = df_data)
        predictions[, i] <- predict(model, newdata = data.frame(df_data$inp))
      }
    } else if (input$model_name == "Non-Linear") {
      for (i in 1:10) {
        model <- loess(df_data[, i + 2] ~ df_data$inp, span = 1/input$flex, data = df_data)
        predictions[, i] <- predict(model, newdata = data.frame(df_data$inp))
      }
    } else if (input$model_name == "Polynomial") {
      for (i in 1:10) {
        model <- lm(df_data[, i + 2] ~ poly(df_data$inp, input$degree), data = df_data)
        predictions[, i] <- predict(model, newdata = data.frame(df_data$inp))
      }
    }
  })
    
  
  output$myLegend <- renderPlot({
    par(mai = rep(0.01, 4))
    legend("center", legend = c("linear model", "non-linear model", "polynomial model"), 
           lty = c(1, 1, 1), lwd = c(4, 4, 4), col = c("darkblue", "green", "red"))
  }, height = 70)
  
  output$BarGraph <- renderPlot({
    df_data <- df()
    degrees <- 1:10
    bias <- numeric(length(degrees))
    variance <- numeric(length(degrees))
    
    for (degree in degrees) {
      predictions <- matrix(NA, nrow = 100, ncol = 10)
      for (i in 1:10) {
        model <- lm(df_data[, i + 2] ~ poly(df_data$inp, degree), data = df_data)
        predictions[, i] <- predict(model, newdata = data.frame(x = df_data$inp))
      }
      avg_predictions <- rowMeans(predictions)
      squared_bias <- (avg_predictions - df_data$true_form)^2
      bias[degree] <- mean(squared_bias)
      variance[degree] <- mean(apply(predictions, 1, var))
    }
    
    bias_variance <- data.frame(
      Degree = rep(degrees, 2),
      Value = c(bias, variance),
      Metric = rep(c("Bias", "Variance"), each = length(degrees))
    )
    
    ggplot(bias_variance, aes(x = factor(Degree), y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge", alpha = 0.8) +
      scale_fill_manual(values = c("Bias" = "skyblue", "Variance" = "lightpink")) +
      labs(title = "Bias and Variance by Polynomial Degree", x = "Polynomial Degree", y = "Value") +
      theme_minimal()
  })

# Run the application
shinyApp(ui = ui, server = server) 






