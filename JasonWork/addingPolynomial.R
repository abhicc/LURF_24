library(tidyverse)
library(shiny)
#let users choose the model



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
  )
)


server <- function(input, output) {
  
  
  a <- 3
  b <- 0.87
  c <- 0.5
  
  df <- reactive({
    
    if(input$dataset == "Data set 1")
    {
      set.seed(2024) #change to 2024
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e6 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e7 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e8 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e9 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e10 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      fx <- a + (b * x)  # true function
      
      y1 <- fx + e1    # observed responses
      y2 <- fx + e2    # observed responses
      y3 <- fx + e3    # observed responses
      y4 <- fx + e4    # observed responses
      y5 <- fx + e5    # observed responses
      y6 <- fx + e6    # observed responses
      y7 <- fx + e7    # observed responses
      y8 <- fx + e8    # observed responses
      y9 <- fx + e9    # observed responses
      y10 <- fx + e10    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5,
                             response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10=y10)  
    }
    
    else if(input$dataset == "Data set 2") 
    {
      set.seed(2024)
      
      # simulate data
      x <- runif(n = 100, min = 20, max = 40)   # input/predictor
      
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e6 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e7 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e8 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e9 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e10 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      fx <- a + (b * sqrt(x)) + (c * sin(x))   # true function
      
      y1 <- fx + e1    # observed responses
      y2 <- fx + e2    # observed responses
      y3 <- fx + e3    # observed responses
      y4 <- fx + e4    # observed responses
      y5 <- fx + e5    # observed responses
      y6 <- fx + e6    # observed responses
      y7 <- fx + e7    # observed responses
      y8 <- fx + e8    # observed responses
      y9 <- fx + e9    # observed responses
      y10 <- fx + e10    # observed responses
      
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5,
                             response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10=y10)  
    } else if(input$dataset == "Data set 3"){
      
      set.seed(2024)
      
      # simulate data
      x <- runif(n = 100, min = -5, max = 5)   # input/predictor
      
      e1 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e2 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e3 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e4 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e5 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e6 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e7 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e8 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e9 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      e10 <- rnorm(n = 100, mean = 0, sd = input$epsilon)  # error
      
      fx <- a + (b * x^2) + (c * x^3)  # polynomial true function
      
      y1 <- fx + e1    # observed responses
      y2 <- fx + e2    # observed responses
      y3 <- fx + e3    # observed responses
      y4 <- fx + e4    # observed responses
      y5 <- fx + e5    # observed responses
      y6 <- fx + e6    # observed responses
      y7 <- fx + e7    # observed responses
      y8 <- fx + e8    # observed responses
      y9 <- fx + e9    # observed responses
      y10 <- fx + e10    # observed responses
      toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5,
                             response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10=y10)  
      
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
    
    # Initialize a data frame to store bias and variance for different complexities
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define the range of complexities
    if (input$model_name == "Linear") {
      complexities <- 1  # Linear model has no complexity parameter
    } else if (input$model_name == "Non-Linear") {
      complexities <- seq(0.1, 10, by = 0.9)
    } else if (input$model_name == "Polynomial") {
      complexities <- 1:10
    }
    
    # Loop over each complexity level
    for (complexity in complexities) {
      predictions <- matrix(NA, nrow = 100, ncol = 10)
      
      # Fit models and obtain predictions
      if (input$model_name == "Linear") {
        for (i in 1:10) {
          model <- lm(df_data[, i + 2] ~ df_data$inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_data$inp[1:100]))
        }
      } else if (input$model_name == "Non-Linear") {
        for (i in 1:10) {
          model <- loess(df_data[, i + 2] ~ df_data$inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_data$inp[1:100]))
        }
      } else if (input$model_name == "Polynomial") {
        for (i in 1:10) {
          model <- lm(df_data[, i + 2] ~ poly(df_data$inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_data$inp[1:100]))
        }
      }
      
      # Calculate average predictions
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      
      # Calculate true form based on your data (replace with appropriate value)
      true_form <- df_data$true_form[1:100]
      
      # Calculate bias
      bias <- avg_predictions - true_form
      
      # Calculate squared bias
      squared_bias <- bias^2
      
      # Calculate overall squared bias
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      # Calculate variances of predictions
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      
      # Calculate overall variance
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      # Store metrics for current complexity level
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    # Plotting code with complexity on the x-axis
    ggplot(metrics, aes(x = as.factor(Complexity), y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8) +
      labs(
        title = "Bias and Variance Across Different Complexity Levels",
        x = "Complexity",
        y = "Value"
      ) +
      theme_minimal() +
      scale_y_continuous(limits = c(0, max(metrics$Value, na.rm = TRUE) * 1.2)) +
      scale_fill_manual(values = c("Bias" = "blue", "Variance" = "red"))
  })
  

  
 
      
      
      
  
      
      
      
      
      
      
      
  
  
  output$myLegend <- renderPlot({
    par(mai=rep(0.01,4))
    # plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=c(0,.1), ylim=c(0,.1))
    legend("center", legend=c("linear model", "non-linear model","polynomial model"), lty=c(1,1,1), lwd=c(4,4,4), col=c("darkblue", "green","red"))
  },height=70)
  
  
  
  
}




# Run the application
shinyApp(ui = ui, server = server)
