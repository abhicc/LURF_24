library(tidyverse)
library(shiny)

# Define UI
ui <- fluidPage(
  # Application title
  titlePanel("Model Bias and Variance"),
  
  fluidRow(
    column(2,
           selectInput(inputId = "dataset",
                       label = "Select a dataset",
                       choices = c("Data set 1", "Data set 2", "Data set 3"),
                       selected = "Data set 1")),
    column(2,
           selectInput(inputId = "model_name",
                       label = "Select model",
                       choices = c("Linear", "Non-Linear", "Polynomial"),
                       selected = "Linear")),
    column(2,
           sliderInput(inputId = "epsilon",
                       label = "Variability",
                       min = 0,
                       max = 8,
                       value = 1,
                       step = 0.5)),
    column(2,
           sliderInput(inputId = "num_ob",
                       label = "Number of observations",
                       min = 100,
                       max = 500,
                       value = 100,
                       step = 100)),
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
                         max = 4,
                         value = 2,
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

# Define server logic
server <- function(input, output) {
  a <- 3
  b <- 0.87
  c <- 0.5
  
  generate_data <- function(x, fx, epsilon, num_responses = 100) {
    n <- length(x)
    responses <- data.frame(matrix(ncol = num_responses, nrow = n))
    colnames(responses) <- paste0("response", 1:num_responses)
    
    for (i in 1:num_responses) {
      e <- rnorm(n, mean = 0, sd = epsilon)
      responses[, i] <- fx + e
    }
    
    data.frame(inp = x, true_form = fx, responses)
  }
  
  generate_test_data <- function(x, fx, epsilon) {
    n <- length(x)
    e <- rnorm(n, mean = 0, sd = epsilon)
    y <- fx + e
    data.frame(inp = x, true_form = fx, observed = y)
  }
  
  df <- reactive({
    toy_data <- NULL
    test_data <- NULL
    
    if (input$dataset == "Data set 1") {
      set.seed(2024)
      
      x <- runif(n = input$num_ob, min = 20, max = 40)
      fx <- a + (b * x)
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      
      x_test <- runif(n = 1000, min = 20, max = 40)
      fx_test <- a + (b * x_test)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    } else if (input$dataset == "Data set 2") {
      set.seed(2024)
      
      x <- runif(n = input$num_ob, min = 20, max = 40)
      fx <- a + (b * sqrt(x)) + (c * sin(x))
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      
      x_test <- runif(n = 1000, min = 20, max = 40)
      fx_test <- a + (b * sqrt(x_test)) + (c * sin(x_test))
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    } else if (input$dataset == "Data set 3") {
      set.seed(2024)
      
      x <- runif(n = input$num_ob, min = -5, max = 5)
      fx <- a + (b * x^2) + (c * x^3)
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      
      x_test <- runif(n = 1000, min = -5, max = 5)
      fx_test <- a + (b * x_test^2) + (c * x_test^3)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    }
    
    return(list(toy_data = toy_data, test_data = test_data))
  })
  
  output$Plot1 <- renderPlot({
    p <- ggplot(data = df()$toy_data, aes(x = inp, y = response1)) + 
      geom_point() +
      scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
      labs(title = "Scatter plot x vs. y", y = "y", x = "x")
    
    if (input$model_name == "Linear") {
      p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
    } else if (input$model_name == "Non-Linear") {
      p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
    } else if (input$model_name == "Polynomial") {
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
    }
    print(p)
  })
  
  output$Plot2 <- renderPlot({
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    if (input$model_name == "Linear") {
      complexities <- 1
    } else if (input$model_name == "Non-Linear") {
      complexities <- seq(0.1, 10, by = 1)
    } else if (input$model_name == "Polynomial") {
      complexities <- 1:10
    }
    
    for (complexity in complexities) {
      predictions <- matrix(nrow = 1000, ncol = 100)
      training_errors <- numeric(100)
      
      if (input$model_name == "Linear") {
        for (i in 1:100) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      } else if (input$model_name == "Non-Linear") {
        for (i in 1:100) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      } else if (input$model_name == "Polynomial") {
        for (i in 1:100) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      }
      
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      true_form <- df_testdata$true_form[1:1000]
      
      bias <- avg_predictions - true_form
      squared_bias <- bias^2
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8) +
      labs(title = "Bias", x = "Complexity", y = NULL) +
      theme_minimal() +
      scale_fill_manual(values = c("Bias" = "blue")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(plot_bias)
  })
  
  output$Plot3 <- renderPlot({
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    if (input$model_name == "Linear") {
      complexities <- 1
    } else if (input$model_name == "Non-Linear") {
      complexities <- seq(0.1, 10, by = 1)
    } else if (input$model_name == "Polynomial") {
      complexities <- 1:10
    }
    
    for (complexity in complexities) {
      predictions <- matrix(nrow = 1000, ncol = 100)
      training_errors <- numeric(100)
      
      if (input$model_name == "Linear") {
        for (i in 1:100) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      } else if (input$model_name == "Non-Linear") {
        for (i in 1:100) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      } else if (input$model_name == "Polynomial") {
        for (i in 1:100) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      }
      
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      true_form <- df_testdata$true_form[1:1000]
      
      bias <- avg_predictions - true_form
      squared_bias <- bias^2
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8) +
      labs(title = "Variance", x = "Complexity", y = NULL) +
      theme_minimal() +
      scale_fill_manual(values = c("Variance" = "red")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    print(plot_variance)
  })
  
  output$Plot4 <- renderPlot({
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    if (input$model_name == "Linear") {
      complexities <- 1
    } else if (input$model_name == "Non-Linear") {
      complexities <- seq(0.1, 10, by = 1)
    } else if (input$model_name == "Polynomial") {
      complexities <- 1:10
    }
    
    for (complexity in complexities) {
      predictions <- matrix(nrow = 1000, ncol = 100)
      training_errors <- numeric(100)
      
      if (input$model_name == "Linear") {
        for (i in 1:100) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      } else if (input$model_name == "Non-Linear") {
        for (i in 1:100) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      } else if (input$model_name == "Polynomial") {
        for (i in 1:100) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
          training_errors[i] <- mean((df_data[[paste0("response", i)]] - predict(model, newdata = data.frame(inp = df_data$inp)))^2)
        }
      }
      
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      true_form <- df_testdata$true_form[1:1000]
      
      bias <- avg_predictions - true_form
      squared_bias <- bias^2
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      test_error <- mean((df_testdata$observed - avg_predictions)^2)
      training_error <- mean(training_errors)
      
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Test Error", Value = test_error),
        data.frame(Complexity = complexity, Metric = "Training Error", Value = training_error)
      )
    }
    
    plot_mse <- ggplot(metrics, aes(x = Complexity, y = Value, color = Metric, group = Metric)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(title = "Training Error vs. Test Error", x = "Complexity", y = "Mean Squared Error") +
      theme_minimal() +
      scale_color_manual(values = c("Test Error" = "blue", "Training Error" = "green")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_mse)
  })
  
  output$myLegend <- renderPlot({
    par(mai = rep(0.01, 4))
    legend("center", legend = c("linear model", "non-linear model", "polynomial model"), lty = c(1, 1, 1), lwd = c(4, 4, 4), col = c("darkblue", "green", "red"))
  }, height = 70)
}

# Run the application
shinyApp(ui = ui, server = server)

