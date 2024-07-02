library(tidyverse)
library(shiny)

# Define UI
ui <- fluidPage(
  titlePanel("Model Bias and Variance"),
  fluidRow(
    column(2,
           selectInput(inputId = "dataset", label = "Select a dataset", choices = c("Data set 1", "Data set 2", "Data set 3"), selected = "Data set 1")),
    column(2,
           selectInput(inputId = "model_name", label = "Select model", choices = c("Linear", "Non-Linear", "Polynomial"), selected = "Polynomial")),
    column(2,
           sliderInput(inputId = "epsilon", label = "Variability", min = 0, max = 8, value = 1, step = 0.5)),
    column(2,
           sliderInput(inputId = "degree", label = "Polynomial degree", min = 1, max = 10, value = 1, step = 1)),
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

# Define server logic
server <- function(input, output) {
  a <- 3
  b <- 0.87
  c <- 0.5
  
  df <- reactive({
    set.seed(2024)
    x <- runif(n = 100, min = ifelse(input$dataset == "Data set 3", -5, 20), max = ifelse(input$dataset == "Data set 3", 5, 40))
    e <- replicate(10, rnorm(n = 100, mean = 0, sd = input$epsilon))
    
    if (input$dataset == "Data set 1") {
      fx <- a + b * x
    } else if (input$dataset == "Data set 2") {
      fx <- a + b * sqrt(x) + c * sin(x)
    } else {
      fx <- a + b * x^2 + c * x^3
    }
    
    y <- fx + e
    toy_data <- as.data.frame(cbind(x, fx, y))
    colnames(toy_data) <- c("inp", "true_form", paste0("response", 1:10))
    toy_data
  })
  
  plot_response <- function(i) {
    ggplot(data = df(), aes(x = inp, y = get(paste0("response", i)))) +
      geom_point() +
      scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
      labs(title = paste("Sample", i), y = "y", x = "x") +
      scale_y_continuous(limits = ifelse(input$dataset == "Data set 3", c(-30, 30), ifelse(input$dataset == "Data set 2", c(3, 13), c(15, 40)))) +
      scale_x_continuous(limits = ifelse(input$dataset == "Data set 3", c(-6, 6), c(20, 40))) +
      geom_smooth(method = ifelse(input$model_name == "Linear", "lm", "loess"), 
                  formula = ifelse(input$model_name == "Polynomial", y ~ poly(x, input$degree), y ~ x), 
                  se = FALSE, aes(color = paste0(input$model_name, " model")), 
                  show.legend = FALSE, span = ifelse(input$model_name == "Non-Linear", 1/input$flex, NULL))
  }
  
  output$Plot1 <- renderPlot({ plot_response(1) })
  output$Plot2 <- renderPlot({ plot_response(2) })
  output$Plot3 <- renderPlot({ plot_response(3) })
  
  output$Plot4 <- renderPlot({
    df_data <- df()
    predictions <- matrix(NA, nrow = 100, ncol = 10)
    
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
    } else {
      for (i in 1:10) {
        model <- lm(df_data[, i + 2] ~ poly(df_data$inp, input$degree), data = df_data)
        predictions[, i] <- predict(model, newdata = data.frame(df_data$inp))
      }
    }
    
    avg_predictions <- rowMeans(predictions)
    true_form <- df_data$true_form
    squared_bias <- (avg_predictions - true_form)^2
    overall_squared_bias <- mean(squared_bias)
    prediction_vars <- apply(predictions, 1, var)
    overall_variance <- mean(prediction_vars)
    
    metrics <- data.frame(
      Metric = c("Bias", "Variance"),
      Value = c(overall_squared_bias, overall_variance)
    )
    
    ggplot(metrics, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8) +
      labs(title = "Bias and Variance", x = "Metrics", y = "Value") +
      theme_minimal() +
      scale_y_continuous(limits = c(0, max(metrics$Value) * 1.2))
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
        predictions[, i] <- predict(model, newdata = data.frame(df_data$inp))
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
}

# Run the application
shinyApp(ui = ui, server = server)
