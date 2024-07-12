library(shiny)
library(ggplot2)

# Define UI for the app
ui <- fluidPage(
  titlePanel("Bias-Variance Tradeoff Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("model_type", "Select Model Type:", choices = list("Linear" = "linear", "Polynomial" = "polynomial")),
      conditionalPanel(
        condition = "input.model_type == 'polynomial'",
        numericInput("model_degree", "Select Polynomial Degree:", value = 2, min = 2, max = 5, step = 1)
      ),
      numericInput("epsilon", "Noise Level (epsilon):", value = 5, min = 0, step = 0.1),
      actionButton("generate", "Generate Data")
    ),
    
    mainPanel(
      plotOutput("dataPlot"),
      plotOutput("biasVariancePlot"),
      tableOutput("resultsTable")
    )
  )
)

library(shiny)
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(208)

# Function to generate a dataset
generate_dataset <- function(n, a, b, c, epsilon, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  x <- runif(n, min = -10, max = 10)
  e <- rnorm(n, mean = 0, sd = epsilon)
  y <- a + b * x + c * x^2 + e
  data.frame(x = x, y = y)
}

# Fit linear model
fit_linear_model <- function(data) {
  lm(y ~ x, data = data)
}

# Fit polynomial model
fit_polynomial_model <- function(data, degree) {
  lm(y ~ poly(x, degree, raw = TRUE), data = data)
}

# Get predictions from a list of models
get_predictions <- function(models, newdata) {
  sapply(models, predict, newdata = newdata)
}

# Calculate bias and variance
calculate_bias_variance <- function(predictions, true_y) {
  means <- rowMeans(predictions)
  bias <- mean((means - true_y)^2)
  variance <- mean(apply(predictions, 1, var))
  list(bias = bias, variance = variance)
}

# Define Server for the app
server <- function(input, output) {
  
  df <- reactive({
    input$generate
    
    isolate({
      a <- 3
      b <- 2
      c <- -0.5
      n <- 100
      epsilon <- input$epsilon
      num_datasets <- 100
      
      datasets <- lapply(1:num_datasets, function(i) generate_dataset(n, a, b, c, epsilon, seed = 208 + i))
      true_x <- runif(n = 100, min = 20, max = 40)
      true_y <- a + b * true_x + c * true_x^2
      
      if (input$model_type == "linear") {
        models <- lapply(datasets, fit_linear_model)
      } else {
        degree <- input$model_degree
        models <- lapply(datasets, fit_polynomial_model, degree = degree)
      }
      
      predictions <- get_predictions(models, newdata = data.frame(x = true_x))
      results <- calculate_bias_variance(predictions, true_y)
      
      list(models = models, predictions = predictions, true_x = true_x, true_y = true_y, datasets = datasets, results = results)
    })
  })
  
  output$dataPlot <- renderPlot({
    data <- df()
    
    ggplot(data$datasets[[1]], aes(x = x, y = y)) +
      geom_point(alpha = 0.5) +
      geom_line(aes(y = data$predictions[,1]), color = "red") +
      labs(title = "Scatterplot of the Model", x = "Input (x)", y = "Response (y)") +
      theme_minimal()
  })
  
  output$biasVariancePlot <- renderPlot({
    data <- df()
    results_df <- data.frame(Metric = c("Bias", "Variance"), Value = c(data$results$bias, data$results$variance))
    
    ggplot(results_df, aes(x = Metric, y = Value, fill = Metric)) +
      geom_bar(stat = "identity") +
      labs(title = "Bias and Variance", x = "Metric", y = "Value") +
      theme_minimal() +
      scale_fill_manual(values = c("Bias" = "red", "Variance" = "blue"))
  })
  
  output$resultsTable <- renderTable({
    data <- df()
    results_df <- data.frame(Degree = ifelse(input$model_type == "linear", 1, input$model_degree),
                             Bias = data$results$bias,
                             Variance = data$results$variance)
    results_df
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
