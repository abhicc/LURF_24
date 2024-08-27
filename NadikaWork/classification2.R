library(shiny)
library(ggplot2)
library(e1071)

# Example data generation function
generate_data <- function() {
  set.seed(123)
  
  # Generate training data
  x_train <- matrix(rnorm(200 * 2), ncol = 2)
  y_train <- ifelse(x_train[, 1] * x_train[, 2] > 0, 1, 0)
  
  # Generate test data
  x_test <- matrix(rnorm(100 * 2), ncol = 2)
  y_test <- ifelse(x_test[, 1] * x_test[, 2] > 0, 1, 0)
  
  return(list(
    training_data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train),
    test_data = data.frame(x1 = x_test[, 1], x2 = x_test[, 2], y_orig = y_test)
  ))
}

# Define the UI
ui <- fluidPage(
  titlePanel("SVM Decision Boundary with Margins"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("c_param", "Regularization Parameter (C)",
                  min = 0.01, max = 3, value = 1, step = 0.01)
    ),
    
    mainPanel(
      plotOutput("svm_plot")
    )
  )
)

# Define the server logic
server <- function(input, output) {
  
  output$svm_plot <- renderPlot({
    data_list <- generate_data()
    train_data <- data_list$training_data
    
    # Generate a grid of values for x1 and x2
    x1_range <- seq(min(train_data$x1) - 1, max(train_data$x1) + 1, length.out = 100)
    x2_range <- seq(min(train_data$x2) - 1, max(train_data$x2) + 1, length.out = 100)
    grid <- expand.grid(x1 = x1_range, x2 = x2_range)
    
    # Fit the SVM model with a linear kernel
    svm_model <- svm(y ~ x1 + x2, data = train_data, kernel = "linear", cost = input$c_param)
    
    # Predict on the grid
    grid$pred <- predict(svm_model, newdata = grid, decision.values = TRUE)
    decision_values <- attributes(predict(svm_model, grid, decision.values = TRUE))$decision.values
    
    # Convert predictions to class labels for plotting
    grid$prediction <- as.factor(ifelse(grid$pred > 0, 1, 0))
    
    # Extract support vectors
    support_vectors <- train_data[svm_model$index, ]
    
    # Plot the decision boundary with margins
    ggplot() +
      geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
      geom_point(data = train_data, aes(x = x1, y = x2, color = as.factor(y)), size = 2) +
      geom_point(data = support_vectors, aes(x = x1, y = x2), shape = 1, size = 4, color = "black") +
      geom_contour(data = grid, aes(x = x1, y = x2, z = decision_values), 
                   breaks = c(-1, 0, 1), linetype = "dashed", color = "black") +
      labs(title = "SVM Decision Boundary with Margins",
           x = "X1", y = "X2") +
      scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
      scale_color_manual(values = c("blue", "red"), name = "Actual") +
      theme_minimal() +  
      theme(legend.position = "top")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

