library(shiny)
library(ggplot2)
library(nnet)
library(mvtnorm)

# Define UI
ui <- fluidPage(
  titlePanel("Neural Network Model Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      sliderInput("num_ob", "Number of observations",
                  min = 100, max = 200, value = 100, step = 50),
      
      sliderInput("epsilon", "Variability",
                  min = 0.2, max = 0.8, value = 0.4, step = 0.2),
      
      sliderInput("node", "Number of Nodes in Hidden Layer", 
                  min = 1, max = 10, value = 5, step = 1)
    ),
    
    mainPanel(
      fluidRow(
        column(7, plotOutput("plot1")),
        column(5, plotOutput("plot2"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data generation
  df <- reactive({
    set.seed(123)  # Set a fixed seed for reproducibility
    
    num_training_points <-  input$num_ob
    num_test_points <- 1000
    noise <- input$epsilon
    num_rep_sets <- 50
    
    # Generating bivariate normal meta-means and 10 component means for each class
    mu_class0 <- c(0.6, -0.6)
    mu_class1 <- c(-0.6, 0.6)
    cov_mat <- matrix(c(1,0,0,1), nrow=2, ncol=2)
    means_class0 <- rmvnorm(n = 10, mean = mu_class0, sigma = cov_mat)
    means_class1 <- rmvnorm(n = 10, mean = mu_class1, sigma = cov_mat)
    
    # Generating test data
    test_obs_class0 <- matrix(nrow=num_test_points/2, ncol=2)
    test_obs_class1 <- matrix(nrow=num_test_points/2, ncol=2)
    for(i in 1:(num_test_points/2)) {
      s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      test_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                     sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
      
      s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      test_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                     sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
    }
    test_data <- data.frame(rbind(test_obs_class0, test_obs_class1), 
                            y_orig = factor(rep(c("0", "1"), each=num_test_points/2), 
                                            levels = c("0", "1")))
    names(test_data) <- c("x1", "x2", "y_orig")
    set.seed(321)  # Set a fixed seed for reproducibility
    
    # Generating replicated training datasets
    x1_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
    x2_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
    y_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
    
    for(j in 1:num_rep_sets) {
      rep_training_obs_class0 <- matrix(nrow=num_training_points/2, ncol=2)
      rep_training_obs_class1 <- matrix(nrow=num_training_points/2, ncol=2)
      for(i in 1:(num_training_points/2)) {
        s <- sample(1:10, size = 1, prob = rep(1/10, 10))
        rep_training_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                               sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
        
        s <- sample(1:10, size = 1, prob = rep(1/10, 10))
        rep_training_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                               sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
      }
      x1_matrix[, j] <- c(rep_training_obs_class0[,1], rep_training_obs_class1[,1])
      x2_matrix[, j] <- c(rep_training_obs_class0[,2], rep_training_obs_class1[,2])
      y_matrix[, j] <- rep(c("0", "1"), each=num_training_points/2)
    }
    
    list(
      training_data = list(x1 = x1_matrix, x2 = x2_matrix, y = y_matrix),
      test_data = test_data
    )
  })
  
  output$plot1 <- renderPlot({
    data_list <- df()
    train_data <- data_list$training_data
    
    # Selecting the first replicated training set for simplicity
    x_train <- data.frame(x1 = train_data$x1[,1], x2 = train_data$x2[,1], y = train_data$y[,1])
    x_train$y <- as.numeric(as.character(x_train$y))  # Ensure y is numeric (0 and 1)
    
    # Generate a grid of values for x1 and x2
    x1_range <- seq(min(x_train$x1) - 1, max(x_train$x1) + 1, length.out = 100)
    x2_range <- seq(min(x_train$x2) - 1, max(x_train$x2) + 1, length.out = 100)
    grid <- expand.grid(x1 = x1_range, x2 = x2_range)
    
    model <- nnet(y ~ x1 + x2, data = x_train, size = input$node, 
                  linout = FALSE, decay = 0.01, maxit = 1000)
    
    # Predict on the grid
    grid$prob <- predict(model, newdata = grid)
    
    # Convert predictions to class labels for plotting
    grid$prediction <- as.factor(ifelse(grid$prob > 0.5, "1", "0"))
    
    # Plot
    ggplot() +
      geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
      geom_point(data = x_train, aes(x = x1, y = x2, color = as.factor(y)), size = 2) +
      labs(title = "Neural Network Decision Boundary",
           x = "X1", y = "X2") +
      scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
      scale_color_manual(values = c("blue", "red"), name = "Actual") +
      theme_minimal() +  
      theme(legend.position = "top")
  })
  
  output$plot2 <- renderPlot({
    data_list <- df()
    test_data <- data_list$test_data
    x_test <- test_data[, c("x1", "x2")]
    y_test <- test_data$y_orig
    
    # Range of number of nodes
    node_values <- seq(1, 10, by = 1)
    avg_test_errors <- numeric(length(node_values))
    avg_train_errors <- numeric(length(node_values))
    
    # Loop through each number of nodes
    for (nodes in node_values) {
      test_errors <- numeric(ncol(data_list$training_data$x1))
      train_errors <- numeric(ncol(data_list$training_data$x1))
      
      # Loop through each replicated training set
      for (i in 1:ncol(data_list$training_data$x1)) {
        # Extract training data for the current replication
        x_train <- data.frame(x1 = data_list$training_data$x1[, i], x2 = data_list$training_data$x2[, i])
        y_train <- factor(data_list$training_data$y[, i])  # Ensure y_train is a factor
        
        # Fit the neural network model using the 'nnet' package
        model <- nnet(y_train ~ x1 + x2, data = x_train, size = nodes, 
                      linout = FALSE, decay = 0.01, maxit = 1000)
        
        # Predict on test data
        grid <- data.frame(x1 = x_test[, "x1"], x2 = x_test[, "x2"])
        test_pred_prob <- predict(model, newdata = grid, type = "raw")
        test_pred <- ifelse(test_pred_prob > 0.5, "1", "0")
        test_errors[i] <- mean(test_pred != y_test, na.rm = TRUE)
        
        # Predict on training data
        train_pred_prob <- predict(model, newdata = x_train, type = "raw")
        train_pred <- ifelse(train_pred_prob > 0.5, "1", "0")
        train_errors[i] <- mean(train_pred != y_train, na.rm = TRUE)
      }
      
      # Compute average errors for current number of nodes
      avg_test_errors[which(node_values == nodes)] <- mean(test_errors, na.rm = TRUE)
      avg_train_errors[which(node_values == nodes)] <- mean(train_errors, na.rm = TRUE)
    }
    
    # Create data frame for plotting
    error_df <- data.frame(
      nodes = rep(node_values, 2),
      avg_error = c(avg_test_errors, avg_train_errors),
      error_type = rep(c("Test Error", "Training Error"), each = length(node_values))
    )
    
    # Plot average error vs number of nodes using a line graph
    ggplot(error_df, aes(x = nodes, y = avg_error, color = error_type)) +
      geom_line(size = 1) +  # Line graph for both error types
      geom_point(size = 2) +  # Points for both error types
      labs(title = "Test and Training Error vs Number of Nodes",
           x = "Number of Nodes",
           y = "Error Rate") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "red")) +
      theme(legend.position = "top", legend.title = element_blank())  # Remove the legend title
  })
  

  



  
}

# Run the application
shinyApp(ui = ui, server = server)
