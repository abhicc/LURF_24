library(shiny)
library(ggplot2)
library(mvtnorm)
library(rpart)
library(rpart.plot)

# Define UI
ui <- fluidPage(
  titlePanel("BIAS-VARIANCE TRADEOFF VISUALIZATION"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      # Common sliders for all tabs
      sliderInput("num_ob", "Number of observations",
                  min = 100, max = 200, value = 100, step = 50),
      # Common sliders for all tabs
      sliderInput("epsilon", "variability",
                  min = 0.2, max = 0.8, value = 0.4, step = 0.2),
      
      # Dynamic slider for epsilon
      uiOutput("epsilon_slider")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "tabs",
        tabPanel("KNN",
                 sliderInput("k_value", "K-value", 
                             min = 1, max = 15, value = 5, step = 1),
                 fluidRow(
                   column(5, plotOutput("tab1_plot1")),
                   column(5, plotOutput("tab1_plot2"))
                 )
        ),
        tabPanel("Decision Tree",
                 sliderInput("depth", "Tree-depth", 
                             min = 2, max = 6, value = 3, step = 1),
                 fluidRow(
                   column(5, plotOutput("tab2_plot1")),
                   column(5, plotOutput("tab2_plot3"))
                 ),
                 fluidRow(
                   column(10, plotOutput("tab2_plot2"))
                 )
        )
        
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  

  
  # Reactive data generation
  df <- reactive({
    set.seed(123)  # Set a fixed seed for reproducibility
    
    num_training_points <- input$num_ob
    num_test_points <- 500
    noise <- input$epsilon
    num_rep_sets <- 50
    
    # Generating bivariate normal meta-means and 10 component means for each class
    mu_class0 <- c(1, -1)
    mu_class1 <- c(-1, 1)
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
  
 
  
  output$tab1_plot2 <- renderPlot({
    data_list <- df()
    test_data <- data_list$test_data
    x_test <- test_data[, 1:2]
    y_test <- test_data$y_orig
    
    # Range of k values from 5 to 33 with an increment of 4
    k_values <- seq(5, 33, by = 4)
    avg_test_errors <- numeric(length(k_values))
    avg_train_errors <- numeric(length(k_values))
    
    # Loop through each k value
    for (k in k_values) {
      test_accuracies <- numeric(ncol(data_list$training_data$x1))
      train_accuracies <- numeric(ncol(data_list$training_data$x1))
      
      # Loop through each replicated training set
      for (i in 1:ncol(data_list$training_data$x1)) {
        x_train <- cbind(data_list$training_data$x1[, i], data_list$training_data$x2[, i])
        y_train <- data_list$training_data$y[, i]
        
        # Fit KNN model and predict on test data
        knn_test_pred <- knn(train = x_train, test = x_test, cl = y_train, k = k)
        knn_train_pred <- knn(train = x_train, test = x_train, cl = y_train, k = k)
        
        # Calculate test and training accuracies
        test_accuracies[i] <- mean(knn_test_pred == y_test)
        train_accuracies[i] <- mean(knn_train_pred == y_train)
      }
      
      # Compute average accuracy and error rate for current k value
      avg_test_accuracy <- mean(test_accuracies)
      avg_train_accuracy <- mean(train_accuracies)
      avg_test_error <- 1 - avg_test_accuracy
      avg_train_error <- 1 - avg_train_accuracy
      
      avg_test_errors[which(k_values == k)] <- avg_test_error
      avg_train_errors[which(k_values == k)] <- avg_train_error
    }
    
    # Create data frame for plotting
    error_df <- data.frame(
      k_value = rep(k_values, 2),
      avg_error = c(avg_test_errors, avg_train_errors),
      error_type = rep(c("Test Error", "Training Error"), each = length(k_values))
    )
    
    # Plot average error vs k value using a line graph
    ggplot(error_df, aes(x = k_value, y = avg_error, color = error_type)) +
      geom_line(size = 1) +  # Line graph for both error types
      geom_point(size = 2) +  # Points for both error types
      labs(title = "Test and Training Error vs K-value",
           x = "K-value",
           y = "Error Rate") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "red"))
  })

  
  
  
  output$tab1_plot1 <- renderPlot({
    data_list <- df()
    test_data <- data_list$test_data
    x_test <- test_data[, 1:2]
    y_test <- test_data$y_orig
    
    # Fixed K value
    k_fixed <- input$k_value
    
    # Get the first replicated training set
    x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
    y_train <- data_list$training_data$y[, 1]
    
    # Generate a grid of values for x1 and x2
    x1_range <- seq(min(c(x_train[, 1], x_test[, 1])) - 1, max(c(x_train[, 1], x_test[, 1])) + 1, length.out = 100)
    x2_range <- seq(min(c(x_train[, 2], x_test[, 2])) - 1, max(c(x_train[, 2], x_test[, 2])) + 1, length.out = 100)
    grid <- expand.grid(x1 = x1_range, x2 = x2_range)
    
    # Predict on the grid
    knn_pred_grid <- knn(train = x_train, test = grid, cl = y_train, k = k_fixed)
    
    # Convert grid predictions to a matrix for plotting
    grid$prediction <- as.factor(knn_pred_grid)
    
    # Plot
    ggplot() +
      geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
      geom_point(data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y_orig = y_train),
                 aes(x = x1, y = x2, color = y_orig), size = 2) +
      labs(title = paste("Decision Boundary with K =", k_fixed),
           x = "X1", y = "X2") +
      scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
      scale_color_manual(values = c("blue", "red"), name = "Actual") +
      theme_minimal() +  
      theme(legend.position = "top")
  })
  
  
  #############
  output$tab2_plot1 <- renderPlot({
    data_list <- df()
    test_data <- data_list$test_data
    x_test <- test_data[, 1:2]
    y_test <- test_data$y_orig
    
    # Get the first replicated training set
    x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
    y_train <- data_list$training_data$y[, 1]
    
    # Generate a grid of values for x1 and x2
    x1_range <- seq(min(c(x_train[, 1], x_test[, 1])) - 1, max(c(x_train[, 1], x_test[, 1])) + 1, length.out = 100)
    x2_range <- seq(min(c(x_train[, 2], x_test[, 2])) - 1, max(c(x_train[, 2], x_test[, 2])) + 1, length.out = 100)
    grid <- expand.grid(x1 = x1_range, x2 = x2_range)
    
    # Create a data frame for the current training set
    train_data <- data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train)
    
    # Fit the decision tree model with a fixed depth
    tree_model <- rpart(
      y ~ x1 + x2,
      data = train_data,
      control = rpart.control(maxdepth = input$depth, minbucket = 1, cp = 0, xval = 0)
    )
    
    # Predict on the grid
    tree_pred_grid <- predict(tree_model, newdata = grid, type = "class")
    
    # Convert grid predictions to a factor for plotting
    grid$prediction <- as.factor(tree_pred_grid)
    
    # Plot
    ggplot() +
      geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
      geom_point(data = data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y_orig = y_train),
                 aes(x = x1, y = x2, color = y_orig), size = 2) +
      labs(title = paste("Decision Boundary with Tree Depth =", input$depth),
           x = "X1", y = "X2") +
      scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
      scale_color_manual(values = c("blue", "red"), name = "Actual") +
      theme_minimal() +  
      theme(legend.position = "top")
  })
  
  
  
  output$tab2_plot2 <- renderPlot({
    
    
    # Get the reactive data frame
    data_list <- df()
    test_data <- data_list$test_data
    x_test <- test_data[, 1:2]
    y_test <- test_data$y_orig
    
    # Get depth from input
    depth <- input$depth
    minbucket_value <- 1  # A small number for minbucket
    
    # Get the first replicated training set
    x_train <- cbind(data_list$training_data$x1[, 1], data_list$training_data$x2[, 1])
    y_train <- data_list$training_data$y[, 1]
    
    # Create a data frame for training
    train_data <- data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train)
    
    # Fit a decision tree model with the specified depth
    tree_model <- rpart(
      formula = y ~ x1 + x2,       # Target variable y and predictors x1, x2
      data = train_data,
      control = rpart.control(maxdepth = depth, minbucket = minbucket_value, cp = 0, xval = 0), # No pruning, no cross-validation
      method = "class"             # For classification
    )
    
    # Plot the decision tree
    rpart.plot(tree_model, main = paste("Decision Tree with Depth =", depth))
  })
  
  
output$tab2_plot3 <- renderPlot({
  
    data_list <- df()
    test_data <- data_list$test_data
    x_test <- test_data[, 1:2]
    y_test <- test_data$y_orig
    
    # Range of depth values (you can adjust this range as needed)
    depth_values <- seq(2, 6, by = 1)
    avg_test_errors <- numeric(length(depth_values))
    avg_train_errors <- numeric(length(depth_values))
    
    # Loop through each depth value
    for (depth in depth_values) {
      test_accuracies <- numeric(ncol(data_list$training_data$x1))
      train_accuracies <- numeric(ncol(data_list$training_data$x1))
      
      # Loop through each replicated training set
      for (i in 1:ncol(data_list$training_data$x1)) {
        x_train <- cbind(data_list$training_data$x1[, i], data_list$training_data$x2[, i])
        y_train <- data_list$training_data$y[, i]
        
        # Create a data frame for the current training set
        train_data <- data.frame(x1 = x_train[, 1], x2 = x_train[, 2], y = y_train)
        
        # Fit decision tree model with controlled depth
        tree_model <- rpart(
          y ~ x1 + x2,
          data = train_data,
          control = rpart.control(maxdepth = depth, minbucket = 1, cp = 0, xval = 0)
        )
        
        # Predict on test data
        tree_test_pred <- predict(tree_model, newdata = data.frame(x1 = x_test[, 1], x2 = x_test[, 2]), type = "class")
        test_accuracies[i] <- mean(tree_test_pred == y_test)
        
        # Predict on training data
        tree_train_pred <- predict(tree_model, newdata = train_data, type = "class")
        train_accuracies[i] <- mean(tree_train_pred == y_train)
      }
      
      # Compute average accuracy and error rate for current depth value
      avg_test_accuracy <- mean(test_accuracies)
      avg_train_accuracy <- mean(train_accuracies)
      avg_test_error <- 1 - avg_test_accuracy
      avg_train_error <- 1 - avg_train_accuracy
      
      avg_test_errors[which(depth_values == depth)] <- avg_test_error
      avg_train_errors[which(depth_values == depth)] <- avg_train_error
    }
    
    # Create data frame for plotting
    error_df <- data.frame(
      depth = rep(depth_values, 2),
      avg_error = c(avg_test_errors, avg_train_errors),
      error_type = rep(c("Test Error", "Training Error"), each = length(depth_values))
    )
    
    # Plot average error vs depth using a line graph
    ggplot(error_df, aes(x = depth, y = avg_error, color = error_type)) +
      geom_line(size = 1) +  # Line graph for both error types
      geom_point(size = 2) +  # Points for both error types
      labs(title = "Test and Training Error vs Tree Depth",
           x = "Tree Depth",
           y = "Error Rate") +
      theme_minimal() +
      scale_color_manual(values = c("blue", "red"))
  })

  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
