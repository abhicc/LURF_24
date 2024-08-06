library(shiny)
library(ggplot2)
library(mvtnorm)

# Define UI
ui <- fluidPage(
  titlePanel("BIAS-VARIANCE TRADEOFF VISUALIZATION"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,  # Set the width of the sidebar
      # Drop-down menu for selecting datasets
      selectInput("dataset", "Dataset",
                  choices = c("Data set 1"),
                  selected = "Data set 1"),
      
      # Common sliders for all tabs
      sliderInput("num_ob", "Number of observations",
                  min = 100, max = 200, value = 100, step = 50),
      
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
                   column(4, plotOutput("tab1_plot1")),
                   column(4, plotOutput("tab1_plot2")),
                   column(4, plotOutput("tab1_plot3"))
                 )
        )
      )
    )
  )
)


# Define server logic
server <- function(input, output, session) {
  
  # Dynamically render the epsilon slider based on the selected dataset
  output$epsilon_slider <- renderUI({
    if (input$dataset == "Data set 1") {
      sliderInput("epsilon", "Variability", 
                  min = 0, max = 1, value = 0.5, step = 0.1)
    } else {
      sliderInput("epsilon", "Variability", 
                  min = 0, max = 0.6, value = 0.2, step = 0.1)
    }
  })
  
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
  
  # Plot the first replicated training dataset
  output$tab1_plot1 <- renderPlot({
    data_list <- df()
    x1 <- data_list$training_data$x1[,1]
    x2 <- data_list$training_data$x2[,1]
    y <- data_list$training_data$y[,1]
    
    df_plot <- data.frame(x1 = x1, x2 = x2, y_orig = y)
    
    ggplot(data = df_plot, aes(x = x1, y = x2, color = y_orig)) +
      geom_point() +
      labs(title = "Training Dataset", x = "X1", y = "X2", color = "Y") +
      scale_color_manual(values = c("0" = "blue", "1" = "red"), labels = c("0", "1")) +
      theme_minimal() +
      xlim(-5, 5) +
      ylim(-5, 5)
  })
  
  output$tab1_plot2 <- renderPlot({
    data_list <- df()
    test_data <- data_list$test_data
    x_test <- test_data[, 1:2]
    y_test <- test_data$y_orig
    
    # Range of k values from 5 to 26 with an increment of 3
    k_values <- seq(5, 33, by = 4)
    avg_errors <- numeric(length(k_values))
    
    # Loop through each k value
    for (k in k_values) {
      accuracies <- numeric(ncol(data_list$training_data$x1))
      
      # Loop through each replicated training set
      for (i in 1:ncol(data_list$training_data$x1)) {
        x_train <- cbind(data_list$training_data$x1[, i], data_list$training_data$x2[, i])
        y_train <- data_list$training_data$y[, i]
        
        # Fit KNN model and predict on test data
        knn_pred <- knn(train = x_train, test = x_test, cl = y_train, k = k)
        accuracies[i] <- mean(knn_pred == y_test)
      }
      
      # Compute average accuracy and error rate for current k value
      avg_accuracy <- mean(accuracies)
      avg_error <- 1 - avg_accuracy
      avg_errors[which(k_values == k)] <- avg_error
    }
    
    # Create data frame for plotting
    error_df <- data.frame(k_value = k_values, avg_error = avg_errors)
    
    # Plot average error vs k value using a line graph
    ggplot(error_df, aes(x = k_value, y = avg_error)) +
      geom_line(color = "blue", size = 1) +  # Line graph
      geom_point(color = "red", size = 2) +  
      labs(title = "Average Error vs K-value",
           x = "K-value",
           y = "Average Error") +
      theme_minimal()
  })
  
  
  
  output$tab1_plot3 <- renderPlot({
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
      geom_point(data = test_data, aes(x = x1, y = x2, color = y_orig), size = 2) +
      labs(title = paste("Decision Boundary with K =", k_fixed),
           x = "X1", y = "X2") +
      scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
      scale_color_manual(values = c("blue", "red"), name = "Actual") +
      theme_minimal() +  
      theme(legend.position = "right")
  })
  
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
