library(shiny)
library(ggplot2)
library(e1071)
library(mvtnorm)

# Define UI
ui <- fluidPage(
  titlePanel("SVM Model Visualization with Margin"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      sliderInput("num_ob", "Number of observations",
                  min = 100, max = 200, value = 100, step = 50),
      
      sliderInput("epsilon", "Variability",
                  min = 0.2, max = 0.8, value = 0.4, step = 0.2),
      
      sliderInput("c_param", "Regularization Parameter (C)",
                  min = 0.01, max = 3, value = 1, step = 0.01)  # Adjusted slider
    ),
    
    mainPanel(
      fluidRow(
        column(7, plotOutput("svm_plot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data generation
  df <- reactive({
    set.seed(123)
    
    num_training_points <- input$num_ob
    noise <- input$epsilon
    
    mu_class0 <- c(1, -1)
    mu_class1 <- c(-1, 1)
    cov_mat <- matrix(c(1,0,0,1), nrow=2, ncol=2)
    means_class0 <- rmvnorm(n = 10, mean = mu_class0, sigma = cov_mat)
    means_class1 <- rmvnorm(n = 10, mean = mu_class1, sigma = cov_mat)
    
    training_obs_class0 <- matrix(nrow=num_training_points/2, ncol=2)
    training_obs_class1 <- matrix(nrow=num_training_points/2, ncol=2)
    for(i in 1:(num_training_points/2)) {
      s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      training_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                         sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
      
      s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      training_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                         sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
    }
    data <- data.frame(rbind(training_obs_class0, training_obs_class1), 
                       y = factor(rep(c("0", "1"), each=num_training_points/2), 
                                  levels = c("0", "1")))
    names(data) <- c("x1", "x2", "y")
    data
  })
  
  output$svm_plot <- renderPlot({
    data <- df()
    
    # Fit SVM model with a linear kernel and regularization parameter C
    svm_model <- svm(y ~ x1 + x2, data = data, kernel = "linear", 
                     probability = TRUE, cost = input$c_param)
    
    # Generate a grid of values for x1 and x2
    x1_range <- seq(min(data$x1) - 1, max(data$x1) + 1, length.out = 100)
    x2_range <- seq(min(data$x2) - 1, max(data$x2) + 1, length.out = 100)
    grid <- expand.grid(x1 = x1_range, x2 = x2_range)
    
    # Predict on the grid
    grid$pred <- predict(svm_model, newdata = grid, decision.values = TRUE)
    
    # Extract the support vectors
    support_vectors <- data[svm_model$index, ]
    
    # Extract decision values for plotting margins
    decision_values <- attributes(predict(svm_model, grid, decision.values = TRUE))$decision.values
    
    # Plot the decision boundary with margin
    ggplot(data) +
      geom_tile(data = grid, aes(x = x1, y = x2, fill = as.factor(pred)), alpha = 0.3) +
      geom_point(aes(x = x1, y = x2, color = y), size = 2) +
      geom_point(data = support_vectors, aes(x = x1, y = x2), shape = 1, size = 4, color = "black") +
      geom_contour(data = grid, aes(x = x1, y = x2, z = decision_values), 
                   breaks = c(-1, 0, 1), linetype = "dashed", color = "black") +
      labs(title = paste("SVM Decision Boundary (Linear Kernel) with C =", input$c_param),
           x = "X1", y = "X2") +
      scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
      scale_color_manual(values = c("blue", "red"), name = "Actual") +
      theme_minimal() +
      theme(legend.position = "top")
  })
}

# Run the application
shinyApp(ui = ui, server = server)


