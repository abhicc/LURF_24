
library(shiny)
library(ggplot2)
library(mvtnorm)

# Define UI
ui <- fluidPage(
  titlePanel("Logistic Regression: Decision Boundary with True/False Classification"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("num_ob", "Number of observations",
                  min = 100, max = 500, value = 100, step = 50),
      sliderInput("epsilon", "Variability (Noise)",
                  min = 0, max = 1, value = 0.5, step = 0.1)
    ),
    
    mainPanel(
      plotOutput("logreg_plot")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Reactive data generation
  df <- reactive({
    set.seed(123)  # For reproducibility
    
    num_training_points <- input$num_ob
    noise <- input$epsilon
    
    # Generate bivariate normal meta-means for two classes
    mu_class0 <- c(1, -1)
    mu_class1 <- c(-1, 1)
    cov_mat <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
    means_class0 <- rmvnorm(n = 10, mean = mu_class0, sigma = cov_mat)
    means_class1 <- rmvnorm(n = 10, mean = mu_class1, sigma = cov_mat)
    
    # Generate training data
    training_obs_class0 <- matrix(nrow = num_training_points / 2, ncol = 2)
    training_obs_class1 <- matrix(nrow = num_training_points / 2, ncol = 2)
    for(i in 1:(num_training_points / 2)) {
      s <- sample(1:10, size = 1)
      training_obs_class0[i, ] <- rmvnorm(n = 1, mean = means_class0[s, ], sigma = matrix(c(noise, 0, 0, noise), nrow = 2, ncol = 2))
      
      s <- sample(1:10, size = 1)
      training_obs_class1[i, ] <- rmvnorm(n = 1, mean = means_class1[s, ], sigma = matrix(c(noise, 0, 0, noise), nrow = 2, ncol = 2))
    }
    
    training_data <- data.frame(
      x1 = c(training_obs_class0[, 1], training_obs_class1[, 1]),
      x2 = c(training_obs_class0[, 2], training_obs_class1[, 2]),
      y = factor(rep(c(0, 1), each = num_training_points / 2), levels = c(0, 1))
    )
    
    training_data
  })
  
  output$logreg_plot <- renderPlot({
    training_data <- df()
    
    # Fit logistic regression model
    logreg_model <- glm(y ~ x1 + x2, data = training_data, family = binomial)
    
    # Generate a grid of values for x1 and x2 to plot the decision boundary
    x1_range <- seq(min(training_data$x1) - 1, max(training_data$x1) + 1, length.out = 100)
    x2_range <- seq(min(training_data$x2) - 1, max(training_data$x2) + 1, length.out = 100)
    grid <- expand.grid(x1 = x1_range, x2 = x2_range)
    
    # Predict probabilities on the grid
    grid$prob <- predict(logreg_model, newdata = grid, type = "response")
    grid$prediction <- ifelse(grid$prob > 0.5, 1, 0)
    
    # Predict on training data
    training_data$prob <- predict(logreg_model, newdata = training_data, type = "response")
    training_data$predicted_class <- ifelse(training_data$prob > 0.5, 1, 0)
    training_data$correct <- ifelse(training_data$y == training_data$predicted_class, "True", "False")
    
    # Plot decision boundary and training data
    ggplot() +
      geom_tile(data = grid, aes(x = x1, y = x2, fill = as.factor(prediction)), alpha = 0.3) +
      geom_point(data = training_data, aes(x = x1, y = x2, color = correct), size = 2) +
      labs(title = "Logistic Regression Decision Boundary",
           x = "X1", y = "X2", fill = "Prediction") +
      scale_fill_manual(values = c("blue", "red")) +
      scale_color_manual(values = c("True" = "green", "False" = "orange")) +
      theme_minimal() +
      theme(legend.position = "top")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

