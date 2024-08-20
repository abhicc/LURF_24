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
                  min = 2, max = 10, value = 5, step = 1)
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
    set.seed(123)
    
    num_training_points <- input$num_ob
    noise <- input$epsilon
    
    mu_class0 <- c(0.8, -0.8)
    mu_class1 <- c(-0.8, 0.8)
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
    list(training_data = data, test_data = data)  # Reuse training data as test data for simplicity
  })
  
  output$plot1 <- renderPlot({
    data_list <- df()
    train_data <- data_list$training_data
    
    x_train <- train_data[, 1:2]
    y_train <- as.numeric(as.character(train_data$y))  # Ensure y is numeric (0 and 1)
    
    # Generate a grid of values for x1 and x2
    x1_range <- seq(min(x_train[, 1]) - 1, max(x_train[, 1]) + 1, length.out = 100)
    x2_range <- seq(min(x_train[, 2]) - 1, max(x_train[, 2]) + 1, length.out = 100)
    grid <- expand.grid(x1 = x1_range, x2 = x2_range)
    
    # Fit the neural network model with nnet
    model <- nnet(y ~ x1 + x2, data = train_data, size = input$node, 
                  linout = FALSE, decay = 0.01, maxit = 1000)
    
    # Predict on the grid
    grid$prob <- predict(model, newdata = grid)
    
    # Convert predictions to class labels for plotting
    grid$prediction <- as.factor(ifelse(grid$prob > 0.5, "1", "0"))
    
    # Plot
    ggplot() +
      geom_tile(data = grid, aes(x = x1, y = x2, fill = prediction), alpha = 0.3) +
      geom_point(data = train_data, aes(x = x1, y = x2, color = y), size = 2) +
      labs(title = "Neural Network Decision Boundary",
           x = "X1", y = "X2") +
      scale_fill_manual(values = c("blue", "red"), name = "Prediction") +
      scale_color_manual(values = c("blue", "red"), name = "Actual") +
      theme_minimal() +  
      theme(legend.position = "top")
  })
  


  



  
}

# Run the application
shinyApp(ui = ui, server = server)
