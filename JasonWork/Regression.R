library(shiny)
library(ggplot2)
library(mvtnorm)
library(keras)

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
      
      sliderInput("node", "Number of Nodes", 
                  min = 2, max = 10, value = 5, step = 1)
    ),
    
    mainPanel(
      fluidRow(
        column(12, plotOutput("plot1"))
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
    data
  })
  
  output$plot1 <- renderPlot({
    data <- df()
    
    # Build the neural network model
    model <- keras_model_sequential() %>%
      layer_dense(units = input$node, activation = 'relu', input_shape = c(2)) %>%
      layer_dense(units = 1, activation = 'sigmoid')
    
    model %>% compile(
      optimizer = 'adam',
      loss = 'binary_crossentropy',
      metrics = 'accuracy'
    )
    
    # Train the model
    model %>% fit(
      as.matrix(data[, c("x1", "x2")]), 
      as.numeric(data$y) - 1,
      epochs = 20,
      batch_size = 32,
      verbose = 0
    )
    
    # Generate grid for plotting decision boundary
    x_seq <- seq(min(data$x1) - 1, max(data$x1) + 1, length.out = 100)
    y_seq <- seq(min(data$x2) - 1, max(data$x2) + 1, length.out = 100)
    grid <- expand.grid(x1 = x_seq, x2 = y_seq)
    grid$prob <- model %>% predict(as.matrix(grid)) %>% as.vector()
    grid$pred <- ifelse(grid$prob > 0.5, "1", "0")
    
    # Plot decision boundary
    ggplot(data, aes(x = x1, y = x2, color = y)) +
      geom_point() +
      stat_contour(data = grid, aes(x = x1, y = x2, z = prob), color = 'blue') +
      labs(title = "Decision Boundary of Neural Network",
           x = "X1", y = "X2") +
      scale_color_manual(values = c("red", "green")) +
      theme_minimal()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
