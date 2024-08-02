library(shiny)
library(ggplot2)
library(mvtnorm)

# Define UI
ui <- fluidPage(
  titlePanel("BIAS-VARIANCE TRADEOFF VISUALIZATION"),
  
  sidebarPanel(
    # Drop-down menu for selecting datasets
    selectInput("dataset", "Dataset",
                choices = c("Data set 1"),
                selected = "Data set 1"),
    
    # Common sliders for all tabs
    sliderInput("num_ob", "Number of observations",
                min = 50, max = 250, value = 100, step = 50),
    
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
    num_training_points <- input$num_ob
    noise <- input$epsilon
    
    # GENERATING BIVARIATE NORMAL META-MEANS AND 10 COMPONENT MEANS FOR EACH CLASS
    mu_class0 <- c(1, -1)
    mu_class1 <- c(-1, 1)
    cov_mat <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)
    means_class0 <- rmvnorm(n = 10, mean = mu_class0, sigma = cov_mat)
    means_class1 <- rmvnorm(n = 10, mean = mu_class1, sigma = cov_mat)
    
    # ONE TRAINING DATASET (USE SIMILAR CODE TO GENERATE TEST DATASET)
    training_obs_class0 <- matrix(nrow = num_training_points / 2, ncol = 2)
    training_obs_class1 <- matrix(nrow = num_training_points / 2, ncol = 2)
    for (i in 1:(num_training_points / 2)) {
      s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      training_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                         sigma = matrix(c(noise, 0, 0, noise), nrow = 2, ncol = 2))
      
      s <- sample(1:10, size = 1, prob = rep(1/10, 10))
      training_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                         sigma = matrix(c(noise, 0, 0, noise), nrow = 2, ncol = 2))
    }
    df_training <- data.frame(rbind(training_obs_class0, training_obs_class1), 
                              y_orig = factor(rep(c("0", "1"), each = num_training_points / 2), 
                                              levels = c("0", "1")))
    names(df_training) <- c("x1", "x2", "y_orig")
    
    df_training
  })
  
  # Plot the training dataset
  output$tab1_plot1 <- renderPlot({
    df_training <- df()
    ggplot(data = df_training, aes(x = x1, y = x2, color = y_orig)) +
      geom_point() +
      labs(title = "Training Dataset", x = "X1", y = "X2") +
      theme_minimal() +
      xlim(-5, 5) +  # Adjust these values to set the desired x-axis limits
      ylim(-5, 5)    # Adjust these values to set the desired y-axis limits
  })

  
  
}

# Run the application
shinyApp(ui = ui, server = server)
