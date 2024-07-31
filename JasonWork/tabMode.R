library(shiny)
library(ggplot2)
library(FNN) # Ensure this library is loaded for KNN functionality

# Define UI
ui <- fluidPage(
  titlePanel("BIAS-VARIANCE TRADEOFF VISUALIZATION"),
  
  sidebarPanel(
    # Drop-down menu for selecting datasets
    selectInput("dataset", "Dataset",
                choices = c("Data set 1", "Data set 2", "Data set 3"),
                selected = "Data set 2"),
    
    # Common sliders for all tabs
    sliderInput("num_ob", "Number of observations",
                min = 50, max = 250, value = 100, step = 100),
    
    # Dynamic slider for epsilon
    uiOutput("epsilon_slider")
  ),
  
  mainPanel(
    tabsetPanel(
      id = "tabs",
      tabPanel("Non-Linear",
               sliderInput("flexibility", "Flexibility", 
                           min = 1, max = 5, value = 1.5, step = 0.5),
               checkboxInput("show_tab1_plot5", "Show Additional Plot", value = FALSE),
               fluidRow(
                 column(3, plotOutput("tab1_plot1")),
                 column(3, plotOutput("tab1_plot2")),
                 column(3, plotOutput("tab1_plot3")),
                 column(3, plotOutput("tab1_plot4")),
                 column(3, conditionalPanel(
                   condition = "input.show_tab1_plot5 == true",
                   plotOutput("tab1_plot5")
                 ))
               )
      ),
      tabPanel("Polynomial",
               sliderInput("degree", "Polynomial degree:", 
                           min = 1, max = 5, value = 2, step = 1),
               checkboxInput("show_tab2_plot5", "Show Additional Plot", value = FALSE),
               fluidRow(
                 column(3, plotOutput("tab2_plot1")),
                 column(3, plotOutput("tab2_plot2")),
                 column(3, plotOutput("tab2_plot3")),
                 column(3, plotOutput("tab2_plot4")),
                 column(3, conditionalPanel(
                   condition = "input.show_tab2_plot5 == true",
                   plotOutput("tab2_plot5")
                 ))
               )
      ),
      tabPanel("KNN",
               sliderInput("k_value", "K-value", 
                           min = 3, max = 15, value = 5, step = 1),
               checkboxInput("show_tab3_plot5", "Show Additional Plot", value = FALSE),
               fluidRow(
                 column(3, plotOutput("tab3_plot1")),
                 column(3, plotOutput("tab3_plot2")),
                 column(3, plotOutput("tab3_plot3")),
                 column(3, plotOutput("tab3_plot4")),
                 column(3, conditionalPanel(
                   condition = "input.show_tab3_plot5 == true",
                   plotOutput("tab3_plot5")
                 ))
               )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Constants
  a <- 3
  b <- 0.87
  c <- 0.5
  
  # Functions to generate data
  generate_data <- function(x, fx, epsilon, num_responses = 20) {
    n <- length(x)
    responses <- data.frame(matrix(ncol = num_responses, nrow = n))
    colnames(responses) <- paste0("response", 1:num_responses)
    
    for (i in 1:num_responses) {
      e <- rnorm(n, mean = 0, sd = epsilon)
      responses[, i] <- fx + e
    }
    
    data.frame(inp = x, true_form = fx, responses)
  }
  
  generate_test_data <- function(x, fx, epsilon) {
    n <- length(x)
    e <- rnorm(n, mean = 0, sd = epsilon)
    y <- fx + e
    data.frame(inp = x, true_form = fx, observed = y)
  }
  
  # Reactive data generation
  df <- reactive({
    toy_data <- NULL
    test_data <- NULL
    
    if (input$dataset == "Data set 1") {
      set.seed(2024)
      x <- runif(n = input$num_ob, min = -5, max = 15)
      fx <- ifelse(0.1*(x^2)<3, 0.1*(x^2), 3)
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      x_test <- runif(n = 1000, min = -5, max = 15)
      fx_test <- ifelse(0.1*(x_test^2)<3, 0.1*(x_test^2), 3)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    } else if (input$dataset == "Data set 2") {
      set.seed(2024)
      x <- runif(n = input$num_ob, min = 20, max = 40)
      fx <- a + (b * sqrt(x)) + (c * sin(x))
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      x_test <- runif(n = 1000, min = 20, max = 40)
      fx_test <- a + (b * sqrt(x_test)) + (c * sin(x_test))
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    } else if (input$dataset == "Data set 3") {
      set.seed(2024)
      x <- runif(n = input$num_ob, min = -5, max = 5)
      fx <- a + (b * x^2) + (c * x^3)
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      x_test <- runif(n = 1000, min = -5, max = 5)
      fx_test <- a + (b * x_test^2) + (c * x_test^3)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    }
    
    return(list(toy_data = toy_data, test_data = test_data))
  })
  
  # Reactive model type based on active tab
  model_type <- reactive({
    switch(input$tabs,
           "Non-Linear" = "Non-Linear",
           "Polynomial" = "Polynomial",
           "KNN" = "KNN",
           "Unknown")  # Default case if no tab is selected
  })
  # Dynamically render the epsilon slider based on the selected dataset
  output$epsilon_slider <- renderUI({
    if (input$dataset == "Data set 2") {
      sliderInput("epsilon", "Variability", 
                  min = 0, max = 1, value = 0.5, step = 0.1)
    } else if (input$dataset == "Data set 3") {
        sliderInput("epsilon", "Variability", 
                    min = 3, max = 9, value = 5, step = 0.5)
      } else {
      sliderInput("epsilon", "Variability", 
                  min = 0, max = 0.6, value = 0.2, step = 0.1)
    }
  })
  
  # Non-Linear Tab Plots
  output$tab1_plot1 <- renderPlot({
    df_data <- df()$toy_data
    
    if (is.null(df_data)) {
      plot(1, type = "n", main = "No Data Available")
      return(NULL)
    }
    
    p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
      geom_point() +
      labs(title = "Training Data", y = "y", x = "x")
    
    if (input$dataset == "Data set 1") {
      p <- p + scale_y_continuous(limits = c(0, 5)) +
        scale_x_continuous(limits = c(0, 15))
    } else if (input$dataset == "Data set 2") {
      p <- p + scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    } else if (input$dataset == "Data set 3") {
      p <- p + scale_y_continuous(limits = c(-30, 30)) +
        scale_x_continuous(limits = c(-6, 6))
    }
    
    # Use the reactive model_type
    if (model_type() == "Non-Linear") {
      p <- p + geom_smooth(span = 1/input$flexibility, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
    } else if (model_type() == "Polynomial") {
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
    } else if (model_type() == "KNN") {
      if (!is.null(input$k_value) && input$k_value > 0) {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
        df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
        p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1)
      } else {
        p <- p + ggtitle("Invalid KNN Parameters")
      }
    } 
    
    print(p)
  })
  
  
  output$tab1_plot2 <- renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
   
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define the range of complexities or k values
    if (model_type() == "Non-Linear") {
      complexities <- seq(1, 5, by = 1)
    } else if (model_type() == "Polynomial") {
      complexities <- 1:5
    } else if (model_type() == "KNN") {
      complexities <- 3:15  # Use k values for KNN
    } else {
      complexities <- numeric()  # No complexities for unknown model types
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (model_type() == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "KNN") {
        for (i in 1:20) {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
          predictions[, i] <- knn_fit$pred
        }
      }
      
      # Calculate average predictions
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      
      # Ensure true_form is correctly aligned with the predictions
      true_form <- df_testdata$true_form
      
      # Calculate bias
      bias <- avg_predictions - true_form
      
      # Calculate squared bias
      squared_bias <- bias^2
      
      # Calculate overall squared bias
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      # Calculate variances of predictions
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      
      # Calculate overall variance
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      # Store metrics for current complexity/k value
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    # Plot Bias
    plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
      labs(
        title = "Bias",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Bias"  # Provide a label for y-axis
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_bias)
  })
  
  
  output$tab1_plot3 <- renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    if (is.null(df_data) || is.null(df_testdata)) {
      plot(1, type = "n", main = "No Data Available")
      return(NULL)
    }
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define the range of complexities or k values
    if (model_type() == "Linear") {
      complexities <- 1  # Linear model has no complexity parameter
    } else if (model_type() == "Non-Linear") {
      complexities <- seq(1, 5, by = 1)
    } else if (model_type() == "Polynomial") {
      complexities <- 1:5
    } else if (model_type() == "KNN") {
      complexities <- 3:15  # Use k values for KNN
    } else {
      complexities <- numeric()  # No complexities for unknown model types
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (model_type() == "Linear") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "KNN") {
        for (i in 1:20) {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
          predictions[, i] <- knn_fit$pred
        }
      }
      
      # Calculate average predictions
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      
      # Ensure true_form is correctly aligned with the predictions
      true_form <- df_testdata$true_form
      
      # Calculate bias
      bias <- avg_predictions - true_form
      
      # Calculate squared bias
      squared_bias <- bias^2
      
      # Calculate overall squared bias
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      # Calculate variances of predictions
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      
      # Calculate overall variance
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      # Store metrics for current complexity/k value
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    # Plot Variance
    plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
      labs(
        title = "Variance",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Variance"  # Provide a label for y-axis
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_variance)
  })
  
  
  output$tab1_plot4 <- renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define complexities or k values
    complexities <- switch(model_type(),  # Use model_type() instead of input$model_name
                           "Linear" = 1,  # Linear model has no complexity parameter
                           "Non-Linear" = seq(1, 5, by = 1),
                           "Polynomial" = 1:5,
                           "KNN" = 3:15)  # Add k values for KNN
    
    for (complexity in complexities) {
      predictions <- matrix(nrow = 1000, ncol = 20)
      training_mse_list <- numeric(20)
      
      for (i in 1:20) {
        response_col <- paste0("response", i)
        
        if (model_type() == "Linear") {
          model <- lm(df_data[[response_col]] ~ inp, data = df_data)
        } else if (model_type() == "Non-Linear") {
          model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
        } else if (model_type() == "Polynomial") {
          model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
        } else if (model_type() == "KNN") {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
          predictions[, i] <- knn_fit$pred
          training_preds <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data[[response_col]], k = complexity)$pred
          training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
          next
        }
        
        predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        training_preds <- predict(model, newdata = data.frame(inp = df_data$inp))
        training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
      }
      
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      true_form <- df_testdata$true_form[1:1000]
      bias <- avg_predictions - true_form
      squared_bias <- bias^2
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
      training_MSE <- mean(training_mse_list, na.rm = TRUE)
      
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "test MSE", Value = test_MSE),
        data.frame(Complexity = complexity, Metric = "training MSE", Value = training_MSE)
      )
    }
    
    # Plot training and test MSE
    plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Training MSE vs. Test MSE",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Mean Squared Error"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"  # Move legend to the top
      )
    
    print(plot_mse)
  })
  
  output$tab1_plot5 <- renderPlot({
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    # Get complexity from input$flexibility
    complexity <- input$flexibility
    # Create a data frame to store true values and predictions
    plot_data <- data.frame(
      inp = rep(df_testdata$inp, 8),  # True form + 7 models
      Value = c(df_testdata$true_form, rep(NA, 7 * nrow(df_testdata))),
      Type = rep(c("True", paste0("Model_", 1:7)), each = nrow(df_testdata))
    )
    
    # Initialize a matrix to store predictions
    predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
    
    for (i in 1:20) {
      response_col <- paste0("response", i)
      
      if (model_type() == "Linear") {
        model <- lm(df_data[[response_col]] ~ inp, data = df_data)
      } else if (model_type() == "Non-Linear") {
        model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
      } else if (model_type() == "Polynomial") {
        model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
      } else if (model_type() == "KNN") {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
        predictions[, i] <- knn_fit$pred
        next
      }
      
      predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
    }
    
    # Plot the true form and the first 7 models
    for (i in 1:7) {
      plot_data$Value[plot_data$Type == paste0("Model_", i)] <- predictions[, i]
    }
    
    # Plot true form and predictions for the first 7 models
    plot_predictions <- ggplot(plot_data, aes(x = inp, y = Value, color = Type, group = Type)) +
      geom_line(size = 1) +
      labs(
        title = "True Form vs. Model Predictions",
        x = "x",
        y = "y"
      ) +
      scale_color_manual(values = c("True" = "black", 
                                    "Model_1" = "pink", 
                                    "Model_2" = "pink", 
                                    "Model_3" = "pink", 
                                    "Model_4" = "pink", 
                                    "Model_5" = "pink", 
                                    "Model_6" = "pink", 
                                    "Model_7" = "pink")) +
      theme_minimal() +
      theme(
        legend.position = "none"  # Hide the legend
      )
    
    print(plot_predictions)
  })
  
  
 
  
###################
  output$tab2_plot1 <- renderPlot({
    df_data <- df()$toy_data
    
    if (is.null(df_data)) {
      plot(1, type = "n", main = "No Data Available")
      return(NULL)
    }
    
    p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
      geom_point() +
      labs(title = "Training Data", y = "y", x = "x")
    
    if (input$dataset == "Data set 1") {
      p <- p + scale_y_continuous(limits = c(0, 5)) +
        scale_x_continuous(limits = c(0, 10))
    } else if (input$dataset == "Data set 2") {
      p <- p + scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    } else if (input$dataset == "Data set 3") {
      p <- p + scale_y_continuous(limits = c(-30, 30)) +
        scale_x_continuous(limits = c(-6, 6))
    }
    
    # Use the reactive model_type
    if (model_type() == "Non-Linear") {
      p <- p + geom_smooth(span = 1/input$flexibility, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
    } else if (model_type() == "Polynomial") {
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
    } else if (model_type() == "KNN") {
      if (!is.null(input$k_value) && input$k_value > 0) {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
        df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
        p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1)
      } else {
        p <- p + ggtitle("Invalid KNN Parameters")
      }
    } else {
      p <- p + ggtitle("Invalid Model Name")
    }
    
    print(p)
  })
  
  
  output$tab2_plot2 <- renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define the range of complexities or k values
    if (model_type() == "Non-Linear") {
      complexities <- seq(1, 5, by = 1)
    } else if (model_type() == "Polynomial") {
      complexities <- 1:5
    } else if (model_type() == "KNN") {
      complexities <- 3:15  # Use k values for KNN
    } else {
      complexities <- numeric()  # No complexities for unknown model types
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (model_type() == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "KNN") {
        for (i in 1:20) {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
          predictions[, i] <- knn_fit$pred
        }
      }
      
      # Calculate average predictions
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      
      # Ensure true_form is correctly aligned with the predictions
      true_form <- df_testdata$true_form
      
      # Calculate bias
      bias <- avg_predictions - true_form
      
      # Calculate squared bias
      squared_bias <- bias^2
      
      # Calculate overall squared bias
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      # Calculate variances of predictions
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      
      # Calculate overall variance
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      # Store metrics for current complexity/k value
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    # Plot Bias
    plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
      labs(
        title = "Bias",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Bias"  # Provide a label for y-axis
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_bias)
  })
  
  
  output$tab2_plot3 <- renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    if (is.null(df_data) || is.null(df_testdata)) {
      plot(1, type = "n", main = "No Data Available")
      return(NULL)
    }
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define the range of complexities or k values
    if (model_type() == "Linear") {
      complexities <- 1  # Linear model has no complexity parameter
    } else if (model_type() == "Non-Linear") {
      complexities <- seq(0.1, 5, by = 1)
    } else if (model_type() == "Polynomial") {
      complexities <- 1:5
    } else if (model_type() == "KNN") {
      complexities <- 3:15  # Use k values for KNN
    } else {
      complexities <- numeric()  # No complexities for unknown model types
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (model_type() == "Linear") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "KNN") {
        for (i in 1:20) {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
          predictions[, i] <- knn_fit$pred
        }
      }
      
      # Calculate average predictions
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      
      # Ensure true_form is correctly aligned with the predictions
      true_form <- df_testdata$true_form
      
      # Calculate bias
      bias <- avg_predictions - true_form
      
      # Calculate squared bias
      squared_bias <- bias^2
      
      # Calculate overall squared bias
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      # Calculate variances of predictions
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      
      # Calculate overall variance
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      # Store metrics for current complexity/k value
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    # Plot Variance
    plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
      labs(
        title = "Variance",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Variance"  # Provide a label for y-axis
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_variance)
  })
  
  
  output$tab2_plot4 <-renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define complexities or k values
    complexities <- switch(model_type(),  # Use model_type() instead of input$model_name
                           "Linear" = 1,  # Linear model has no complexity parameter
                           "Non-Linear" = seq(0.1, 5, by = 1),
                           "Polynomial" = 1:5,
                           "KNN" = 3:15)  # Add k values for KNN
    
    for (complexity in complexities) {
      predictions <- matrix(nrow = 1000, ncol = 20)
      training_mse_list <- numeric(20)
      
      for (i in 1:20) {
        response_col <- paste0("response", i)
        
        if (model_type() == "Linear") {
          model <- lm(df_data[[response_col]] ~ inp, data = df_data)
        } else if (model_type() == "Non-Linear") {
          model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
        } else if (model_type() == "Polynomial") {
          model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
        } else if (model_type() == "KNN") {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
          predictions[, i] <- knn_fit$pred
          training_preds <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data[[response_col]], k = complexity)$pred
          training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
          next
        }
        
        predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        training_preds <- predict(model, newdata = data.frame(inp = df_data$inp))
        training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
      }
      
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      true_form <- df_testdata$true_form[1:1000]
      bias <- avg_predictions - true_form
      squared_bias <- bias^2
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
      training_MSE <- mean(training_mse_list, na.rm = TRUE)
      
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "test MSE", Value = test_MSE),
        data.frame(Complexity = complexity, Metric = "training MSE", Value = training_MSE)
      )
    }
    
    # Plot training and test MSE
    plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Training MSE vs. Test MSE",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Mean Squared Error"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"  # Move legend to the top
      )
    
    print(plot_mse)
  })
  
  output$tab2_plot5 <- renderPlot({
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    # Get complexity from input$flexibility
    complexity <- input$degree
    # Create a data frame to store true values and predictions
    plot_data <- data.frame(
      inp = rep(df_testdata$inp, 8),  # True form + 7 models
      Value = c(df_testdata$true_form, rep(NA, 7 * nrow(df_testdata))),
      Type = rep(c("True", paste0("Model_", 1:7)), each = nrow(df_testdata))
    )
    
    # Initialize a matrix to store predictions
    predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
    
    for (i in 1:20) {
      response_col <- paste0("response", i)
      
      if (model_type() == "Linear") {
        model <- lm(df_data[[response_col]] ~ inp, data = df_data)
      } else if (model_type() == "Non-Linear") {
        model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
      } else if (model_type() == "Polynomial") {
        model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
      } else if (model_type() == "KNN") {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
        predictions[, i] <- knn_fit$pred
        next
      }
      
      predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
    }
    
    # Ensure there are enough predictions
    num_models <- min(7, ncol(predictions))
    
    # Plot the true form and the first num_models models
    for (i in 1:num_models) {
      plot_data$Value[plot_data$Type == paste0("Model_", i)] <- predictions[, i]
    }
    
    # Plot true form and predictions for the first num_models models
    plot_predictions <- ggplot(plot_data, aes(x = inp, y = Value, color = Type, group = Type)) +
      geom_line(size = 1) +
      labs(
        title = "True Form vs. Model Predictions",
        x = "x",
        y = "y"
      ) +
      scale_color_manual(values = c("True" = "black", 
                                    "Model_1" = "pink", 
                                    "Model_2" = "pink", 
                                    "Model_3" = "pink", 
                                    "Model_4" = "pink", 
                                    "Model_5" = "pink", 
                                    "Model_6" = "pink", 
                                    "Model_7" = "pink")) +
      theme_minimal() +
      theme(
        legend.position = "none"  # Hide the legend
      )
    
    print(plot_predictions)
  })
  
  
  
  
  
  ###################
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  output$tab3_plot1 <- renderPlot({
    df_data <- df()$toy_data
    
    
    
    p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
      geom_point() +
      labs(title = "Training Data", y = "y", x = "x")
    
    # Conditional limits based on the dataset
    if (input$dataset == "Data set 1") {
      p <- p + scale_y_continuous(limits = c(0, 5)) +
        scale_x_continuous(limits = c(0, 10))
    } else if (input$dataset == "Data set 2") {
      p <- p + scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
    } else if (input$dataset == "Data set 3") {
      p <- p + scale_y_continuous(limits = c(-30, 30)) +
        scale_x_continuous(limits = c(-6, 6))
    }
    
    # Use the reactive model_type
    if (model_type() == "Non-Linear") {
      p <- p + geom_smooth(span = 1/input$flexibility, se = FALSE, color = "red", show.legend = FALSE)
    } else if (model_type() == "Polynomial") {
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, color = "red", show.legend = FALSE)
    } else if (model_type() == "KNN") {
      if (!is.null(input$k_value) && input$k_value > 0) {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
        df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
        p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1, color = "red")
      } else {
        p <- p + ggtitle("Invalid KNN Parameters")
      }
    } 
    print(p)
  })
  
  
  output$tab3_plot2 <- renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define the range of complexities or k values
    if (model_type() == "Non-Linear") {
      complexities <- seq(0.1, 5, by = 1)
    } else if (model_type() == "Polynomial") {
      complexities <- 1:5
    } else if (model_type() == "KNN") {
      complexities <- 3:15  # Use k values for KNN
    } else {
      complexities <- numeric()  # No complexities for unknown model types
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (model_type() == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "KNN") {
        for (i in 1:20) {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
          predictions[, i] <- knn_fit$pred
        }
      }
      
      # Calculate average predictions
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      
      # Ensure true_form is correctly aligned with the predictions
      true_form <- df_testdata$true_form
      
      # Calculate bias
      bias <- avg_predictions - true_form
      
      # Calculate squared bias
      squared_bias <- bias^2
      
      # Calculate overall squared bias
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      # Calculate variances of predictions
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      
      # Calculate overall variance
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      # Store metrics for current complexity/k value
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    # Plot Bias
    plot_bias <- ggplot(metrics[metrics$Metric == "Bias", ], aes(x = as.factor(Complexity), y = Value)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "blue") +
      labs(
        title = "Bias",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Bias"  # Provide a label for y-axis
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_bias)
  })
  
  
  output$tab3_plot3 <- renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    if (is.null(df_data) || is.null(df_testdata)) {
      plot(1, type = "n", main = "No Data Available")
      return(NULL)
    }
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define the range of complexities or k values
    if (model_type() == "Linear") {
      complexities <- 1  # Linear model has no complexity parameter
    } else if (model_type() == "Non-Linear") {
      complexities <- seq(0.1, 5, by = 1)
    } else if (model_type() == "Polynomial") {
      complexities <- 1:5
    } else if (model_type() == "KNN") {
      complexities <- 3:15  # Use k values for KNN
    } else {
      complexities <- numeric()  # No complexities for unknown model types
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (model_type() == "Linear") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (model_type() == "KNN") {
        for (i in 1:20) {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[, i + 2], k = complexity)
          predictions[, i] <- knn_fit$pred
        }
      }
      
      # Calculate average predictions
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      
      # Ensure true_form is correctly aligned with the predictions
      true_form <- df_testdata$true_form
      
      # Calculate bias
      bias <- avg_predictions - true_form
      
      # Calculate squared bias
      squared_bias <- bias^2
      
      # Calculate overall squared bias
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      
      # Calculate variances of predictions
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      
      # Calculate overall variance
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      
      # Store metrics for current complexity/k value
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "Bias", Value = overall_squared_bias),
        data.frame(Complexity = complexity, Metric = "Variance", Value = overall_variance)
      )
    }
    
    # Plot Variance
    plot_variance <- ggplot(metrics[metrics$Metric == "Variance", ], aes(x = as.factor(Complexity), y = Value)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.5, alpha = 0.8, fill = "red") +
      labs(
        title = "Variance",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Variance"  # Provide a label for y-axis
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_variance)
  })
  
  output$tab3_plot4 <-renderPlot({
    
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    # Initialize a data frame to store bias and variance for different complexities/k values
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define complexities or k values
    complexities <- switch(model_type(),  # Use model_type() instead of input$model_name
                           "Linear" = 1,  # Linear model has no complexity parameter
                           "Non-Linear" = seq(0.1, 5, by = 1),
                           "Polynomial" = 1:5,
                           "KNN" = 3:15)  # Add k values for KNN
    
    for (complexity in complexities) {
      predictions <- matrix(nrow = 1000, ncol = 20)
      training_mse_list <- numeric(20)
      
      for (i in 1:20) {
        response_col <- paste0("response", i)
        
        if (model_type() == "Linear") {
          model <- lm(df_data[[response_col]] ~ inp, data = df_data)
        } else if (model_type() == "Non-Linear") {
          model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
        } else if (model_type() == "Polynomial") {
          model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
        } else if (model_type() == "KNN") {
          knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
          predictions[, i] <- knn_fit$pred
          training_preds <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data[[response_col]], k = complexity)$pred
          training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
          next
        }
        
        predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        training_preds <- predict(model, newdata = data.frame(inp = df_data$inp))
        training_mse_list[i] <- mean((df_data[[response_col]] - training_preds)^2)
      }
      
      avg_predictions <- rowMeans(predictions, na.rm = TRUE)
      true_form <- df_testdata$true_form[1:1000]
      bias <- avg_predictions - true_form
      squared_bias <- bias^2
      overall_squared_bias <- mean(squared_bias, na.rm = TRUE)
      prediction_vars <- apply(predictions, 1, var, na.rm = TRUE)
      overall_variance <- mean(prediction_vars, na.rm = TRUE)
      test_MSE <- overall_variance + overall_squared_bias + (input$epsilon)^2
      training_MSE <- mean(training_mse_list, na.rm = TRUE)
      
      metrics <- rbind(
        metrics,
        data.frame(Complexity = complexity, Metric = "test MSE", Value = test_MSE),
        data.frame(Complexity = complexity, Metric = "training MSE", Value = training_MSE)
      )
    }
    
    # Plot training and test MSE
    plot_mse <- ggplot(metrics, aes(x = as.factor(Complexity), y = Value, color = Metric, group = Metric)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(
        title = "Training MSE vs. Test MSE",
        x = ifelse(model_type() == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Mean Squared Error"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("test MSE" = "purple", "training MSE" = "green")) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top"  # Move legend to the top
      )
    
    print(plot_mse)
  })
  
  output$tab3_plot5 <- renderPlot({
    # Get the current value of reactive data frame df
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    # Get complexity from input$flexibility
    complexity <- input$flexibility
    # Create a data frame to store true values and predictions
    plot_data <- data.frame(
      inp = rep(df_testdata$inp, 8),  # True form + 7 models
      Value = c(df_testdata$true_form, rep(NA, 7 * nrow(df_testdata))),
      Type = rep(c("True", paste0("Model_", 1:7)), each = nrow(df_testdata))
    )
    
    # Initialize a matrix to store predictions
    predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
    
    for (i in 1:20) {
      response_col <- paste0("response", i)
      
      if (model_type() == "Linear") {
        model <- lm(df_data[[response_col]] ~ inp, data = df_data)
      } else if (model_type() == "Non-Linear") {
        model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
      } else if (model_type() == "Polynomial") {
        model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
      } else if (model_type() == "KNN") {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_testdata$inp), y = df_data[[response_col]], k = complexity)
        predictions[, i] <- knn_fit$pred
        next
      }
      
      predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
    }
    
    # Plot the true form and the first 7 models
    for (i in 1:7) {
      plot_data$Value[plot_data$Type == paste0("Model_", i)] <- predictions[, i]
    }
    
    # Plot true form and predictions for the first 7 models
    plot_predictions <- ggplot(plot_data, aes(x = inp, y = Value, color = Type, group = Type)) +
      geom_line(size = 1) +
      labs(
        title = "True Form vs. Model Predictions",
        x = "x",
        y = "y"
      ) +
      scale_color_manual(values = c("True" = "black", 
                                    "Model_1" = "pink", 
                                    "Model_2" = "pink", 
                                    "Model_3" = "pink", 
                                    "Model_4" = "pink", 
                                    "Model_5" = "pink", 
                                    "Model_6" = "pink", 
                                    "Model_7" = "pink")) +
      theme_minimal() +
      theme(
        legend.position = "none"  # Hide the legend
      )
    
    print(plot_predictions)
  })
  
  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
