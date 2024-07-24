library(tidyverse)
library(shiny)
library(FNN)

#let users choose the model



ui <- fluidPage(
  
  # Application title
  titlePanel("Model Bias and Variance"),
  
 
  
  fluidRow(
    
    column(2,
           selectInput(inputId = "dataset",
                       label = "Select a dataset",
                       choices = c("Data set 1", "Data set 2","Data set 3"),
                       selected = "Data set 1")),
    column(2,
           selectInput(inputId = "model_name",
                       label = "Select model",
                       choices = c("Non-Linear", "Polynomial", "KNN"),
                       selected = "Non-Linear")),
    
    column(2,
           sliderInput(inputId = "epsilon",
                       label = "Variability",
                       min = 0,
                       max = 7,
                       value = 1,
                       step = 0.5)),
    column(2,
           sliderInput(inputId = "num_ob",
                       label = "Number of observations",
                       min = 100,
                       max = 500,
                       value = 100,
                       step = 50)),
    
    column(2,
           conditionalPanel(
             condition = "input.model_name == 'Non-Linear'",
             sliderInput(inputId = "flex",
                         label = "Flexibility",
                         min = 0.1,
                         max = 8,
                         value = 1,
                         step = 0.1)
           )),
    column(2,
           conditionalPanel(
             condition = "input.model_name == 'KNN'",
             sliderInput(inputId = "k_value",
                         label = "K-value",
                         min = 3,
                         max = 10,
                         value = 3,
                         step = 1)
           )),
    column(2,
           conditionalPanel(
             condition = "input.model_name == 'Polynomial'",
             sliderInput(inputId = "degree",
                         label = "Polynomial degree",
                         min = 1,
                         max = 8,
                         value = 1,
                         step = 1)
           ))
  ),
    
    
 
  
  
  fluidRow(
    column(3, plotOutput("Plot1")),
    column(3, plotOutput("Plot2")),
    column(3, plotOutput("Plot3")),
    column(3, plotOutput("Plot4"))
  )
)


server <- function(input, output) {
  
  
  a <- 3
  b <- 0.87
  c <- 0.5
  
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
  
  
  
  
  df <- reactive({
    
    toy_data <- NULL
    test_data <- NULL
    
    if(input$dataset == "Data set 1")
    {
      set.seed(2024) #change to 2024
      
      #  Data set 1
      x <- runif(n = input$num_ob, min = 20, max = 40)
      fx <- a + (b * x)
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      # Generate test data
      x_test <- runif(n = 1000, min = 20, max = 40)
      fx_test <- a + (b * x_test)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    }
    
    else if(input$dataset == "Data set 2") 
    {
      set.seed(2024)
      
      # Data set 2
      x <- runif(n = input$num_ob, min = 20, max = 40)
      fx <- a + (b * sqrt(x)) + (c * sin(x))
      toy_data <- generate_data(x, fx, input$epsilon)
      # Generate test data
      set.seed(2025)
      
      x_test <- runif(n = 1000, min =20, max = 40)
      fx_test <- a + (b * sqrt(x_test)) + (c * sin(x_test))
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
      
      
    } else if(input$dataset == "Data set 3"){
      
      set.seed(2024)
      
      # Data set 3
      x <- runif(n = input$num_ob, min = -5, max = 5)
      fx <- a + (b * x^2) + (c * x^3)
      toy_data <- generate_data(x, fx, input$epsilon)  
      # Generate test data
      set.seed(2025)
      
      x_test <- runif(n = 1000, min = -5, max = 5)
      fx_test <- a + (b * x_test^2) + (c * x_test^3)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
      
      
    }
    
    return(list(toy_data = toy_data, test_data = test_data))
  })
  
  
  
 
  
  
  
 
  

  
  library(shiny)
  library(ggplot2)
  library(FNN)
  
  output$Plot1 <- renderPlot({
    df_data <- df()$toy_data
    
    if (input$dataset == "Data set 1") {
      p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
        geom_point() +
        labs(title = "Training Data", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      } else if (input$model_name == "KNN") {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
        df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
        p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1)
      }
      print(p)
    }
    
    else if (input$dataset == "Data set 2") {
      p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
        geom_point() +
        labs(title = "Training Data", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      } else if (input$model_name == "KNN") {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
        df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
        p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1)
      }
      print(p)
    }
    
    else if (input$dataset == "Data set 3") {
      p <- ggplot(data = df_data, aes(x = inp, y = response1)) + 
        geom_point() +
        labs(title = "Training Data", y = "y", x = "x") +
        scale_y_continuous(limits = c(-30, 30)) +
        scale_x_continuous(limits = c(-6, 6))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1/input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      } else if (input$model_name == "KNN") {
        knn_fit <- knn.reg(train = as.matrix(df_data$inp), test = as.matrix(df_data$inp), y = df_data$response1, k = input$k_value)
        df_knn <- data.frame(inp = df_data$inp, response1 = knn_fit$pred)
        p <- p + geom_step(data = df_knn, aes(x = inp, y = response1), size = 1)
      }
      print(p)
    }
    
  })
  
  
  


  
  
  
  
  
  
 
  output$Plot2 <- renderPlot({
    
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
    if (input$model_name == "Linear") {
      complexities <- 1  # Linear model has no complexity parameter
    } else if (input$model_name == "Non-Linear") {
      complexities <- seq(0.1, 8, by = 1)
    } else if (input$model_name == "Polynomial") {
      complexities <- 1:8
    } else if (input$model_name == "KNN") {
      complexities <- 3:20  # Use k values for KNN
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (input$model_name == "Linear") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (input$model_name == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (input$model_name == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (input$model_name == "KNN") {
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
        x = ifelse(input$model_name == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = ""  # Empty string for no y-axis label
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_bias)
  })
  
  
  output$Plot3 <- renderPlot({
    
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
    if (input$model_name == "Linear") {
      complexities <- 1  # Linear model has no complexity parameter
    } else if (input$model_name == "Non-Linear") {
      complexities <- seq(0.1, 8, by = 1)
    } else if (input$model_name == "Polynomial") {
      complexities <- 1:8
    } else if (input$model_name == "KNN") {
      complexities <- 3:20  # Use k values for KNN
    }
    
    # Loop over each complexity/k value
    for (complexity in complexities) {
      # Store predictions for each complexity/k value
      predictions <- matrix(nrow = nrow(df_testdata), ncol = 20)
      
      # Fit models and obtain predictions
      if (input$model_name == "Linear") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ inp, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (input$model_name == "Non-Linear") {
        for (i in 1:20) {
          model <- loess(df_data[, i + 2] ~ inp, span = 1/complexity, data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (input$model_name == "Polynomial") {
        for (i in 1:20) {
          model <- lm(df_data[, i + 2] ~ poly(inp, complexity), data = df_data)
          predictions[, i] <- predict(model, newdata = data.frame(inp = df_testdata$inp))
        }
      } else if (input$model_name == "KNN") {
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
        x = ifelse(input$model_name == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = ""  # Empty string for no y-axis label
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "none")  # Remove legend
    
    print(plot_variance)
  })
  

    
  output$Plot4 <- renderPlot({
    
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    # Define complexities or k values
    complexities <- switch(input$model_name,
                           "Non-Linear" = seq(0.1, 8, by = 1),
                           "Polynomial" = 1:8,
                           "KNN" = 3:20)  # Add k values for KNN
    
    for (complexity in complexities) {
      predictions <- matrix(nrow = 1000, ncol = 20)
      training_mse_list <- numeric(20)
      
      for (i in 1:20) {
        response_col <- paste0("response", i)
        
        if (input$model_name == "Linear") {
          model <- lm(df_data[[response_col]] ~ inp, data = df_data)
        } else if (input$model_name == "Non-Linear") {
          model <- loess(df_data[[response_col]] ~ inp, span = 1/complexity, data = df_data)
        } else if (input$model_name == "Polynomial") {
          model <- lm(df_data[[response_col]] ~ poly(inp, complexity), data = df_data)
        } else if (input$model_name == "KNN") {
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
        x = ifelse(input$model_name == "KNN", "k Value", "Complexity"),  # Adjust x-axis label
        y = "Mean Squared Error"
      ) +
      theme_minimal() +
      scale_color_manual(values = c("test MSE" = "blue", "training MSE" = "green")) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(plot_mse)
  })
  
  
      
      
      
      
      
  
  

  
  
}

# Run the application
shinyApp(ui = ui, server = server)



