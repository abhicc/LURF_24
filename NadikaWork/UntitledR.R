library(tidyverse)
library(shiny)

# Let users choose the model
ui <- fluidPage(
  
  # Application title
  titlePanel("Model Bias and Variance"),
  
  fluidRow(
    column(2,
           selectInput(inputId = "dataset",
                       label = "Select a dataset",
                       choices = c("Data set 1", "Data set 2","Data set 3"),
                       selected = "Linear")),
    column(2,
           selectInput(inputId = "model_name",
                       label = "Select model",
                       choices = c("Linear", "Non-Linear","Polynomial"),
                       selected = "Linear")),
    column(2,
           sliderInput(inputId = "epsilon",
                       label = "Variability",
                       min = 0,
                       max = 8,
                       value = 1,
                       step = 0.5)),
    column(2,
           conditionalPanel(
             condition = "input.model_name == 'Non-Linear'",
             sliderInput(inputId = "flex",
                         label = "Flexibility",
                         min = 0.1,
                         max = 10,
                         value = 1,
                         step = 1)
           )),
    column(2,
           conditionalPanel(
             condition = "input.model_name == 'Polynomial'",
             sliderInput(inputId = "degree",
                         label = "Polynomial degree",
                         min = 1,
                         max = 10,
                         value = 1,
                         step = 1)
           )),
    column(2, plotOutput("myLegend"))
  ),
  
  fluidRow(
    column(4, plotOutput("Plot1")),
    column(4, plotOutput("Plot2")),
    column(4, plotOutput("Plot3")),
    column(4, plotOutput("Plot4"))
  )
)

server <- function(input, output) {
  
  a <- 3
  b <- 0.87
  c <- 0.5
  
  generate_data <- function(x, fx, epsilon, num_responses = 100) {
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
    
    if (input$dataset == "Data set 1") {
      set.seed(2024)
      x <- runif(n = 100, min = 20, max = 40)
      fx <- a + (b * x)
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      x_test <- runif(n = 1000, min = 20, max = 40)
      fx_test <- a + (b * x_test)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    } else if (input$dataset == "Data set 2") {
      set.seed(2024)
      x <- runif(n = 100, min = 20, max = 40)
      fx <- a + (b * sqrt(x)) + (c * sin(x))
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      x_test <- runif(n = 1000, min = 20, max = 40)
      fx_test <- a + (b * sqrt(x_test)) + (c * sin(x_test))
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    } else if (input$dataset == "Data set 3") {
      set.seed(2024)
      x <- runif(n = 100, min = -5, max = 5)
      fx <- a + (b * x^2) + (c * x^3)
      toy_data <- generate_data(x, fx, input$epsilon)
      set.seed(2025)
      x_test <- runif(n = 1000, min = -5, max = 5)
      fx_test <- a + (b * x_test^2) + (c * x_test^3)
      test_data <- generate_test_data(x_test, fx_test, input$epsilon)
    }
    
    return(list(toy_data = toy_data, test_data = test_data))
  })
  
  output$Plot1 <- renderPlot({
    if (input$dataset == "Data set 1") {
      p <- ggplot(data = df()$toy_data, aes(x = inp, y = response1)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Scatter plot x vs. y", y = "y", x = "x") +
        scale_y_continuous(limits = c(15, 40)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1 / input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    } else if (input$dataset == "Data set 2") {
      p <- ggplot(data = df()$toy_data, aes(x = inp, y = response1)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 1", y = "y", x = "x") +
        scale_y_continuous(limits = c(3, 13)) +
        scale_x_continuous(limits = c(20, 40))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1 / input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    } else if (input$dataset == "Data set 3") {
      p <- ggplot(data = df()$toy_data, aes(x = inp, y = response1)) + 
        geom_point() +
        scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
        labs(title = "Sample 1", y = "y", x = "x") +
        scale_y_continuous(limits = c(-30, 30)) +
        scale_x_continuous(limits = c(-6, 6))
      
      if (input$model_name == "Linear") {
        p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = FALSE)
      } else if (input$model_name == "Non-Linear") {
        p <- p + geom_smooth(span = 1 / input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = FALSE)
      } else if (input$model_name == "Polynomial") {
        p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = FALSE)
      }
      print(p)
    }
  })
  
  calculate_rmse <- function(true, predicted) {
    sqrt(mean((true - predicted)^2, na.rm = TRUE))
  }
  
  output$Plot4 <- renderPlot({
    df_data <- df()$toy_data
    df_testdata <- df()$test_data
    
    metrics <- data.frame(
      Complexity = numeric(),
      Metric = character(),
      Value = numeric()
    )
    
    if (input$model_name == "Linear") {
      complexities <- 1
    } else if (input$model_name == "Non-Linear") {
      complexities <- seq(1, 10, by = 1)
    } else if (input$model_name == "Polynomial") {
      complexities <- seq(1, 10, by = 1)
    }
    
    for (complex in complexities) {
      train_preds <- numeric()
      test_preds <- numeric()
      
      for (i in 3:ncol(df_data)) {
        sample_data <- df_data[, c(1, i)]
        colnames(sample_data) <- c("inp", "observed")
        
        if (input$model_name == "Linear") {
          model <- lm(observed ~ inp, data = sample_data)
        } else if (input$model_name == "Non-Linear") {
          model <- loess(observed ~ inp, data = sample_data, span = 1 / complex)
        } else if (input$model_name == "Polynomial") {
          model <- lm(observed ~ poly(inp, complex), data = sample_data)
        }
        
        train_preds <- cbind(train_preds, predict(model, newdata = sample_data))
        test_preds <- cbind(test_preds, predict(model, newdata = df_testdata))
      }
      
      train_rmse <- calculate_rmse(df_data$true_form, rowMeans(train_preds, na.rm = TRUE))
      test_rmse <- calculate_rmse(df_testdata$true_form, rowMeans(test_preds, na.rm = TRUE))
      
      metrics <- rbind(metrics, data.frame(Complexity = complex, Metric = "Train RMSE", Value = train_rmse))
      metrics <- rbind(metrics, data.frame(Complexity = complex, Metric = "Test RMSE", Value = test_rmse))
    }
    
    ggplot(metrics, aes(x = Complexity, y = Value, color = Metric)) +
      geom_line() +
      geom_point() +
      labs(title = "RMSE vs Model Complexity", x = "Complexity", y = "RMSE") +
      theme_minimal() +
      theme(legend.title = element_blank())
  })
  
  output$myLegend <- renderPlot({
    df_data <- df()$toy_data
    
    sample_data <- df_data[, c(1, 3)]
    colnames(sample_data) <- c("inp", "observed")
    
    p <- ggplot(sample_data, aes(x = inp, y = observed)) + 
      geom_point() +
      scale_color_manual(values = c("linear model" = "blue", "non-linear model" = "green", "polynomial model" = "red")) +
      labs(title = "Scatter plot x vs. y", y = "y", x = "x") +
      scale_y_continuous(limits = c(min(df_data$true_form), max(df_data$true_form))) +
      scale_x_continuous(limits = c(min(df_data$inp), max(df_data$inp)))
    
    if (input$model_name == "Linear") {
      p <- p + geom_smooth(method = "lm", se = FALSE, aes(color = "linear model"), show.legend = TRUE)
    } else if (input$model_name == "Non-Linear") {
      p <- p + geom_smooth(span = 1 / input$flex, se = FALSE, aes(color = "non-linear model"), show.legend = TRUE)
    } else if (input$model_name == "Polynomial") {
      p <- p + geom_smooth(method = "lm", formula = y ~ poly(x, input$degree), se = FALSE, aes(color = "polynomial model"), show.legend = TRUE)
    }
    
    print(p)
  })
}

shinyApp(ui = ui, server = server)







