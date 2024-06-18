library(tidyverse)
library(caret)
library(shiny)

ui <- fluidPage(
  
  # Application title
  titlePanel("K-Nearest Neighbors"),
  
  sidebarLayout(
    sidebarPanel(
    
    # column(3,
           # selectInput(inputId = "true_model",
           #             label = "Select a 'true' model",
           #             choices = c("Linear", "Non-linear"),
           #             selected = "Linear"),
    
    # column(3,
           # sliderInput(inputId = "epsilon",
           #             label = "Select variability:",
           #             min = 0,
           #             max = 5,
           #             value = 1,
           #             step = 0.5),
           # 
    # column(3,
           sliderInput(inputId = "k",
                       label = "Select a value of K:",
                       min = 1,
                       max = 97,
                       value = 10,
                       step = 1,
                       animate = TRUE)),
    
    # column(3, 
           # plotOutput("myLegend"))
    
  
  
  mainPanel(
    # column(4, 
           # plotOutput("truePlot"),
    # column(4,
           plotOutput("observedPlot")
  ))
)


server <- function(input, output) {
  
  output$observedPlot <- renderPlot({
    
    outlets <- readRDS("outlets.rds")   # load dataset
    
    kval <- as.numeric(input$k)
    
    knnfit <- knnreg(profit ~ population, data = outlets, k = kval)
    
    # predict(object = __________, newdata = data.frame(population = __________))  # 1-nn prediction
    # 
    # slrfit <- lm(profit ~ population, data = outlets)   # fit the SLR model
    
    pop_seq <- seq(min(outlets$population, na.rm = TRUE), max(outlets$population, na.rm = TRUE), 0.01)
    
    # obtain predictions for all training data points
    # knn_1 <- predict(knnfit1, newdata = data.frame(population = min(outlets$population, na.rm = TRUE):max(outlets$population, na.rm = TRUE)))
    # knn_1 <- predict(knnfit1, newdata = data.frame(population = pop_seq))
    preds <- predict(knnfit, newdata = data.frame(population = pop_seq))
    # knn_5 <- predict(knnfit5, newdata = data.frame(population = min(outlets$population, na.rm = TRUE):max(outlets$population, na.rm = TRUE)))
    # knn_5 <- predict(knnfit5, newdata = data.frame(population = pop_seq))
    # p <-  predict(slrfit, newdata = data.frame(population = min(outlets$population, na.rm = TRUE):max(outlets$population, na.rm = TRUE)))
    # p <-  predict(slrfit, newdata = data.frame(population = pop_seq))
    
    # column bind original data with predicted values
    # ames1 <- outlets %>% select(profit, population) %>% filter(!is.na(population))
    # predictions <- data.frame(population = min(outlets$population, na.rm = TRUE):max(outlets$population, na.rm = TRUE),
    # linear = p, knn_1, knn_5)
    # predictions <- data.frame(population = pop_seq,
                              # linear = p, knn_1, knn_5)
    predictions <- data.frame(population = pop_seq, predictions = preds)
    
    # plot the three models
    # ggplot(data = outlets, aes(x = population, y = profit)) +
    #   geom_point() +
    #   geom_line(data = predictions, aes(x = population, y = knn_1), color = "cyan", linetype = "dashed", linewidth = 1) +   # 1-nn regression
    #   geom_line(data = predictions, aes(x = population, y = knn_5), color = "red", linetype = "dotted", linewidth = 1) +   # 5-nn regression
    #   geom_line(data = predictions, aes(x = population, y = linear), color = "blue", linewidth = 1) +
    #   theme_bw()
    
    ggplot(data = outlets, aes(x = population, y = profit)) +
      geom_point() +
      geom_line(data = predictions, aes(x = population, y = predictions), color = "red", linetype = "dashed", linewidth = 1) +   # 1-nn regression
      # geom_line(data = predictions, aes(x = population, y = knn_5), color = "red", linetype = "dotted", linewidth = 1) +   # 5-nn regression
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      theme_bw()
    
  })
  
  

  
  
}


# Run the application
shinyApp(ui = ui, server = server)
