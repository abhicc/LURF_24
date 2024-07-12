set.seed(2024)

generate_data <- function(x, fx, epsilon, num_responses = 200) {
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

# Define parameters
a <- 1
b <- 2
c <- 3
epsilon <- 1.0  # example standard deviation for the error term

# Example for Data set 1
x <- runif(n = 100, min = 20, max = 40)
fx <- a + (b * x)
toy_data1 <- generate_data(x, fx, epsilon)
# Generate test data
x_test <- runif(n = 1000, min = 20, max = 40)
fx_test <- a + (b * x_test)
test_data <- generate_test_data(x_test, fx_test, epsilon)

# Example for Data set 2
x <- runif(n = 100, min = 20, max = 40)
fx <- a + (b * sqrt(x)) + (c * sin(x))
toy_data2 <- generate_data(x, fx, epsilon)

# Example for Data set 3
x <- runif(n = 100, min = -5, max = 5)
fx <- a + (b * x^2) + (c * x^3)
toy_data3 <- generate_data(x, fx, epsilon)



