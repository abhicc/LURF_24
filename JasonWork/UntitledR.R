library(keras)
library(ggplot2)
library(dplyr)

# Simulated training and test data
set.seed(123)
library(mvtnorm)

num_training_points <- 200
num_test_points <- 500
noise <- 0.1
num_rep_sets <- 1

mu_class0 <- c(1, -1)
mu_class1 <- c(-1, 1)
cov_mat <- matrix(c(1,0,0,1), nrow=2, ncol=2)
means_class0 <- rmvnorm(n = 10, mean = mu_class0, sigma = cov_mat)
means_class1 <- rmvnorm(n = 10, mean = mu_class1, sigma = cov_mat)

# Generating test data
test_obs_class0 <- matrix(nrow=num_test_points/2, ncol=2)
test_obs_class1 <- matrix(nrow=num_test_points/2, ncol=2)
for(i in 1:(num_test_points/2)) {
  s <- sample(1:10, size = 1, prob = rep(1/10, 10))
  test_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,], sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
  
  s <- sample(1:10, size = 1, prob = rep(1/10, 10))
  test_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,], sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
}
test_data <- data.frame(rbind(test_obs_class0, test_obs_class1), y = factor(rep(c(0, 1), each=num_test_points/2)))

# Generating training data
x1_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
x2_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
y_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
for(j in 1:num_rep_sets) {
  rep_training_obs_class0 <- matrix(nrow=num_training_points/2, ncol=2)
  rep_training_obs_class1 <- matrix(nrow=num_training_points/2, ncol=2)
  for(i in 1:(num_training_points/2)) {
    s <- sample(1:10, size = 1, prob = rep(1/10, 10))
    rep_training_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,], sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
    
    s <- sample(1:10, size = 1, prob = rep(1/10, 10))
    rep_training_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,], sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
  }
  x1_matrix[, j] <- c(rep_training_obs_class0[,1], rep_training_obs_class1[,1])
  x2_matrix[, j] <- c(rep_training_obs_class0[,2], rep_training_obs_class1[,2])
  y_matrix[, j] <- rep(c(0, 1), each=num_training_points/2)
}
training_data <- data.frame(x1 = x1_matrix[,1], x2 = x2_matrix[,1], y = as.factor(y_matrix[,1]))

# Build the neural network
model <- keras_model_sequential() %>%
  layer_dense(units = 5, activation = 'relu', input_shape = c(2)) %>%
  layer_dense(units = 1, activation = 'sigmoid')

model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = 'accuracy'
)

# Train the model
model %>% fit(
  as.matrix(training_data[, c("x1", "x2")]), 
  as.numeric(training_data$y) - 1,
  epochs = 20,
  batch_size = 32
)

# Predict and visualize
test_data$pred <- model %>% predict_classes(as.matrix(test_data[, c("x1", "x2")])) %>% as.factor()

ggplot(test_data, aes(x = x1, y = x2, color = pred)) +
  geom_point() +
  labs(title = "Decision Boundary of Neural Network") +
  scale_color_manual(values = c("blue", "red")) +
  theme_minimal() +
  geom_contour(data = as.data.frame(predict(model, as.matrix(test_data[, c("x1", "x2")]))), 
               aes(z = V1, color = "..level.."), binwidth = 0.1, size = 1, linetype = "dashed") +
  scale_color_gradient(low = "white", high = "black")
