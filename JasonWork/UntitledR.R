library(mvtnorm)

num_training_points <- 200   # LET'S KEEP THIS FIXED FOR NOW     
num_test_points <- 600      
num_rep_sets <- 100    
noise <- 0.1          # USER SHOULD BE ABLE TO CHANGE THIS ON THE APP (SAY 0.1, 0.5, 0.9)  


# GENERATING BIVARIATE NORMAL META-MEANS AND 10 COMPONENT MEANS FOR EACH CLASS
mu_class0 <- c(1, -1)   # OR (0.5, -0.5)
mu_class1 <- c(-1, 1)   # OR (-0.5, 0.5)
cov_mat <- matrix(c(1,0,0,1), nrow=2, ncol=2)
means_class0 <- rmvnorm(n = 10, mean = mu_class0, sigma = cov_mat)
means_class1 <- rmvnorm(n = 10, mean = mu_class1, sigma = cov_mat)


# ONE TRAINING DATASET (USE SIMILAR CODE TO GENERATE TEST DATASET)
training_obs_class0 <- matrix(nrow=num_training_points/2, ncol=2)
training_obs_class1 <- matrix(nrow=num_training_points/2, ncol=2)
for(i in 1:num_training_points/2)
{
  s <- sample(1:10, size = 1, prob = rep(1/10, 10))
  training_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                     sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
  
  s <- sample(1:10, size = 1, prob = rep(1/10, 10))
  training_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                     sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
}
df_training <- data.frame(rbind(training_obs_class0, training_obs_class1), 
                          y_orig = factor(rep(c("0", "1"),each=num_training_points/2), 
                                          levels =c("0", "1")))
names(df_training) <- c("x1", "x2", "y_orig")

ggplot(data = df_training) + geom_point(aes(x = x1, y = x2, color = y_orig))


# REPLICATED TRAINING DATASETS
x1_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
x2_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)
y_matrix <- matrix(nrow = num_training_points, ncol = num_rep_sets)

for(j in 1:num_rep_sets)
{
  
  rep_training_obs_class0 <- matrix(nrow=num_training_points/2, ncol=2)
  rep_training_obs_class1 <- matrix(nrow=num_training_points/2, ncol=2)
  for(i in 1:num_training_points/2)
  {
    s <- sample(1:10, size = 1, prob = rep(1/10, 10))
    rep_training_obs_class0[i,] <- rmvnorm(n = 1, mean = means_class0[s,],
                                           sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
    
    s <- sample(1:10, size = 1, prob = rep(1/10, 10))
    rep_training_obs_class1[i,] <- rmvnorm(n = 1, mean = means_class1[s,],
                                           sigma = matrix(c(noise,0,0,noise), nrow=2, ncol=2))
  }
  x1_matrix[, j] <- c(rep_training_obs_class0[,1], rep_training_obs_class1[,1])
  x2_matrix[, j] <- c(rep_training_obs_class0[,2], rep_training_obs_class1[,2])
  y_matrix[, j] <- rep(c("0", "1"), each=num_training_points/2)
  
}

