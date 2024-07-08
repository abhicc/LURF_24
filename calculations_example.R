set.seed(2024) #change to 2024

# simulate data
x <- runif(n = 100, min = 20, max = 40)   # input/predictor

e1 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e2 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e3 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e4 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e5 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e6 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e7 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e8 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e9 <- rnorm(n = 100, mean = 0, sd = 2)  # error
e10 <- rnorm(n = 100, mean = 0, sd = 2)  # error

a <- 3
b <- 0.87
fx <- a + (b * x)  # true function

y1 <- fx + e1    # observed responses
y2 <- fx + e2    # observed responses
y3 <- fx + e3    # observed responses
y4 <- fx + e4    # observed responses
y5 <- fx + e5    # observed responses
y6 <- fx + e6    # observed responses
y7 <- fx + e7    # observed responses
y8 <- fx + e8    # observed responses
y9 <- fx + e9    # observed responses
y10 <- fx + e10    # observed responses

toy_data <- data.frame(inp = x, true_form = fx, response1 = y1, response2 = y2, response3 = y3, response4 = y4, response5 = y5,
                       response6 = y6, response7 = y7, response8 = y8, response9 = y9, response10=y10)

ggplot(data = toy_data) + geom_point(aes(x = inp, y = response1))   # one of the data plots for the shiny app

d = 1   # linear fit

m1 <- lm(response1 ~ poly(inp, degree = d, raw = TRUE), data = toy_data)   # model for the first replicated dataset
y_hat_1 <- predict(m1, newdata = data.frame(inp = toy_data$inp))
# y_hat_1 <- predict(m1, newdata = data.frame(inp = test_dat$inp))

# replicate the above process for 10 times

m2 <- lm(response2 ~ poly(inp, degree = d, raw = TRUE), data = toy_data)   # model for the first replicated dataset
y_hat_2 <- predict(m2, newdata = data.frame(inp = toy_data$inp))

df_preds <- data.frame(y_hat_1 = y_hat_1, y_hat_2 = y_hat_2)    # you should have ten columns instead of 2


# bias squared calculation
# 1
E_y_hat <- apply(df_preds, 1, mean)
# 2 and 3
bias_squared <- (E_y_hat - toy_data$true_form)^2
# 4
mean(bias_squared)


# variance calculation
# 1
V_y_hat <- apply(df_preds, 1, var)
# 2
mean(V_y_hat)


bias_var_df <- data.frame(degere = 1:3, bias_sq = mean(bias_squared), variance = mean(V_y_hat) )

# create a dataset/matrix of each set y values (dimension - number of data points n x number of replications 10)







