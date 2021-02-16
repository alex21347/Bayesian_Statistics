#Computer Lab 3 Exercise 1

#exp-gamma model gives Gamma(a + n, b + sum(x)) as posterior

a = 1
b = 1
lambda = 1

lower_quantile = c(0,0,0)
upper_quantile = c(0,0,0)

for (i in 1:3){
  n = 10^i
  
  sample = rexp(n, rate = lambda)
  sample_sum = sum(sample)
  
  lower_quantile[i] = qgamma(0.05, shape = a + n, rate = b + sample_sum)
  upper_quantile[i] = qgamma(0.95, shape = a + n, rate = b + sample_sum)
}


mc_lower_quantile = c(0,0,0)
mc_upper_quantile = c(0,0,0)

for (i in 1:3){
  n = 10^i
  
  sample = rexp(n, rate = lambda)
  sample_sum = sum(sample)
  
  post_sample = rgamma(1000, shape = a + n, rate = b + sample_sum)
  
  mc_lower_quantile[i] = quantile(post_sample, 0.05)
  mc_upper_quantile[i] = quantile(post_sample, 0.95)
}

correct_val = c(1,1,1)

library(lattice)
xyplot(upper_quantile + lower_quantile + mc_upper_quantile + mc_lower_quantile + correct_val ~ c(10,100,1000), ylab = "quantiles", type = "l", auto.key = list(points = TRUE,lines = FALSE))

############################################################

#Computer Lab 3 Exercise 2

func <- function(x){
  y = x^2
  return(y)
}

n = 500
success = rep(0,n)
rolling_estimate = rep(0,n)

for (i in 1:n){
  rand_num_x = runif(1,min = 0, max = 2)
  rand_num_y = runif(1,min = 0, max = 4)
  
  if (rand_num_y < func(rand_num_x)){
    success[i] = 1
  }
  rolling_estimate[i] = 8*mean(success[1:i])
}
rolling_estimate

plot(seq(1,n,1),rolling_estimate,type="l")
abline(8/3,0, col = 'red', lty=2)

final_estimate = tail(rolling_estimate, n = 1)

############################################################

#Computer Lab 3 Exercise 3

func <- function(x){
  y = (1 - x^2)^0.5
  return(y)
}

n = 500
success = rep(0,n)
rolling_estimate = rep(0,n)

for (i in 1:n){
  rand_num_x = runif(1,min = 0, max = 1)
  rand_num_y = runif(1,min = 0, max = 1)
  print(func(rand_num_x))
  
  if (rand_num_y < func(rand_num_x)){
    success[i] = 1
  }
  rolling_estimate[i] = 4*mean(success[1:i])
}
rolling_estimate

plot(seq(1,n,1),rolling_estimate,type="l")
abline(3.1415926535,0, col = 'red', lty=2)

final_estimate = tail(rolling_estimate, n = 1)

############################################################

#Computer Lab 3 Exercise 4

n = 5000
success1 = rep(0,n)
rolling_estimate1 = rep(0,n)

success2 = rep(0,n)
rolling_estimate2 = rep(0,n)

for (i in 1:n){
  theta_1 = rgamma(1, shape = 219, rate = 112)
  theta_2 = rgamma(1, shape = 68, rate = 45)
  
  Y_1 = rpois(1, lambda = theta_1)
  Y_2 = rpois(1, lambda = theta_2)
  
  if (theta_1 > theta_2){
    success1[i] = 1
  }
  rolling_estimate1[i] = mean(success1[1:i])
  
  
  if (Y_1 > Y_2){
    success2[i] = 1
  }
  rolling_estimate2[i] = mean(success2[1:i])
}

plot(seq(1,n,1),rolling_estimate1,type="l")
abline(0.9716, 0, col = 'red', lty=2)

final_estimate1 = tail(rolling_estimate1, n = 1)


plot(seq(1,n,1),rolling_estimate2,type="l")
abline(0.4788, 0, col = 'red', lty=2)

final_estimate2 = tail(rolling_estimate2, n = 1)