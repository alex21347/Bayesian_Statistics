#normal dist


X = seq(-2,2,0.1)
Y = X

for (i in 1:41){
  Y[i] = dnorm(X[i], mean = 0, sd = 1)
  
}

plot(X,Y)