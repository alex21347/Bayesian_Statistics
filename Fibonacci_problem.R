#Fibonacci problem


fibonacci <- function(n){
  
  X = c(1,1)
  
  for (i in 1:10)
    X = c(X,X[i-2]+X[i-1])
  
  return(X)
  
}

Y = fibonacci(5)







