#random walk in 1D


walk_1D <- function(n){
  X = 0
  for (i in 1:n){
    num <- runif(1, min = 0, max = 1) 
    if (num > 0.5){
      X <- c(X,X[i-1]+1)
    }else{
      X <- c(X,X[i-1]-1)
    }
  }
  return(X)
}


n = 100
walk <- walk_1D(n)
x <- seq(1,n,1)
plot(x,walk)



