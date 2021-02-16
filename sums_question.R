#sum question

sum1 <- function(n1,n2){
  result = 0
  for (i in 1:n1){
    for (j in 1:n2){
      result = result + i^4/(3+j)
    }
  }
  
  return(result)
}

Y = sum1(10,10)
Y

sum2 <- function(n){
  result = 0
  for (i in 1:n){
    for (j in 1:i){
      result = result + i^4/(3+i*j)
    }
  }
  
  return(result)
}

Y = sum1(10,10)
Y

Y = sum2(10)
Y

