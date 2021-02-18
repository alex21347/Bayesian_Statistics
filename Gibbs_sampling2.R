data = c(1.64, 1.70, 1.72, 1.74, 1.82 , 1.82, 1.82, 1.90, 2.08)

mu0  = 1.9
tau0 = 0.2
a    = 5
b    = 1
n = 9

# INITIALISATION
MU_VEC = rep(0,10000-1)
VAR_VEC = rep(0,10000-1)

MU_VEC[1]  <- 1.9
VAR_VEC[1] <- 0.2

##############################################################

for (t in 2:10000){
  
  # STEP 1 Re-sample the mean - mu
  
  x_Sigma2 <- VAR_VEC[t-1]
  
  sigma_star <- 1/(n/(x_Sigma2) + 1/(tau0))
  
  mu_star    <- sigma_star * (sum(data)/x_Sigma2 + mu0/tau0)
  
  MU_VEC[t] <- rnorm(1,mu_star,sqrt(sigma_star))
  
  ##########################
  # STEP 2 Re-sample the variance - sigma2
  
  a_star <- a + length(data)/2 
  b_star <- b + 1/2 * sum((data - MU_VEC[t])^2) 
  
  y_Sigma2_inv <- rgamma(1,a_star,b_star)
  
  VAR_VEC[t] <- 1/y_Sigma2_inv
  
} # end of iterations


##############################################################

##### SOLUTION PART (b)

mean(MU_VEC)

mean(VAR_VEC)

var(MU_VEC)

var(VAR_VEC)