#gibbs sampler - computer lab 2 

tau_0_2 = 0.2
mu_0 = 1.9
a = 5
b = 1
n = 9

mus = NULL
sigmas2 = NULL

y = c(1.64, 1.70, 1.72, 1.74, 1.82, 1.82, 1.82, 1.90, 2.08)

sum_y = sum(y)


#initialisation

mus = c(mus,mu_0)
sigmas2 = c(sigmas2, tau_0_2)

T = 1000

for (i in 1:T){
  
  arg1 = ((sum_y/sigmas2[i])+(mu_0/tau_0_2))/((n/sigmas2[i])+(1/tau_0_2))
  arg2 = 1/((n/sigmas2[i])+(1/tau_0_2))
  
  next_mu = rnorm(1, mean = arg1, sd = arg2)
  
  arg3 = a + n/2
  
  sum_temp = 0
  for (j in 1:n){
    sum_temp = sum_temp + (y[j]-next_mu)^2}
  
  arg4 = b + (1/2)*sum_temp
  
  next_sigma2 = rgamma(1, shape = arg3, rate = arg4)
  
  mus = c(mus, next_mu)
  sigmas2 = c(sigmas2, 1/next_sigma2)
  
  
}


sigmas2
mus

sigma2_estimate = mean(sigmas2)
mu_estimate = mean(mus)

sigma2_variance = var(sigmas2)
mu_variance = var(mus)


c(quantile(mus,0.025),quantile(mus,0.975))

c(quantile(sigmas2,0.025),quantile(sigmas2,0.975))

