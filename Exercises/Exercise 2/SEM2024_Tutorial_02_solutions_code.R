# ############################################################################# #
#                                                                               #
#### Code Problem Set 2                                                      ####
#                                                                               #
# ############################################################################# #



# ############################################################################# #
##### Problem 2.2 Estimator Properties                                      #####
# ############################################################################# #

### We want to visualise the results of Problem 2.1 using R. This can be done by following the steps:

###### (a)   --------------------------------------------------------------------
### Draw N=1000 (pseudo-)random realisations of a normally distributed random variable with mean mu=5 and variance sigma^2=4. 
### This can be done in R using the function rnorm

N               <- 10^3
mu              <- 5
sigma_2         <- 4

x               <- rnorm(n = N, mean = mu, sd = sqrt(sigma_2))




###### (b)   --------------------------------------------------------------------
### Calculate the sample mean of the drawn realisations.

mean(x)




###### (c)   --------------------------------------------------------------------
### Repeat step (a) and (b) M=1000 times. This can be done using a for-loop. 
### For each time calculate the mean of the draws and store it in a vector. 
### This way you obtain M=1000 estimates of the mean of the distribution from which the samples are drawn.

M               <- 10^3
estimates_mean  <- numeric(M)

for(i in 1:M){
  set.seed(i)           ### set a seed to make your code reproducible 
  x                 <- rnorm(n = N, mean = mu, sd = sqrt(sigma_2))
  estimates_mean[i] <- mean(x)
}




###### (d)   --------------------------------------------------------------------
### Calculate the mean and variance of the estimates. Compare the results with the estimator properties from Problem 2.1.

mean(estimates_mean)
var(estimates_mean)

### the mean of the estimates should be approximately equal to the true mean mu
### the variance of the estimates should be approximately equal to the variance of the random variable divided by the sample size, so sigma^2/N
matrix(data = c(mean(estimates_mean), var(estimates_mean), 
                mu, sigma_2/N,
                (mean(estimates_mean)-mu), var(estimates_mean)-sigma_2/N),
       nrow = 3, byrow = TRUE,
       dimnames = list(c("estimate", "theory", "difference"), c("mean","variance"))
)




###### (e)   --------------------------------------------------------------------
### Plot a histogram of the results using the function hist and compare the shape with a histogram of (pseudo)-random draws from a normal distribution with appropriate mean and variance.

hist(x = estimates_mean, breaks = 25, freq = FALSE)
hist(rnorm(n = M, mean = mu, sd = sqrt(sigma_2/N)), breaks = 25, freq = FALSE, col = "red", add = TRUE)

### alternatively: side by side:
par(mfrow=c(1,2))
hist(x = estimates_mean, breaks = 25, freq = FALSE)
hist(rnorm(n = M, mean = mu, sd = sqrt(sigma_2/N)), breaks = 25, freq = FALSE, col = "red")
par(mfrow=c(1,1))

### alternatively we can plot the density of the normal distribution over the histogram
hist(x = estimates_mean, breaks = 25, freq = FALSE)
x_fit <- seq(from = min(estimates_mean), to = max(estimates_mean), length = 100) 
y_true <- dnorm(x = x_fit, mean = mu, sd = sqrt(sigma_2/N))
lines(x_fit, y_true, col = "red")




### This can also be done for random variables that follow a different distribution than the one given in (a), 
### showing that the properties of the sample mean as an estimator for the expected value does not depend on the distribution of x.




###### (f)   --------------------------------------------------------------------
### Try out the same steps (a) to (e) with different distributions. 
### You can first variate the mean and variance of the normal distribution. 
### You should also try out different types of distributions. 
### Use the help function ?Distributions for some inspiration. You may also vary N and M.

N <- 10^3
M <- 10^3

{
  ### draw
  estimates_mean  <- numeric(M)
  for(i in 1:M){
    ### normal distribution
    x_draws <- rnorm(n = N, mean = -7, sd = 8)
    
    ### uniform distribution
    #x_draws <- runif(n = N, min = 3, max = 9)
    
    ### poisson distribution
    #x_draws <- rpois(n = N, lambda = 3)
    
    ### t distribution
    #x_draws <- rt(n = N, df = 2)
    
    ### cauchy distribution
    #x_draws <- rcauchy(n = N, location = 3, scale = .05)
    
    estimates_mean[i] <- mean(x_draws)
  }
  
  ### plot histograms
  par(mfrow=c(1,2))
  hist(x = x_draws, 
       breaks = 25,
       freq = FALSE,
       main = "Hist. of one sample",
       xlab = "draws")
  
  hist(x = estimates_mean, 
       breaks = 25, 
       freq = FALSE,
       main = "Hist. of sample means",
       xlab = "means")
  x_fit <- seq(min(estimates_mean), max(estimates_mean), length = 100) 
  y_norm <- dnorm(x_fit, mean = mean(estimates_mean), sd = sd(estimates_mean))
  lines(x_fit, y_norm, col = "red")
  par(mfrow=c(1,1))
  
  print(
    matrix(data = c(mean(estimates_mean), var(estimates_mean)),
           nrow = 2,
           dimnames = list(c("mean","variance"),c("sample estimate"))
    )
  )
}




###### (g)   --------------------------------------------------------------------
### Can you find a distribution for which the histogram of the distribution of the estimates of the mean (plot from part (e)) does not represent approximately a normal distribution?
### Why is this the case? Does this contradict the previous results from Problem 2.1?

### If we draw from a Cauchy Distribution, the histogram does not represent a normal distribution.
### The variance of the estimates is also not stable and varies, if we repeat the process several times.
### This is the case because mean and variance of the Cauchy distribution is undefined and hence the Central Limit Theorem does not apply.




###### (h)   --------------------------------------------------------------------
### Which theorem formalizes the results that we can observe here?

### The Central Limit Theorem



