# ############################################################################ #
#                                                                              #
#### Code Problem Set 4                                                     ####
#                                                                              #
# ############################################################################ #




# ############################################################################ #
##### Problem 4.1                                                          #####
# ############################################################################ #


###### (b)   -------------------------------------------------------------------
### Draw 500 times from the data generating process (2). 
### For this, draw the values of the regressors randomly, such that x1 is drawn from a uniform distribution on the interval[1,3],
### x2 is drawn from a normal distribution with mean zero and standard deviation three, 
### and x3 is drawn from a normal distribution with mean zero and standard deviation one.
set.seed(2)

### number of draws
N           <- 500

### draw the regressors
x0          <- rep(1,N)
x1          <- runif(N,1,3)
x2          <- rnorm(N,0,3)
x3          <- rnorm(N,0,1)

### combine them in an X matrix, including the higher order terms
X           <- matrix(c(x0, x1, x2, x2^3, x3), ncol=5)

### define the parameters
sigma_2     <- .002
beta_true   <- c(3, 0.5, 7, 0.003, 0.001)

### draw the error terms from a normal distribution
u           <- rnorm(N, 0, sd = sqrt(sigma_2*x1))

### calculate y
y           <- X%*%beta_true + u




###### (c)   -------------------------------------------------------------------
### Estimate model (3) on the simulated data to obtain the estimates of the parameters and the standard deviations of the estimates.

model_1     <- lm(y ~ x1 + x2)
summary(model_1)




###### (d)   -------------------------------------------------------------------
### Repeat (b) and (c) 10000 times and save the estimates you obtain every time. 
### Compare the mean on the 10000 estimates with the true parameters. 
### Compare the standard deviation of the 10000 estimates of each parameter with the standard deviation of the estimators you calculated in (c).

N_rep           <- 10000

estimates_sim   <- matrix(0, nrow = N_rep, ncol = 3)
for(i in 1:N_rep){
  set.seed(i)
  x0_sim            <- rep(1, N)
  x1_sim            <- runif(N, 1, 3)
  x2_sim            <- rnorm(N, 0, 3)
  x3_sim            <- rnorm(N, 0, 1)
  X_sim             <- matrix(c(x0_sim, x1_sim, x2_sim, x2_sim^3, x3_sim), ncol = 5)
  
  u_sim             <- rnorm(N, 0, sqrt(sigma_2*x1_sim))
  
  y_sim             <- X_sim%*%beta_true + u_sim
  
  model_sim         <- lm(y_sim ~ x1_sim + x2_sim)
  estimates_sim[i,] <- model_sim$coefficients
}

### parameter estimates
rbind("true" = beta_true[1:3], 
      "estimated" = model_1$coefficients,
      "mean estimates" = c(mean(estimates_sim[,1]), mean(estimates_sim[,2]), mean(estimates_sim[,3])))

### standard deviations of the estimates
rbind(estimated = summary(model_1)$coefficients[,2], 
      simulated = c(sd(estimates_sim[,1]), sd(estimates_sim[,2]), sd(estimates_sim[,3])))




###### (e)   -------------------------------------------------------------------
### Perform a RESET-Test with p=3 on the model estimated in part (c). 
### Does it detect the misspecification of the model?

### save fitted values
y_hat       <- model_1$fitted.values

### calculate SSR_r
SSR_r       <- sum(model_1$residuals^2)

### model fit for RESET-test
model_reset <- lm(y ~ x1 + x2 + I(y_hat^2) + I(y_hat^3))
summary(model_reset)

### calculate SSR_ur
SSR_ur      <- sum(model_reset$residuals^2)

### RESET-test statistic
p           <- 3
k           <- 3
F_test      <- ((SSR_r-SSR_ur)/(p-1))/(SSR_ur/(N-k-p+1))
F_test

### critical value for 5% significance
qf(0.95, df1 = p-1, df2 = N-k-p+1)

### p-value
1-pf(F_test, df1 = p-1, df2 = N-k-p+1)
pf(F_test, df1 = p-1, df2 = N-k-p+1, lower.tail = FALSE)



###### (f)   -------------------------------------------------------------------
### Add now x_2^2 and x_2^3 to the model (3) and fit it to the data.

model_2     <- lm(y ~ x1+x2+I(x2^2)+I(x2^3))
summary(model_2)




###### (g)   -------------------------------------------------------------------
### Repeat (d) for this new model and save the standard deviations of the 10000 obtained estimates.

estimates_sim_2   <- matrix(0, nrow = N_rep, ncol = 5)
for(i in 1:N_rep){
  set.seed(i)
  x0_sim                <- rep(1, N)
  x1_sim                <- runif(N, 1, 3)
  x2_sim                <- rnorm(N, 0, 3)
  x3_sim                <- rnorm(N, 0, 1)
  X_sim                 <- matrix(c(x0_sim, x1_sim, x2_sim, x2_sim^3, x3_sim), ncol = 5)
  
  u_sim                 <- rnorm(N, 0, sqrt(sigma_2*x1_sim))
  
  y_sim                 <- X_sim%*%beta_true + u_sim
  
  model_sim             <- lm(y_sim ~ x1_sim + x2_sim + I(x2_sim^2) + I(x2_sim^3))
  estimates_sim_2[i,]   <- model_sim$coefficients
}

### parameter estimates
rbind("true" = c(beta_true[1:3],0,beta_true[4]), 
      "estimated" = model_2$coefficients,
      "mean estimates" = c(mean(estimates_sim_2[,1]),mean(estimates_sim_2[,2]),mean(estimates_sim_2[,3]),mean(estimates_sim_2[,4]),mean(estimates_sim_2[,5])))

### standard deviations of the estimates
rbind(estimated = summary(model_2)$coefficients[,2], 
      simulated = c(sd(estimates_sim_2[,1]), sd(estimates_sim_2[,2]), sd(estimates_sim_2[,3]), sd(estimates_sim_2[,4]), sd(estimates_sim_2[,5])))




###### (h)   -------------------------------------------------------------------
### Test the model obtained in (e) for heteroskedasticity. You can use a Breusch-Pagan test or a White test.

res           <- model_2$residuals
y_hat         <- model_2$fitted.values

model_white   <- lm(I(res^2) ~ x1+x2+I(x2^2)+I(x2^3)+I(x2^4)+I(x2^5)+I(x1*x2)+I(x1*x2^2)+I(x1*x2^3))
summary(model_white)

model_BP      <- lm(I(res^2) ~ x1+x2+I(x2^2)+I(x2^3))
summary(model_BP)




###### (i)   -------------------------------------------------------------------
### Based on the test you used to detect heteroskedasticity, calculate the Feasible Generalised Least Squares estimator. 
### To do so, calculate the matrix L with 
### L^2=Omega=diag(\hat\sigma_1^2,...,\hat\sigma_N^2)
### and estimate the rewritten model L^{???1}y = L^{???1}X \beta + L^{???1}u.

model_sigma   <- lm(I(log(res^2)) ~ x1)
summary(model_sigma)

### calculate the matrix L based on the fitted values of the variances
L_FGLS        <- diag(sqrt(exp(model_sigma$fitted.values)))
L_FGLS_inv    <- solve(L_FGLS)

### define the rewritten model
y_FGLS        <- L_FGLS_inv %*% y
x0_FGLS       <- L_FGLS_inv %*% x0
x1_FGLS       <- L_FGLS_inv %*% x1
x2_FGLS       <- L_FGLS_inv %*% x2
x2_FGLS_2     <- L_FGLS_inv %*% x2^2
x2_FGLS_3     <- L_FGLS_inv %*% x2^3

model_FGLS    <- lm(y_FGLS ~ x0_FGLS + x1_FGLS + x2_FGLS + x2_FGLS_2 + x2_FGLS_3 + 0)
summary(model_FGLS)



###### (j)   -------------------------------------------------------------------
### Calculate the Heteroskedasticity-consistent White estimator for the variance of the OLS estimator

X_mat <- as.matrix(data.frame(1, x1, x2, x2^2, x2^3))
X_mat <- model.matrix(model_2)
rbind("lm" = model_2$coefficients, 
      "by hand" = as.numeric(solve(t(X_mat)%*%X_mat)%*%t(X_mat)%*%y))

beta_hat_var_HC <- solve(t(X_mat)%*%X_mat) %*% t(X_mat) %*% diag(model_2$residuals^2) %*% X_mat %*% solve(t(X_mat)%*%X_mat)
beta_hat_var_HC <- sandwich::vcovHC(model_2, type = "HC")



###### (k)   -------------------------------------------------------------------
### Compare the estimates and their standard deviations with those obtained in (f) 
### and to the standard deviations of the 10000 estimates obtained in (g).

### parameter estimates
rbind(OLS = model_2$coefficients, 
      FGLS = model_FGLS$coefficients, 
      true = c(beta_true[1:3],0,beta_true[4]))

### standard deviations of the estimates
rbind(OLS = summary(model_2)$coefficients[,2], 
      FGLS = summary(model_FGLS)$coefficients[,2], 
      White = sqrt(diag(beta_hat_var_HC)), 
      simulated = c(sd(estimates_sim_2[,1]),sd(estimates_sim_2[,2]),sd(estimates_sim_2[,3]),sd(estimates_sim_2[,4]),sd(estimates_sim_2[,5])))





### Extra: use the GLS estimator
### this is possible because we actually know the true data generating process and, hence, the heteroscedastie.

### calculate the matrix L based on the fitted values of the variances
L_GLS        <- sqrt(sigma_2*x1)

### define the rewritten model
y_GLS        <- y/L_GLS
x0_GLS       <- x0/L_GLS
x1_GLS       <- x1/L_GLS
x2_GLS       <- x2/L_GLS
x2_GLS_2     <- x2^2/L_GLS
x2_GLS_3     <- x2^3/L_GLS

model_GLS    <- lm(y_GLS ~ x0_GLS + x1_GLS + x2_GLS + x2_GLS_2 + x2_GLS_3 + 0)
summary(model_FGLS)

### parameter estimates
rbind(OLS = model_2$coefficients, 
      FGLS = model_FGLS$coefficients, 
      GLS = model_GLS$coefficients, 
      true = c(beta_true[1:3],0,beta_true[4]))

### standard deviations of the estimates
rbind(OLS = summary(model_2)$coefficients[,2], 
      FGLS = summary(model_FGLS)$coefficients[,2], 
      GLS = summary(model_GLS)$coefficients[,2], 
      White = sqrt(diag(beta_hat_var_HC)), 
      simulated = c(sd(estimates_sim_2[,1]),sd(estimates_sim_2[,2]),sd(estimates_sim_2[,3]),sd(estimates_sim_2[,4]),sd(estimates_sim_2[,5])))

