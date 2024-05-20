# Phan Van Hoang
# 22280033
# Test 03

# Bai 01
# (i)
n_1 = 10
x = c(1,2,1,0,0,0,1,2,1,2)
theta_hat = 2 - mean(x)

# (ii)

  Px = function(theta, x){
    result <- ifelse(x == 0, theta / 3,
              ifelse(x == 1, theta / 3,
              ifelse(x == 2, 1 - (2 * theta) / 3, NA)))
    return(result)
  }
  logLikelihood = function(theta){
    sum(log(Px(theta,x)))
  }
# (iii)  
  theta = seq(from = 3/4, to = 5/4, length.out = 100)
  logfunc = rep(1,100)
  
  for (i in 1:100){
    logfunc[i] = logLikelihood(theta[i])
  }
  
  plot(theta, logfunc, type = 'l', col = 'skyblue2', pch = 19, lwd = 2)
# (iv)
  theta_MLE = 21/20
  points(theta_MLE, logLikelihood(theta_MLE), col = 'red', pch = 19)

# Bai 02
 gamma = -digamma(1)
 n_2 = 1000
 mu_X_mau = 0.6016
 var_mau = 1.785

 # (i) 
 quantileLeft = qchisq(5/(100*2), df = n_2- 1,lower.tail = FALSE)
 quantileRight = qchisq(5/(100*2), df = n_2- 1,lower.tail = FALSE)
 lowerBound = (n_2-1)* var_mau/quantileLeft
 upperBound = (n_2-1)* var_mau/quantileRight
 lower_beta= sqrt(lowerBound/ (pi^2/6))
 
 upper_beta= sqrt(upperBound/ (pi^2/6))
 
 
 # (ii)
 
 eps = qnorm ( 1 /(100*2) , lower.tail = FALSE ) * sqrt(var_mau)/ sqrt ( n_2 )
 lowerBound = mu_X_mau - eps
 upperBound = mu_X_mau + eps
 
 lowerbound_alpha = lowerBound - gamma
 upperbound_alpha = upperBound - gamma