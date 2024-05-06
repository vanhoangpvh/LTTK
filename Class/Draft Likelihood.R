X = c(3.051,0.704,3.511,2.266,3.189,1.099,0.738,1.868,0.522,2.731)

f = function(X,alpha){
  1/2 * alpha^3 * X^2 * exp(-alpha*X)
}

loglikelihood = function(alpha){
  sum(log(f(X,alpha)))
}

alpha = seq(from = 1, to = 2, length.out = 100)
logfunc = rep(1,100)
for(i in 1:100){
  logfunc[i] = loglikelihood(alpha[i])
}

plot(alpha, logfunc, type = 'l', col = 'skyblue2', pch = 19, lwd = 2)

alpha_MLE = 3 / mean(X)
points(alpha_MLE, loglikelihood(alpha_MLE), col = 'red',pch = 19)