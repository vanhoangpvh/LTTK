sample = c(3.169, 3.235,3.801,2.053,2.051,6.026,2.417,2.053,3.867,2.131)

f = function(x,alpha,theta){
  kq = ifelse(x>=theta,((alpha*theta^alpha)/x^(alpha+1)),0)
  return (kq)
}

# theta = 2
alpha_hat = mean(sample)/(mean(sample)-2)
alpha_hat

logLikelihood = function(alpha,theta){
   sum(log(f(sample,alpha,theta)))
}

# alpha = 3
theta = seq(from = 1, to = 3, length.out  = 100)
Logfunc = rep(1,100)

for (i in 1:100){
  Logfunc[i] = logLikelihood(alpha = 3, theta[i])
}

plot(theta, Logfunc,type = 'l',pch=19,lwd = 2)

theta_MLE = min(sample)
points(theta_MLE,logLikelihood(alpha = 3,theta = theta_MLE),col='red',pch = 19)



lowerBound = mean(sample) - qt(5/(100*2),df = n-1,lower.tail  =FALSE) * sd(sample)/sqrt(length(sample))
lowerBound

lowerBound1 = mean(sample) - qnorm(1-5/(100*2)) *sd(sample)/sqrt(length(sample))
lowerBound1



tuoigheptim = c(28,41,46,53,39,36,47,29,48,44)
thoigiansong = c(7,278,44,48,406,382,1995,176,323,1846)

model = lm(thoigiansong ~ tuoigheptim)

summary(model)

coefficients <- coef(model)
cat("Regression line equation: y =", coefficients[1], "+", coefficients[2], "* x\n")

# Khoang tin cay 95% cho beta0 va beta1
confint(model,level = 0.95)

# Tim gia tri uoc luong khi x = 55
predict(model, data.frame(tuoigheptim = 55))
# Tim khoang tin cay cho gia tri uoc luong
predict(model, data.frame(tuoigheptim = 55), interval = "prediction", level = 0.95)
# Tim R^2
R2 = summary(model)$r.squared
R2

predict = predict(model)
predict

SSE = sum((thoigiansong - predict)^2)
SST = sum((thoigiansong - mean(thoigiansong))^2)

SSE
SST

R2 = 1 - SSE/SST
R2

r = cor(thoigiansong,tuoigheptim)
r

r1 = sqrt(R2)
r1






