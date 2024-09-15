data = read.csv(file.choose(), sep =',')
data
# Bai 1
alpha_hat =  1 + (mean(data$x)^2 / var(data$x))
beta_hat = mean(data$x) *(alpha_hat -1)

f = function(x,alpha,beta){
  kq = ifelse(x > 0, ((beta^alpha) / gamma(alpha)) * x^(alpha - 1) * exp(-beta / x), 0)
  return(kq)
}

logLikelihood = function(alpha = 3, beta) {
  sum(log(f(data$x, alpha, beta)))
}

Likelihood = function(alpha,beta){
  prod(f(data$x,alpha,beta))
}

beta = seq(from =  1,to = 2,length.out = 100)
Likefunc = rep(1,100)

for(i in 1:100){
  Likefunc[i] = Likelihood(alpha = 3,beta[i])
}

plot(beta,Likefunc,type = 'l')

beta_MLE = 1.2
points(beta_MLE,Likefunc(alpha = 3,beta = beta_MLE),lwd = 2)

# Bai 2
# 2.1 
# H0: beta = 1.5
# H1: beta > 1.5
t.test(data$x,alternative = 'greater',mu=1.5/(3-1))$p.value
# p.value=0.39>0.01 -> Khong du co so bac bo H0 -> chap nhan H0 -> Beta=1.5

# 2.2
# H0: p = 0.25
count2.2=0
for (i in 1:450){
  if (0.5<=data$x[i] & data$x[i]<=0.75){
    count2.2=count2.2+1
  }
}
count2.2
prop.test(count,n=450,alternative='less',p=0.25)$p.value
# p.value=0.46>0.05 -> Khong du co so bac bo H0 -> chap nhan H0 -> p=0.25

# Bai 3
data(women)
# 3.1
plot(women$height,women$weight)

# 3.2
model = lm(women$weight~women$height)
summary_model = summary(model)

# Kiem dinh gia thuyet cho beta0
# H0: beta0 = 0
# H1: beta0 != 0
summary_model$coefficients["(Intercept)", "Pr(>|t|)"]
# Vi p_value = 1.711e-09 < 0.05 -> Bac bo H0 -> beta0 != 0

# Kiem dinh gia thuyet cho beta1
# H0: beta1 = 0
# H1: beta1 != 0
summary_model$coefficients["women$height", "Pr(>|t|)"]
# Vi p_value = 1.0909e-09 < 0.05 -> Bac bo H0 -> beta1 != 0

# 3.3
# H0: beta1 = 3.5
# H1: beta1 != 3.5
coefficents = coef(model)
beta1_hat = coefficients[2]


beta1 = 3.5

t_beta1 = (beta1_hat-beta1)/summary_model$coefficients[2,2]

p_value_1 = pt(t_beta1,df = length(women$height)-2)
p_value_1
# p_value = 1.47e-14 < 0.01 -> bac bo H0











