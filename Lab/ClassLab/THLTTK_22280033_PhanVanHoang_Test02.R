#Bai kiem tra lan 2
#Ho ten: Phan Van Hoang
#MSSV 22280033

#Bai 1
#(i)
f_logi = function(x,mu,s){
  f_tuso = exp(-(x-mu)/s)
  f_mauso = s*(1+exp(-(x-mu)/s))^2
  f = f_tuso/f_mauso
  return (f)
}

curve(f_logi(x,1,1),add = TRUE, from = -2, to = 2, col = 'red', lwd = 2)

#(ii)
set.seed(1)
N = 7000
X = runif(7000, 0, 1)
mu_ii = 1
s_ii = 1
Y = mu_ii + s_ii*log(X/(1-X))

hist(Y,breaks = 40, freq = FALSE)
curve(f_logi(x,mu_ii,s_ii),add = TRUE, col = 'red', lwd = 2)

#(iii)
randomGenerate = function(n,mu,s){
  
}

#Bai 2

#(i)
g = function(x){ #x>=0
  return (x/(x+1))
}

g_deriv = function(x){
  return (1/(x+1)^2)
}

#(ii)
curve(g(x),add =TRUE, col = 'red', lwd = 2, from = 0, to = 2)
curve(g_deriv(x), add = TRUE, col = 'red', lwd = 2, from = 0, to = 2)

#(iii)
n = 200
N_iii = 8000
mu_iii = 3
sigmas = 6
set.seed(1)

Y_iii = rep(1,N_iii)

for(i in 1:N_iii){
  sample_i = rchisq(n,3)
  Y_iii[i] = sqrt(n) * (g(1/n *sum(sample_i))-g(mu_iii))
}

hist(Y_iii,freq = FALSE, breaks = 100)
curve(dnorm(x,0,sqrt(6*abs(g_deriv(3))^2)),add =TRUE, col = 'red', lwd = 2)

