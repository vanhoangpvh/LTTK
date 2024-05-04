# Phan Van Hoang
# 22280033
# Giua Ki LTTK

# Bai 01
# (1.1)
curve(sin(x), from = -pi, to = pi, col = 'red', lwd = 2)

# (1.2)
# (i)
t = seq(-pi,pi,length.out = 100)

#(ii)
corruptSignal = rep(1,100)
epsilon = rnorm(100,0,sqrt((0.1)^2))
for(i in 1:100)(
  corruptSignal[i] = sin(t[i]) + epsilon[i]
)

corruptSignal

# (iii)
D = matrix(
  data = rep(0,98*100),
  nrow = 98, byrow = TRUE
)

for (i in 1:98){
  D[i, i] <- D[i, i] - 1
  D[i, i + 2] <- D[i, i + 2] + 1
}

D

# (1.3)

#temp = I + 5*t(D) *D

temp = diag(rep(1,100)) + 5 * t(D) %*% D
temp

reconstructSignal = corruptSignal * solve(temp)
reconstructSignal

plot(t, corruptSignal, type = 'l', xlab = "t", ylab = "Corrupted Signal", col = "blue")
lines(t, reconstructSignal, col = "red",lwd = 2)


# Bai 02
# (2.1)  
F = function(x,a,b){
  return(ifelse(x > 0 & x < 1,a*b*x^(a-1)*(1-x^a)^(b-1),0))
}

curve(F(x,2,5),from = 0, to = 1, col = 'red', lwd = 2)

# (2.2)
set.seed(1)
N = 10^5
X_2 = runif(N,0,1)
Y_2 = (1-X^(1/5))^(1/2) 
hist(Y_2,breaks = floor(sqrt(N)), freq = FALSE)
curve(F(x,2,5),from = 0, to = 1, col = 'red', lwd = 2, add = TRUE)

generateF = function(n,a,b){
  X = runif(n,min = 0,max = 1)
  Y = (1-X^(1/b))^(1/a)
  return (Y)
}

# (2.3)

set.seed(1)
N_3 = 8000
X_3 = generateF(N_3,2,1)
Y_3 = -log(X_3)
hist(Y_3,breaks = floor(sqrt(N_3)),freq = FALSE)
curve(dexp(x,2),col = 'red',lwd = 2, add= TRUE)

# (2.4)

N_4 <- 10^4
F_1 <- numeric(N_4)
count <- 1
for (i in 1:N_4) {
  U <- runif(1, 0, 1)
  V <- runif(1, 0, 1)
  while (U^(1/2)/((U^(1/2)+V^(1/2))) <= 1) {
    F_1[count] <- U^(1/2)/((U^(1/2)+V^(1/2)))
    count <- count + 1
  }
}


hist(F_1, breaks = sqrt(N_4), freq = FALSE)
curve(dbeta(x, 2, 2), col = 'red', lwd = 2, add = TRUE)

