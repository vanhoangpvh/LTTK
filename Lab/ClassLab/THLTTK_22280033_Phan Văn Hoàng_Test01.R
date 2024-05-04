##
## Bai kiem tra thuc hanh LTTK
## Thu 2 - Ca hoc 1
##
## Ho ten :Phan Van Hoang - MSSV : 22280033
##
## *****************************************

##Bai 01
##1.1 
nCk = function(n,k){
  x = factorial(n)
  y = factorial(k)*factorial(n-k)
  return(x/y)
}

##1.2
BinomMass = function(k,n,p){
  x = nCk(n,k)*(p^k)*(1-p)^{n-k}
  return (x)
}

##1.3
BinomCDF = function(x,n,p){
  S = 0
  for(k in 0:x){
    S = S + BinomMass(k,n,p)
  }
  return (S)
}

##1.4
n = 100
p = 0.65
k = 1:n
P = BinomMass(k,n,p)
plot(k,P,type = 'h')

##1.5
##Tinh E[X]
EX = function(n,p){
  S = 0
  for(k in 0:n){
    S =S+k*BinomMass(k,n,p)
  }
  return (S)
}

EX(10,0.75)
10*0.75
##Tinh VarX
VarX = function(n,p){
  S = 0
  for(k in 0:n){
    S = S + ((k-EX(n,p))^2)*BinomMass(k,n,p)
  }
  return (S)
}
VarX(10,0.75)
10*0.75*0.25

##Bai 02
##2.1
DiemThi = data.frame(
  GiuaKi = c(77,50,71,72,81,94,96,99,67),
  CuoiKi = c(82,66,78,34,47,85,99,99,68)
)

##2.2
M = matrix(
  data = c(rep(1,9),DiemThi$GiuaKi),
  nrow = 9,ncol =2,byrow=F
)

y = DiemThi$CuoiKi

##2.3
##Goi Q la ma tran 2x1 chua alpha va beta
Q = solve(t(M)%*%M) %*% t(M) %*% y

alpha = Q[1,1]
beta = Q[2,1]

##2.4
##i
x1 = DiemThi$GiuaKi
plot(x1,y,xlab = 'Diem thi giua ki', ylab = 'Diem thi cuoi ki')
##ii
x2 = seq(from = -100, to = 100, by = 0.5)
y1 = alpha + beta*x2
lines(x2,y1,col = 'red', lwd = 2)


