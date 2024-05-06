A = matrix(
  data = 1:100,
  nrow = 10, ncol = 10, byrow = TRUE
)
A
diag(A)


matrix_v = matrix(
  data = 1:10,
  nrow = 10, ncol = 1, byrow = TRUE
)

result = solve(t(A)%*%A+diag(10)) %*% t(A) %*% matrix_v
result

diag(10)

df = data.frame(
  x = 1:10,
  y = 11:20
)

df$x

df[df$y > 15,]
df[df$x %in% c(1,2,3,4,5),]

