library(matlib)
sf1 <- matrix(c(1106.000,396.700,108.400,0.787,26.230,396.700,2382.000,1143.000,-0.214,-23.960,108.400,1143.000,2136.000,2.189,-20.8400,.787,-0.214,2.189,0.016,0.216,26.230,-23.960,-20.840,0.216,70.560),nrow = 5,ncol = 5)
s11 <- sf1[1:3,1:3]
s12 <- sf1[1:3,4:5]
s21 <- sf1[4:5,1:3]
s22 <- sf1[4:5,4:5]

U_matrix <- solve(sqrt(s11))%*% s12 %*% solve(s22) %*% s21 %*% solve(sqrt(s11))
rho_matrix <- eigen(U_matrix)
V_matrix <- solve(sqrt(s22))%*% s21 %*% solve(s11) %*% s12 %*% solve(sqrt(s22))
rho1_matrix <- eigen(V_matrix)

U_coeffiecients <- (rho_matrix$vectors) %*% (solve(sqrt(s11))) 
V_coeffiecients <- (rho1_matrix$vectors) %*% (solve(sqrt(s22)))
sqrt_matrix <- diag(sqrt(sf1))
diagonal_matrix <- diag(x=c(sqrt_matrix))
### self correlation
selfcorrelation <- U_coeffiecients %*% s11 %*% solve(diagonal_matrix[1:3,1:3])
selfcorrelation1 <- V_coeffiecients %*% s22 %*% solve(diagonal_matrix[4:5,4:5])

### Normalised Coeffiecient
normalised_U_coeffiecient <- U_coeffiecients %*% diagonal_matrix[1:3,1:3]
normalised_V_coeffiecient <- V_coeffiecients %*% diagonal_matrix[4:5,4:5]


p <- 3
q <- 2
n <- 46
const <- -(n - 1 - (0.5 * (p + q + 1)))*log((1-(rho_matrix$values[1]))*(1-(rho_matrix$values[2])))
const1 <- -(n-1-(0.5 * (p+q+1)))*log(1-(rho1_matrix$values[1]))

test_value <- qchisq(0.95,6)
test_value1 <- qchisq(0.95,2)
