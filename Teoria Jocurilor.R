install.packages("gamer") 
library(gamer)

# Definim matricea de payoff-uri
# Primul element este pentru A, al doilea pentru B
# Valorile sunt ani de închisoare (negative, deoarece vrem să minimizăm pedeapsa)
payoffs = list(
  A = matrix(c(-1, -5, 0, -3), nrow = 2, byrow = TRUE),
  B = matrix(c(-1, 0, -5, -3), nrow = 2, byrow = TRUE)
)

dimnames(payoffs$A) = list(A = c("Cooperate", "Defect"), B = c("Cooperate", "Defect"))
dimnames(payoffs$B) = dimnames(payoffs$A)

# Creăm jocul
game = normal_form(payoffs)

# Afișăm jocul pentru verificare
print(game)

# Calculăm echilibrul Nash
nash_eq = nash(game)
print(nash_eq)




#doua firme Modelul Cournot
library(nleqslv)

a = 100  
b = 1   
c = 20   

cournot_eqs <- function(q) {
  q1 <- q[1]
  q2 <- q[2]
  eq1 <- a - b * (q1 + q2) - c - b * q1  
  eq2 <- a - b * (q1 + q2) - c - b * q2 
  return(c(eq1, eq2))
}

# Punct de start init
start_vals <- c(10, 10)

sol <- nleqslv(start_vals, cournot_eqs)

q1_opt <- sol$x[1]
q2_opt <- sol$x[2]
Q_total <- q1_opt + q2_opt
P_eq <- a - b * Q_total

cat("Cantitatea optimă a firmei 1:", q1_opt, "\n")
cat("Cantitatea optimă a firmei 2:", q2_opt, "\n")
cat("Prețul de echilibru:", P_eq, "\n")


#trei firme Modelul Cournot
library(nleqslv)

a = 100  
b = 1    
c = 20  

cournot_eqs_3 <- function(q) {
  q1 <- q[1]
  q2 <- q[2]
  q3 <- q[3]
  eq1 <- a - b * (q1 + q2 + q3) - c - b * q1  # dπ1/dq1 = 0
  eq2 <- a - b * (q1 + q2 + q3) - c - b * q2  # dπ2/dq2 = 0
  eq3 <- a - b * (q1 + q2 + q3) - c - b * q3  # dπ3/dq3 = 0
  return(c(eq1, eq2, eq3))
}

start_vals <- c(10, 10, 10)

sol <- nleqslv(start_vals, cournot_eqs_3)

if (sol$termcd == 1) {  
  q1_opt <- sol$x[1]
  q2_opt <- sol$x[2]
  q3_opt <- sol$x[3]
  Q_total <- q1_opt + q2_opt + q3_opt
  P_eq <- a - b * Q_total
  cat("Cantitatea optimă a firmei 1:", q1_opt, "\n")
  cat("Cantitatea optimă a firmei 2:", q2_opt, "\n")
  cat("Cantitatea optimă a firmei 3:", q3_opt, "\n")
  cat("Prețul de echilibru:", P_eq, "\n")
} else {
  cat("Nu s-a putut găsi o soluție.\n")
}

