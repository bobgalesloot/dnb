### This is material illustrating the methods from:
###   Advies Commissie Parameters
###   29 november 2022
###   Technical Appendix: Specification of the CP2022 Model
###   9.1 Simulation under P
###
### Date: 14 March 2024
### Author: Bob Galesloot
###
### Please send comments, suggestions, bugs, code etc. to
### bobgalesloot@bgez.nl
###
### This code is being provided solely for information and general
### illustrative purposes. The authors will not be responsible for the
### consequences of reliance upon using the code or for numbers produced
### from using the code.
###
### © BGEZ 2024
###
### Press cmd + shift + return to run all

#install.packages("openxlsx")
#rm(list = ls(all = T))
#gc()

start_time <- Sys.time()

DNB <- F # Use DNB scenarios or not
setwd("~/R")
filename <- "cp2022-p-scenarioset-20k-2024q1.xlsx"
N <- 20000 # Number of scenarios
Tmax <- 100 # Number of years >= 2
Taumax <- 100 # Number of maturities >= 2
CPB_Inflatie <- c(
  rep(0.038, 12), # Number of months remaining in the year
  rep(0.026, 12),
  rep(0.025, 12),
  rep(0.024, 12),
  rep(0.021, 12),
  rep(0.022, 12),
  rep(0.022, 12),
  rep(0.022, 12)
)

if (Tmax * 12 <= length(CPB_Inflatie)) {
  CP_Inflatie <- CPB_Inflatie[1:(Tmax * 12)]
} else {
  CP_Inflatie <- c(CPB_Inflatie,
                   rep(0.02, Tmax * 12 - length(CPB_Inflatie)))
}
rm(CPB_Inflatie)

set.seed(123)

library(openxlsx)

DNB_Parameters <-
  as.numeric(as.matrix(read.xlsx(
    filename, sheet = "0_Parameters", colNames = T
  ))[1:47, 2])
DNB_Phi <-
  unname(as.matrix(
    read.xlsx(filename, sheet = "7_Renteparameter_phi_N", colNames = F)
  ))[1:Taumax, 1:(Tmax + 1)]
DNB_Psi <-
  unname(as.matrix(
    read.xlsx(filename, sheet = "8_Renteparameter_Psi_N", colNames = F)
  ))[1:Taumax,]
if (DNB) {
  DNB_X1 <-
    unname(as.matrix(
      read.xlsx(filename, sheet = "1_Toestandsvariabele_1", colNames = F)
    ))[1:N, 1:(Tmax + 1)]
  DNB_X2 <-
    unname(as.matrix(
      read.xlsx(filename, sheet = "2_Toestandsvariabele_2", colNames = F)
    ))[1:N, 1:(Tmax + 1)]
  DNB_X3 <-
    unname(as.matrix(
      read.xlsx(filename, sheet = "3_Toestandsvariabele_3", colNames = F)
    ))[1:N, 1:(Tmax + 1)]
  DNB_Aandelenrendement <-
    unname(as.matrix(
      read.xlsx(filename, sheet = "4_Aandelenrendement", colNames = F)
    ))[1:N, 1:Tmax]
  DNB_Prijsinflatie_EU <-
    unname(as.matrix(
      read.xlsx(filename, sheet = "5_Prijsinflatie_EU", colNames = F)
    ))[1:N, 1:Tmax]
  DNB_Prijsinflatie_NL <-
    unname(as.matrix(
      read.xlsx(filename, sheet = "6_Prijsinflatie_NL", colNames = F)
    ))[1:N, 1:Tmax]
  
# Equation (14)
  
  DNB_y <- array(0.0, dim = c(N, Taumax, (Tmax + 1)))
  for (t in 1:(Tmax + 1)) {
    for (tau in 1:Taumax) {
      DNB_y[, tau, t] <-
        exp(-(
          DNB_Phi[tau, t] + DNB_Psi[tau, 1] * DNB_X1[, t] +
            DNB_Psi[tau, 2] * DNB_X2[, t] +
            DNB_Psi[tau, 3] * DNB_X3[, t]
        ) / tau) - 1
    }
  }
}

# Equation (3)

EX <- c(DNB_Parameters[1], DNB_Parameters[2], DNB_Parameters[3])
K <- rbind(
  c(DNB_Parameters[7], 0, 0),
  c(DNB_Parameters[8], DNB_Parameters[10], DNB_Parameters[12]),
  c(DNB_Parameters[9], DNB_Parameters[11], DNB_Parameters[13])
)
Lambda <- rbind(
  c(DNB_Parameters[28], 0, 0, 0, 0),
  c(0, DNB_Parameters[29], 0, 0, 0),
  c(0, 0, DNB_Parameters[30], 0, 0),
  c(0, 0, 0, DNB_Parameters[31], 0),
  c(0, 0, 0, 0, DNB_Parameters[32])
)
Lambda1 <- Lambda[2:5, 2:5]
Lambda0 <- rbind(c(0, 0, 0, 0, 0),
                 c(0, 1, 0, 0, 0),
                 c(0, 0, 1, 0, 0),
                 c(0, 0, 0, 1, 0),
                 c(0, 0, 0, 0, 1))
omega <- DNB_Parameters[21]
Sigmarpi <- rbind(
  c(omega, 0, 0, 0, 0),
  c(DNB_Parameters[22], DNB_Parameters[24], DNB_Parameters[26], 0, 0),
  c(DNB_Parameters[23], DNB_Parameters[25], DNB_Parameters[27], 0, 0)
)

# Equation (5)

SigmaSPi <-
  rbind(
    c(
      DNB_Parameters[35],
      DNB_Parameters[36],
      DNB_Parameters[37],
      DNB_Parameters[38],
      DNB_Parameters[39]
    ),
    c(
      DNB_Parameters[40],
      DNB_Parameters[41],
      DNB_Parameters[42],
      DNB_Parameters[43],
      DNB_Parameters[44]
    )
  )
D0 <- (SigmaSPi %*% Lambda0) %*% t(SigmaSPi)
D0 <- c(D0[1, 1], D0[2, 2])
mu0 <- c(DNB_Parameters[33], DNB_Parameters[34]) -
  ((1 / 2) * D0)

# Equation (6)

D <- (SigmaSPi %*% Lambda) %*% t(SigmaSPi)
D <- c(D[1, 1], D[2, 2])
K0 <- rbind(c(0, 1, 0),
            c(0, 0, 1)) -
  ((1 / 2) * D %*% t(c(1, 0, 0)))

n <- Tmax * 12
t0 <- 0
Deltat <- (1 / n) * (Tmax - t0)
Simulation <- array(0.0, dim = c(N, n + 1, 7))

# Simulation[,, 1]: v
# Simulation[,, 2]: r
# Simulation[,, 3]: pi
# Simulation[,, 4]: ln S/S
# Simulation[,, 5]: ln Pi/Pi
# Simulation[,, 6]: ln PiNL/PiNL
# Simulation[,, 7]: eta

for (j in 1:N) {
  Simulation[j, 1, 1] <- DNB_Parameters[45] # v0
  Simulation[j, 1, 2] <- DNB_Parameters[46] # r0
  Simulation[j, 1, 3] <- DNB_Parameters[47] # pi0
}

# Equation (51)

f_Andersen <- function(Vinst) {
  V2 <- Vinst
  UV1 <- runif(1)
  m <- Vlong_Andersen + (V2 - Vlong_Andersen) * k1_Andersen
  s2 <- V2 * k2_Andersen + k3_Andersen
  psi <- s2 / (m ^ 2)
  psihat <- 1 / psi
  b2 <- 2 * psihat - 1 + sqrt(2 * psihat * (2 * psihat - 1))
  a <- m / (1 + b2)
  if (psi <= psiC_Andersen) {
    V2 <- a * (sqrt(b2) + qnorm(UV1)) ^ 2
  } else {
    p <- (psi - 1) / (psi + 1)
    if (UV1 <= p) {
      V2 <- 0
    } else {
      beta <- (1 - p) / m
      V2 <- log((1 - p) / (1 - UV1)) / beta
    }
  }
  return(V2)
}

Vlong_Andersen <- EX[1]
kappa_Andersen <- K[1, 1]
epsilon_Andersen <- omega
dT_Andersen <- Deltat
k1_Andersen <- exp(-kappa_Andersen * dT_Andersen)
k2_Andersen <-
  epsilon_Andersen ^ 2 * k1_Andersen * (1 - k1_Andersen) / kappa_Andersen
k3_Andersen <-
  exp(kappa_Andersen * dT_Andersen) * 0.5 * k2_Andersen * (1 - k1_Andersen) * Vlong_Andersen
psiC_Andersen <- 1.5

for (j in 1:N) {
  for (i in 1:n) {
    Simulation[j, i + 1, 1] <- f_Andersen(Vinst = Simulation[j, i, 1])
    Simulation[j, i, 7] <- (1 / omega) *
      ((Simulation[j, i, 1] * Deltat) ^ (-1 / 2)) *
      (Simulation[j, i + 1, 1] - Simulation[j, i, 1] - K[1, 1] * (EX[1] - Simulation[j, i, 1]) * Deltat)
  }
}

rm(
  f_Andersen,
  Vlong_Andersen,
  kappa_Andersen,
  epsilon_Andersen,
  dT_Andersen,
  k1_Andersen,
  k2_Andersen,
  k3_Andersen,
  psiC_Andersen
)

# Equations (52)-(53)

LeftM <- rbind(c(0, 1, 0, 0, 0),
               c(0, 0, 1, 0, 0),
               c(0, 0, 0, 1, 0),
               c(0, 0, 0, 0, 1))
Sigma <- rbind(Sigmarpi, SigmaSPi)
Deltat12 <- Deltat ^ (1 / 2)
for (j in 1:N) {
  for (i in 1:n) {
    Lpart <- rbind(K %*% (EX - c(Simulation[j, i, 1:3])),
                   mu0 + (K0 %*% c(Simulation[j, i, 1:3]))) * Deltat
    Rpart <- (Sigma %*%
                ((Lambda0 + Simulation[j, i, 1] * Lambda) ^ (1 / 2)) *
                Deltat12) %*%
      t(cbind(Simulation[j, i, 7], t(rnorm(4))))
    Temp <- LeftM %*% (Lpart + Rpart)
    Simulation[j, i + 1, 2] <- Simulation[j, i, 2] + Temp[1]
    Simulation[j, i + 1, 3] <- Simulation[j, i, 3] + Temp[2]
    Simulation[j, i + 1, 4] <- Simulation[j, i, 4] + Temp[3]
    Simulation[j, i + 1, 5] <- Simulation[j, i, 5] + Temp[4]
  }
}

# Equations (54)-(55)

for (i in 1:n) {
  H <- # Be careful: DNB uses the first 20,000 scenerios, no matter N
    -(1 / N) * sum(Simulation[, i + 1, 5] - Simulation[, i, 5]) +
    log(1 + CP_Inflatie[i]) * Deltat
  Simulation[, i + 1, 6] <-
    Simulation[, i, 6] + Simulation[, i + 1, 5] - Simulation[, i, 5] + H
}

rm(
  K,
  K0,
  Lambda,
  Lambda0,
  Lambda1,
  LeftM,
  Lpart,
  Rpart,
  Sigma,
  Sigmarpi,
  SigmaSPi,
  Temp,
  D,
  D0,
  Deltat,
  Deltat12,
  DNB_Parameters,
  EX,
  H,
  mu0,
  omega,
  t0
)

X1 <- matrix(0.0, nrow = N, ncol = n / 12 + 1)
X2 <- matrix(0.0, nrow = N, ncol = n / 12 + 1)
X3 <- matrix(0.0, nrow = N, ncol = n / 12 + 1)
for (j in 1:N) {
  for (i in (1:(n / 12 + 1))) {
    X1[j, i] <- Simulation[j, i * 12 - 11, 1]
    X2[j, i] <- Simulation[j, i * 12 - 11, 2]
    X3[j, i] <- Simulation[j, i * 12 - 11, 3]
  }
}
Prijsinflatie_EU <- matrix(0.0, nrow = N, ncol = n / 12)
for (j in 1:N) {
  for (i in (1:(n / 12))) {
    Prijsinflatie_EU[j, i] <-
      exp(Simulation[j, i * 12 + 1, 5] - Simulation[j, i * 12 - 11, 5]) - 1
  }
}
Prijsinflatie_NL <- matrix(0.0, nrow = N, ncol = n / 12)
for (j in 1:N) {
  for (i in (1:(n / 12))) {
    Prijsinflatie_NL[j, i] <-
      exp(Simulation[j, i * 12 + 1, 6] - Simulation[j, i * 12 - 11, 6]) - 1
  }
}
Aandelenrendement <- matrix(0.0, nrow = N, ncol = n / 12)
for (j in 1:N) {
  for (i in (1:(n / 12))) {
    Aandelenrendement[j, i] <-
      exp(Simulation[j, i * 12 + 1, 4] - Simulation[j, i * 12 - 11, 4]) - 1
  }
}

# Equation (14)

y <- array(0.0, dim = c(N, Taumax, (Tmax + 1)))
for (t in 1:(Tmax + 1)) {
  for (tau in 1:Taumax) {
    y[, tau, t] <-
      exp(-(
        DNB_Phi[tau, t] + DNB_Psi[tau, 1] * X1[, t] +
          DNB_Psi[tau, 2] * X2[, t] +
          DNB_Psi[tau, 3] * X3[, t]
      ) / tau) - 1
  }
}

rm(i, j, n, t, tau)

end_time <- Sys.time()
end_time - start_time
