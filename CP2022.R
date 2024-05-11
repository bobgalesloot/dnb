### This is material illustrating the methods from:
###   Advies Commissie Parameters
###   29 november 2022
###   Technical Appendix: Specification of the CP2022 Model
###   9.1 Simulation under P
###
### Date: 26 March 2024
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

rm(list = ls(all = T))
gc()

start_time <- Sys.time()

DNB <- T # Use DNB scenarios or not
setwd("~/R")
filename_xlsx <- "cp2022-p-scenarioset-20k-2024q1.xlsx"
filename_csv <- "CP2022 P scenarioset 100K 2024Q1.csv"
N <- 20000 # Number of scenarios >= 2 (<= 100000 if DNB)
Tmax <- 10 # Number of years >= 2
Taumax <- 10 # Number of maturities >= 2
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

set.seed(42)

library(openxlsx)

DNB_Parameters <-
  as.numeric(as.matrix(read.xlsx(
    filename_xlsx, sheet = "0_Parameters", colNames = T
  ))[1:47, 2])
if (N > 20000) {
  DNB_Phi <- unname(as.matrix(read.csv(
    filename_csv,
    skip = 600000,
    nrows = min(100, Taumax),
    header = F
  )))[, 1:(Tmax + 1)]
  DNB_Psi <- unname(as.matrix(read.csv(
    filename_csv,
    skip = 600100,
    nrows = min(100, Taumax),
    header = F
  )))[, 1:3]
  if (DNB) {
    DNB_X1 <- unname(as.matrix(read.csv(
      filename_csv,
      nrows = min(100000, N), header = F
    )))[, 1:(Tmax + 1)]
    DNB_X2 <- unname(as.matrix(read.csv(
      filename_csv,
      skip = 100000,
      nrows = min(100000, N),
      header = F
    )))[, 1:(Tmax + 1)]
    DNB_X3 <- unname(as.matrix(read.csv(
      filename_csv,
      skip = 200000,
      nrows = min(100000, N),
      header = F
    )))[, 1:(Tmax + 1)]
    DNB_Aandelenrendement <- unname(as.matrix(
      read.csv(
        filename_csv,
        skip = 300000,
        nrows = min(100000, N),
        header = F,
        sep = ",",
        dec = "."
      )
    ))[, 1:min(100, Tmax)]
    DNB_Prijsinflatie_EU <- unname(as.matrix(
      read.csv(
        filename_csv,
        skip = 400000,
        nrows = min(100000, N),
        header = F,
        sep = ",",
        dec = "."
      )
    ))[, 1:min(100, Tmax)]
    DNB_Prijsinflatie_NL <- unname(as.matrix(
      read.csv(filename_csv,
        skip = 500000,
        nrows = min(100000, N),
        header = F,
        sep = ",",
        dec = "."
      )
    ))[, 1:min(100, Tmax)]
  }
} else {
  DNB_Phi <-
    unname(as.matrix(
      read.xlsx(filename_xlsx, sheet = "7_Renteparameter_phi_N", colNames = F)
    ))[1:Taumax, 1:(Tmax + 1)]
  DNB_Psi <-
    unname(as.matrix(
      read.xlsx(filename_xlsx, sheet = "8_Renteparameter_Psi_N", colNames = F)
    ))[1:Taumax,]
  if (DNB) {
    DNB_X1 <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "1_Toestandsvariabele_1", colNames = F)
      ))[1:N, 1:(Tmax + 1)]
    DNB_X2 <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "2_Toestandsvariabele_2", colNames = F)
      ))[1:N, 1:(Tmax + 1)]
    DNB_X3 <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "3_Toestandsvariabele_3", colNames = F)
      ))[1:N, 1:(Tmax + 1)]
    DNB_Aandelenrendement <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "4_Aandelenrendement", colNames = F)
      ))[1:N, 1:Tmax]
    DNB_Prijsinflatie_EU <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "5_Prijsinflatie_EU", colNames = F)
      ))[1:N, 1:Tmax]
    DNB_Prijsinflatie_NL <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "6_Prijsinflatie_NL", colNames = F)
      ))[1:N, 1:Tmax]
  }
}

rm(filename_csv, filename_xlsx)

# Fix "rentedip"

# DNB_Phi[,2] <- (DNB_Phi[,1] + DNB_Phi[,3]) / 2

# Equation (14)

if (DNB) {
  DNB_y <- array(0.0, dim = c(N, Taumax, (Tmax + 1)))
  for (t in 1:(Tmax + 1)) {
    DNB_y[,, t] <- t(exp(-(DNB_Phi[, t] + DNB_Psi[, 1] %o% DNB_X1[, t] +
                             DNB_Psi[, 2] %o% DNB_X2[, t] +
                             DNB_Psi[, 3] %o% DNB_X3[, t]) / c(1:Taumax)) - 1)
  }
  rm(t, DNB_X1, DNB_X2, DNB_X3)
}

# Equation (3)

EX <- DNB_Parameters[1:3]
K <- cbind(
  DNB_Parameters[7:9],
  c(0, DNB_Parameters[10:11]),
  c(0, DNB_Parameters[12:13])
)
Lambda <- diag(DNB_Parameters[28:32])
Lambda0 <- diag(c(0, 1, 1, 1, 1))
omega <- DNB_Parameters[21]
Sigmarpi <- rbind(
  c(omega, 0, 0, 0, 0),
  c(DNB_Parameters[22], DNB_Parameters[24], DNB_Parameters[26], 0, 0),
  c(DNB_Parameters[23], DNB_Parameters[25], DNB_Parameters[27], 0, 0)
)

# Equation (5)

SigmaSPi <- rbind(DNB_Parameters[35:39], DNB_Parameters[40:44])
mu0 <- DNB_Parameters[33:34] - (1 / 2) *
  diag((SigmaSPi %*% Lambda0) %*% t(SigmaSPi))

# Equation (6)

K0 <- rbind(c(0, 1, 0),
            c(0, 0, 1)) -
  (1 / 2) * diag((SigmaSPi %*% Lambda) %*% t(SigmaSPi)) %*% t(c(1, 0, 0))

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

Simulation[, 1, 1] <- DNB_Parameters[45] # v0
Simulation[, 1, 2] <- DNB_Parameters[46] # r0
Simulation[, 1, 3] <- DNB_Parameters[47] # pi0

# Equation (51)

f_Andersen2 <- function(Vinst) {
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

rm(f_Andersen2)

f_Andersen <- function(Vinst) {
  V2 <- Vinst
  m <- Vlong_Andersen + (V2 - Vlong_Andersen) * k1_Andersen
  psihat <- (m ^ 2) / (V2 * k2_Andersen + k3_Andersen)
  b2 <- 2 * psihat - 1 + sqrt(2 * psihat * (2 * psihat - 1))
  V2 <- (m / (1 + b2)) * (sqrt(b2) + rnorm(1)) ^ 2
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
  exp(kappa_Andersen * dT_Andersen) * (1 / 2) * k2_Andersen *
  (1 - k1_Andersen) * Vlong_Andersen
#psiC_Andersen <- 1.5

for (j in 1:N) {
  for (i in 1:n) {
    Simulation[j, i + 1, 1] <- f_Andersen(Vinst = Simulation[j, i, 1])
    Simulation[j, i, 7] <- (1 / omega) *
      ((Simulation[j, i, 1] * Deltat) ^ (-1 / 2)) *
      (Simulation[j, i + 1, 1] - Simulation[j, i, 1] -
         K[1, 1] * (EX[1] - Simulation[j, i, 1]) * Deltat)
  }
}

rm(f_Andersen, Vlong_Andersen, kappa_Andersen, epsilon_Andersen, dT_Andersen,
   k1_Andersen, k2_Andersen, k3_Andersen, i, j)
#rm(psiC_Andersen)

# Equations (52)-(53)

LeftM <- cbind(c(0, 0, 0, 0), diag(4))
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
    Simulation[j, i + 1, 2:5] <- Simulation[j, i, 2:5] + Temp
  }
}

rm(K, K0, Lambda, Lambda0, LeftM, Lpart, Rpart, Sigma, Sigmarpi, SigmaSPi, Temp,
   Deltat12, DNB_Parameters, EX, mu0, omega, t0, i, j)

# Equations (54)-(55)

for (i in 1:n) {
  H <- # Be careful: DNB uses the first 20,000 scenerios, no matter N
    - (1 / N) * sum(Simulation[, i + 1, 5] - Simulation[, i, 5]) +
    log(1 + CP_Inflatie[i]) * Deltat
  Simulation[, i + 1, 6] <-
    Simulation[, i, 6] + Simulation[, i + 1, 5] - Simulation[, i, 5] + H
}

rm(CP_Inflatie, i, H, Deltat)

BGEZ_X1 <- BGEZ_X2 <- BGEZ_X3 <- matrix(0.0, nrow = N, ncol = n / 12 + 1)
for (i in (1:(n / 12 + 1))) {
  BGEZ_X1[, i] <- Simulation[, i * 12 - 11, 1]
  BGEZ_X2[, i] <- Simulation[, i * 12 - 11, 2]
  BGEZ_X3[, i] <- Simulation[, i * 12 - 11, 3]
}

BGEZ_Prijsinflatie_EU <-
  BGEZ_Prijsinflatie_NL <-
  BGEZ_Aandelenrendement <- matrix(0.0, nrow = N, ncol = n / 12)
for (i in (1:(n / 12))) {
  BGEZ_Prijsinflatie_EU[, i] <-
    exp(Simulation[, i * 12 + 1, 5] - Simulation[, i * 12 - 11, 5]) - 1
  BGEZ_Prijsinflatie_NL[, i] <-
    exp(Simulation[, i * 12 + 1, 6] - Simulation[, i * 12 - 11, 6]) - 1
  BGEZ_Aandelenrendement[, i] <-
    exp(Simulation[, i * 12 + 1, 4] - Simulation[, i * 12 - 11, 4]) - 1
}

rm(i, n)

# Equation (14)

BGEZ_y <- array(0.0, dim = c(N, Taumax, (Tmax + 1)))
for (t in 1:(Tmax + 1)) {
  BGEZ_y[,, t] <- t(exp(-(DNB_Phi[, t] + DNB_Psi[, 1] %o% BGEZ_X1[, t] +
                            DNB_Psi[, 2] %o% BGEZ_X2[, t] +
                            DNB_Psi[, 3] %o% BGEZ_X3[, t]) / c(1:Taumax)) - 1)
  }

rm(t, DNB_Phi, DNB_Psi, BGEZ_X1, BGEZ_X2, BGEZ_X3)

compare <- function(A,
                    B,
                    nameA = deparse(substitute(A)),
                    nameB = deparse(substitute(B))) {
  qqplot(
    A,
    B,
    bty = "n",
    pch = 20,
    asp = 1,
    xlab = paste(nameA, ", quantiles", sep = ""),
    ylab = paste(nameB, ", quantiles", sep = ""),
    panel.first = rect(
      quantile(A, 0.005),
      quantile(B, 0.005),
      quantile(A, 0.995),
      quantile(B, 0.995),
      col = "lightgrey",
      border = NA
    )
  )
  text(
    quantile(A, 0.005),
    quantile(B, 0.995),
    "\n\n\n\n\nGrey area: between 0.5th\nand 99.5th percentile",
    pos = 4
  )
  abline(0, 1, lwd = 2, col = "red", lty = 2)
}

if (DNB) {
  compare(BGEZ_y[,10,2], DNB_y[,10,2])
}

end_time <- Sys.time()
end_time - start_time

rm(start_time, end_time)
