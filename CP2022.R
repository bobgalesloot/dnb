### This is material illustrating the methods from:
###   Advies Commissie Parameters
###   29 november 2022
###   Technical Appendix: Specification of the CP2022 Model
###   9.1 Simulation under P
###

### Date: 22 August 2025
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
### © BGEZ 2025
###
### Press cmd + shift + return to run all

install.packages("openxlsx")
install.packages("data.table")

rm(list = ls(all = TRUE))
gc()
set.seed(42)

DNB <- TRUE # Use DNB scenarios or not
RDP <- FALSE # Fix "rentedip" or not

setwd("~/R")

filename_xlsx <- "cp2022-p-scenarioset-20k-2025q3.xlsx"
filename_csv <- "CP2022 P scenarioset 100K 2025Q3.csv"

N <- 50000 # Number of scenarios >= 2 (<= 100000 if DNB)
Tmax <- 10 # Number of years >= 2
Taumax <- 30 # Number of maturities >= 2
Months <- 12 # 12
CPB_Inflatie <- c(
  rep(0.032, 6 * Months / 12), # Number of months remaining in the year
  rep(0.026, Months),
  rep(0.025, Months),
  rep(0.021, Months),
  rep(0.021, Months),
  rep(0.021, Months),
  rep(0.021, Months),
  rep(0.021, Months),
  rep(0.021, Months)
)

if (Tmax * Months <= length(CPB_Inflatie)) {
  CP_Inflatie <- CPB_Inflatie[1:(Tmax * Months)]
} else {
  CP_Inflatie <- c(CPB_Inflatie,
                   rep(0.02, Tmax * Months - length(CPB_Inflatie)))
}
rm(CPB_Inflatie)

cat("Reading quarterly data\n")

read_csv_block <- function(file, skip, nrows, cols) {
  cat("Reading", file, "from line",
      format(skip + 1, scientific = FALSE, big.mark = ","), "for", 
      format(nrows, scientific = FALSE, big.mark = ","), "rows and", cols, "columns\n")
  unname(as.matrix(
    fread(file, skip = skip, nrows = nrows, header = FALSE,
          select = seq_len(cols))))
}

library(openxlsx)

DNB_Parameters <-
  as.numeric(as.matrix(read.xlsx(
    filename_xlsx, sheet = "0_Parameters"))[1:47, 2])
if (N > 20000) {
  if (DNB) {
    library(data.table) # for fread
    DNB_X1 <- read_csv_block(filename_csv, 0, min(100000, N), Tmax + 1)
    DNB_X2 <- read_csv_block(filename_csv, 100000, min(100000, N), Tmax + 1)
    DNB_X3 <- read_csv_block(filename_csv, 200000, min(100000, N), Tmax + 1)
    DNB_Aandelenrendement <-
      read_csv_block(filename_csv, 300000, min(100000, N), min(100, Tmax))
    DNB_Prijsinflatie_EU <-
      read_csv_block(filename_csv, 400000, min(100000, N), min(100, Tmax))
    DNB_Prijsinflatie_NL <-
      read_csv_block(filename_csv, 500000, min(100000, N), min(100, Tmax))
  }
  DNB_Phi <- read_csv_block(filename_csv, 600000, min(100, Taumax), Tmax + 1)
  DNB_Psi <- read_csv_block(filename_csv, 600100, min(100, Taumax), 3)
} else {
  if (DNB) {
    DNB_X1 <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "1_Toestandsvariabele_1",
                  colNames = FALSE)))[1:N, 1:(Tmax + 1)]
    DNB_X2 <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "2_Toestandsvariabele_2",
                  colNames = FALSE)))[1:N, 1:(Tmax + 1)]
    DNB_X3 <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "3_Toestandsvariabele_3",
                  colNames = FALSE)))[1:N, 1:(Tmax + 1)]
    DNB_Aandelenrendement <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "4_Aandelenrendement",
                  colNames = FALSE)))[1:N, 1:Tmax]
    DNB_Prijsinflatie_EU <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "5_Prijsinflatie_EU",
                  colNames = FALSE)))[1:N, 1:Tmax]
    DNB_Prijsinflatie_NL <-
      unname(as.matrix(
        read.xlsx(filename_xlsx, sheet = "6_Prijsinflatie_NL",
                  colNames = FALSE)))[1:N, 1:Tmax]
  }
  DNB_Phi <-
    unname(as.matrix(
      read.xlsx(filename_xlsx, sheet = "7_Renteparameter_phi_N", colNames = FALSE)
    ))[1:Taumax, 1:(Tmax + 1)]
  DNB_Psi <-
    unname(as.matrix(
      read.xlsx(filename_xlsx, sheet = "8_Renteparameter_Psi_N", colNames = FALSE)
    ))[1:Taumax,]
}

rm(filename_csv, filename_xlsx, read_csv_block)

# Fix "rentedip"

if (RDP) {
  cat("Rentedip\n")
  DNB_Phi_Star <- DNB_Phi
  for (t in 2:(Tmax - 1)){
    DNB_Phi_Star[, t] <- (DNB_Phi[, t + 1] + DNB_Phi_Star[, t - 1]) / 2
  }
  DNB_Phi_Star[, Tmax] <- DNB_Phi_Star[, Tmax - 1]
  DNB_Phi <- DNB_Phi_Star
  rm(DNB_Phi_Star)
}

# Equation (14)

if (DNB) {
  cat("Equation (14)\n")
  DNB_y <- array(0.0, dim = c(N, Taumax, Tmax + 1))
  for (t in seq_len(Tmax + 1)) 
    DNB_y[, , t] <- t(expm1(
      -(DNB_Phi[, t] +
          outer(DNB_Psi[, 1], DNB_X1[, t]) +
          outer(DNB_Psi[, 2], DNB_X2[, t]) +
          outer(DNB_Psi[, 3], DNB_X3[, t])) /
        seq_len(Taumax)))
  rm(t, DNB_X1, DNB_X2, DNB_X3)
}

# Equation (3)
cat("Equation (3)\n")

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
cat("Equation (5)\n")

SigmaSPi <- rbind(DNB_Parameters[35:39], DNB_Parameters[40:44])
mu0 <- DNB_Parameters[33:34] - (1 / 2) *
  diag((SigmaSPi %*% Lambda0) %*% t(SigmaSPi))

# Equation (6)
cat("Equation (6)\n")

K0 <- rbind(c(0, 1, 0),
            c(0, 0, 1)) -
  (1 / 2) * diag((SigmaSPi %*% Lambda) %*% t(SigmaSPi)) %*% t(c(1, 0, 0))

n <- Tmax * Months
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
cat("Equation (51)\n")

f_Andersen <- function(Vinst) {
  m <- Vlong_Andersen + (Vinst - Vlong_Andersen) * k1_Andersen
  twopsihat <- 2 * (m * m) / (Vinst * k2_Andersen + k3_Andersen)
  b2 <- twopsihat - 1 + sqrt(twopsihat * (twopsihat - 1))
  rand <- sqrt(b2) + rnorm(1)
  m * rand * rand / (1 + b2)
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

denom_prefactor <- 1 / (omega * sqrt(Deltat))
for (j in 1:N) {
  for (i in 1:n) {
    Vold <- Simulation[j, i, 1]
    Vnew <- f_Andersen(Vinst = Vold)
    Simulation[j, i + 1, 1] <- Vnew
    Simulation[j, i, 7] <- (Vnew - Vold - kappa_Andersen * (Vlong_Andersen - Vold) * Deltat) * 
      denom_prefactor / sqrt(Vold)
  }
}
           
rm(f_Andersen, Vlong_Andersen, kappa_Andersen, epsilon_Andersen, dT_Andersen,
   k1_Andersen, k2_Andersen, k3_Andersen, i, j, Vold, Vnew, denom_prefactor)

# Equations (52)-(53)
cat("Equations (52)-(53)\n")

LeftM <- cbind(c(0, 0, 0, 0), diag(4))
Sigma <- rbind(Sigmarpi, SigmaSPi)
Deltat12 <- sqrt(Deltat)
randmat <- matrix(rnorm(N * n * 4), nrow = N, ncol = n * 4)
for (j in 1:N) {
  for (i in 1:n) {
    X <- Simulation[j, i, 1:3]
    Lpart <- rbind(K %*% (EX - X), mu0 + (K0 %*% X)) * Deltat
    diag_values <- sqrt(diag(Lambda0 + Simulation[j, i, 1] * Lambda)) * Deltat12
    randvec <- c(Simulation[j, i, 7], randmat[j, (i-1)*4 + 1:4])
    Rpart <- sweep(Sigma, 2, diag_values, "*") %*% randvec
    Simulation[j, i + 1, 2:5] <- Simulation[j, i, 2:5] + LeftM %*% (Lpart + Rpart)
  }
}

rm(K, K0, Lambda, Lambda0, LeftM, Lpart, Rpart, Sigma, Sigmarpi, SigmaSPi,
   Deltat12, DNB_Parameters, EX, mu0, omega, t0, i, j, randmat, randvec, diag_values, X)

# Equations (54)-(55)
cat("Equations (54)-(55)\n")

for (i in 1:n) {
  H <- # Be careful: DNB uses the first 20,000 scenerios, no matter N
    - (1 / N) * sum(Simulation[, i + 1, 5] - Simulation[, i, 5]) +
    log1p(CP_Inflatie[i]) * Deltat
  Simulation[, i + 1, 6] <-
    Simulation[, i, 6] + Simulation[, i + 1, 5] - Simulation[, i, 5] + H
}

rm(CP_Inflatie, i, H, Deltat)

BGEZ_X1 <- BGEZ_X2 <- BGEZ_X3 <- matrix(0.0, nrow = N, ncol = n / Months + 1)
for (i in (1:(n / Months + 1))) {
  BGEZ_X1[, i] <- Simulation[, i * Months - (Months - 1), 1]
  BGEZ_X2[, i] <- Simulation[, i * Months - (Months - 1), 2]
  BGEZ_X3[, i] <- Simulation[, i * Months - (Months - 1), 3]
}

BGEZ_Prijsinflatie_EU <-
  BGEZ_Prijsinflatie_NL <-
  BGEZ_Aandelenrendement <- matrix(0.0, nrow = N, ncol = n / Months)
for (i in (1:(n / Months))) {
  BGEZ_Prijsinflatie_EU[, i] <-
    expm1(Simulation[, i * Months + 1, 5] - Simulation[, i * Months - (Months - 1), 5])
  BGEZ_Prijsinflatie_NL[, i] <-
    expm1(Simulation[, i * Months + 1, 6] - Simulation[, i * Months - (Months - 1), 6])
  BGEZ_Aandelenrendement[, i] <-
    expm1(Simulation[, i * Months + 1, 4] - Simulation[, i * Months - (Months - 1), 4])
}

rm(i, n, Simulation)

# Equation (14)
cat("Equation (14)\n")

BGEZ_y <- array(0.0, dim = c(N, Taumax, Tmax + 1))
for (t in seq_len(Tmax + 1))
  BGEZ_y[, , t] <- t(expm1(
    -(DNB_Phi[, t] +
      outer(DNB_Psi[, 1], BGEZ_X1[, t]) +
      outer(DNB_Psi[, 2], BGEZ_X2[, t]) +
      outer(DNB_Psi[, 3], BGEZ_X3[, t])) /
      seq_len(Taumax)))

rm(t, DNB_Phi, DNB_Psi, BGEZ_X1, BGEZ_X2, BGEZ_X3)

compare <- function(A, B,
                    nameA = deparse(substitute(A)),
                    nameB = deparse(substitute(B))) {
  qqplot(
    A, B, bty = "n", pch = 20, asp = 1,
    xlab = paste(nameA, ", quantiles", sep = ""),
    ylab = paste(nameB, ", quantiles", sep = ""),
    panel.first = rect(
      quantile(A, 0.001),
      quantile(B, 0.001),
      quantile(A, 0.999),
      quantile(B, 0.999),
      col = "lightgrey",
      border = NA))
  title(paste("BGEZ and DNB P-Scenarios 2025Q3\n(2 ×",
              format(nrow(A), scientific = FALSE, big.mark = ","), "×", ncol(A), "simulated years)"))
  abline(0, 1, lwd = 2, col = "red", lty = 2)
  text(
    quantile(A, 0.001),
    quantile(B, 0.999),
    "\n\n\n\n\n\nGrey area: between\n0.1th and 99.9th\npercentiles",
    pos = 4, col = "black"
  )
}

if (DNB) {
  compare(BGEZ_Aandelenrendement, DNB_Aandelenrendement)
}
