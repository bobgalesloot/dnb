### Check whether the first 20,000 scenarios in two DNB files (xlsx and csv)
### contain the same data
###
### Date: 19 March 2024
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

setwd("~/R")
filename_xlsx <- "cp2022-p-scenarioset-20k-2024q1.xlsx"
filename_csv <- "CP2022 P scenarioset 100K 2024Q1.csv"
N = 20000 # Number of scenarios >= 2 (<= 100000 if DNB)
Tmax <- 100 # Number of years >= 2
Taumax <- 100 # Number of maturities >= 2

library(openxlsx)

DNB_Phi_100k <- unname(as.matrix(read.csv(
  filename_csv,
  skip = 600000,
  nrows = min(100, Taumax),
  header = F
)))[, 1:(Tmax + 1)]
DNB_Psi_100k <- unname(as.matrix(read.csv(
  filename_csv,
  skip = 600100,
  nrows = min(100, Taumax),
  header = F
)))[, 1:3]
DNB_X1_100k <- unname(as.matrix(read.csv(
  filename_csv,
  nrows = min(100000, N), header = F
)))[, 1:(Tmax + 1)]
DNB_X2_100k <- unname(as.matrix(read.csv(
  filename_csv,
  skip = 100000,
  nrows = min(100000, N),
  header = F
)))[, 1:(Tmax + 1)]
DNB_X3_100k <- unname(as.matrix(read.csv(
  filename_csv,
  skip = 200000,
  nrows = min(100000, N),
  header = F
)))[, 1:(Tmax + 1)]
DNB_Aandelenrendement_100k <- unname(as.matrix(
  read.csv(
    filename_csv,
    skip = 300000,
    nrows = min(100000, N),
    header = F,
    sep = ",",
    dec = "."
  )
))[, 1:min(100, Tmax)]
DNB_Prijsinflatie_EU_100k <- unname(as.matrix(
  read.csv(
    filename_csv,
    skip = 400000,
    nrows = min(100000, N),
    header = F,
    sep = ",",
    dec = "."
  )
))[, 1:min(100, Tmax)]
DNB_Prijsinflatie_NL_100k <- unname(as.matrix(
  read.csv(
    filename_csv,
    skip = 500000,
    nrows = min(100000, N),
    header = F,
    sep = ",",
    dec = "."
  )
))[, 1:min(100, Tmax)]

DNB_Phi_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "7_Renteparameter_phi_N", colNames = F)
  ))[1:Taumax, 1:(Tmax + 1)]
DNB_Psi_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "8_Renteparameter_Psi_N", colNames = F)
  ))[1:Taumax,]
DNB_X1_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "1_Toestandsvariabele_1", colNames = F)
  ))[1:N, 1:(Tmax + 1)]
DNB_X2_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "2_Toestandsvariabele_2", colNames = F)
  ))[1:N, 1:(Tmax + 1)]
DNB_X3_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "3_Toestandsvariabele_3", colNames = F)
  ))[1:N, 1:(Tmax + 1)]
DNB_Aandelenrendement_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "4_Aandelenrendement", colNames = F)
  ))[1:N, 1:Tmax]
DNB_Prijsinflatie_EU_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "5_Prijsinflatie_EU", colNames = F)
  ))[1:N, 1:Tmax]
DNB_Prijsinflatie_NL_20k <-
  unname(as.matrix(
    read.xlsx(filename_xlsx, sheet = "6_Prijsinflatie_NL", colNames = F)
  ))[1:N, 1:Tmax]

max(
  abs(DNB_Aandelenrendement_100k - DNB_Aandelenrendement_20k),
  abs(DNB_Phi_100k - DNB_Phi_20k),
  abs(DNB_Prijsinflatie_EU_100k - DNB_Prijsinflatie_EU_20k),
  abs(DNB_Prijsinflatie_NL_100k - DNB_Prijsinflatie_NL_20k),
  abs(DNB_Psi_100k - DNB_Psi_20k),
  abs(DNB_X1_100k - DNB_X1_20k),
  abs(DNB_X2_100k - DNB_X2_20k),
  abs(DNB_X3_100k - DNB_X3_20k)
)

end_time <- Sys.time()
end_time - start_time
