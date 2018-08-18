# Evaluating Model - Nash–Sutcliffe model efficiency coefficient


# data from experiment
#obs_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/gergs2016_control_1.csv", header = TRUE, sep = ";")
#obs_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/gergs2014_0.5.csv", header = TRUE, sep = ";", dec = ".")
obs_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/poptest_data.csv", header = TRUE, sep = ";", dec = ".")
#obs_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/daten_landau_2011.csv", header = TRUE, sep = ";", dec = ".")
#obs_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/starvation.csv", header = TRUE, sep = ";", dec = ".")
#obs_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/daten_toxtest.csv", header = TRUE, sep = ";", dec = ".")

# data from deb model
#model_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/R_Masterarbeit/median_temp.csv", header = TRUE, sep = ",")
#model_data <- model_data[, -1]
model_data <- mc.pop.mean

# vector with observed time steps
ts <- obs_data[,1]

# modeled data for observed time steps only
#sim_data <- model_data[ts + 1,]
sim_data <- model_data[ts + 1]

# mean values for observed data (vector)
obs_data_mean <- apply(obs_data[, -1], 1, mean)
#obs_data_mean <- apply(obs_data[, -1], 1, median)
#obs_data_mean <- (obs_data[, -1])                    # für starvation daten

#obs_data_mean <- (obs_data[,5]) # für toxtest

# vector with observed/simulated value only
sim_data_v <- as.vector(sim_data[,2])
sim_data_v <- sim_data

# CALCULATION WITH NSE Function from hydroGOF Package

library("hydroGOF")
nse <- NSE(sim_data_v, obs_data_mean)
nse







# MANUAL CALCULATION

# zähler
zaehler <- c()
for (t in 1:length(sim)){
  zaehler[t] <- (sim[t] - obs[t]) ^ 2
  }
z_sum <- sum(zaehler)

# nenner
nenner <- c()
for (t in 1:length(sim)){
  nenner[t] <- (obs[t] - mean(obs)) ^ 2
}
n_sum <- sum(nenner)

E <- 1 - (z_sum / n_sum)
E




