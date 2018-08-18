# Recovery assessment - Auswertung und Plots

#### Plot with 0.25qm 50MC 10 years
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-15_00.40_simresults_0.25qm_50mc_10years.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_0.25qm_50mc_10years_foodx0.63_control.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
#lines(c(0:(nrow(sim_results)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)


#### Plot with 0.1qm 50MC 10 years
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-15_13.25_simresults_0.1qm_50mc_10years.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_0.1qm_50mc_10years_foodx0.63_control.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
#lines(c(0:(nrow(sim_results)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)


#### Plot showing 5years 50 MC 0.25qm
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-15_00.40_simresults_0.25qm_50mc_10years.csv", header = TRUE, sep = ",", dec = ".")
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)
sim_quant <- sim_quant[, 1:(365*5)]
sim_mean <- sim_mean[1:(365*5)]
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_50mc_5years_foodx0.63_control.pdf")
par(cex = 1.2)
plot(c(0:(ncol(sim_quant)-1)), sim_quant[5,], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
#lines(c(0:(ncol(sim_quant)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(ncol(sim_quant)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)

# Plot showing 5years 20 MC 0.25qm
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/R_Masterarbeit/2018-06-15_14.04_simresults_MC=20.csv", header = TRUE, sep = ",", dec = ".")
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_20mc_5years_foodx0.63_control.pdf")
par(cex = 1.2)
plot(c(0:(ncol(sim_quant)-1)), sim_quant[5,], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_quant[5, ],1500)), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
#lines(c(0:(ncol(sim_quant)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(ncol(sim_quant)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)

# Plot showing 5years 10 MC 0.25qm
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/R_Masterarbeit/2018-06-15_11.33_simresults_MC=10.csv", header = TRUE, sep = ",", dec = ".")
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_10mc_5years_foodx0.63_control.pdf")
par(cex = 1.2)
plot(c(0:(ncol(sim_quant)-1)), sim_quant[5,], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_quant[5, ],1500)), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
#lines(c(0:(ncol(sim_quant)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(ncol(sim_quant)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)



#### Figure A5 ####
# different number of MC simulations

# Plot showing 5years 200 MC 0.1qm sd = 0.05
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-19_03.15_simresults_0.1qm_200mc_5years_sd=0.05.csv", header = TRUE, sep = ",", dec = ".")
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_200mc_5years_0.1qm_foodx0.63_control_sd=0.05.pdf")
par(cex = 1.2)
plot(c(0:(ncol(sim_quant)-1)), sim_quant[5,], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_max,700)), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[2, ], type = "l", lty = 3) # 25% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[4, ], type = "l", lty = 3) # 75% quantil
#lines(c(0:(ncol(sim_quant)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(ncol(sim_quant)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)


# Plot showing 5years 50 MC 0.1qm sd = 0.05
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-19_12.13_simresults_0.1qm_50mc_5years_sd=0.05.csv", header = TRUE, sep = ",", dec = ".")
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_50mc_5years_0.1qm_foodx0.63_control_sd=0.05.pdf")
par(cex = 1.2)
plot(c(0:(ncol(sim_quant)-1)), sim_quant[5,], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_max,700)), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[2, ], type = "l", lty = 3) # 25% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[4, ], type = "l", lty = 3) # 75% quantil
#lines(c(0:(ncol(sim_quant)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(ncol(sim_quant)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)


# Plot showing 5years 20 MC 0.1qm sd = 0.05
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-19_12.54_simresults_0.1qm_20mc_5years_sd=0.05.csv", header = TRUE, sep = ",", dec = ".")
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_20mc_5years_0.1qm_foodx0.63_control_sd=0.05.pdf")
par(cex = 1.2)
plot(c(0:(ncol(sim_quant)-1)), sim_quant[5,], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_max,700)), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[2, ], type = "l", lty = 3) # 25% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[4, ], type = "l", lty = 3) # 75% quantil
#lines(c(0:(ncol(sim_quant)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(ncol(sim_quant)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)


# Plot showing 5years 10 MC 0.1qm sd = 0.05
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-19_22.15_simresults_0.1qm_10mc_5years_sd=0.05.csv", header = TRUE, sep = ",", dec = ".")
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_sim_10mc_5years_0.1qm_foodx0.63_control_sd=0.05.pdf")
par(cex = 1.2)
plot(c(0:(ncol(sim_quant)-1)), sim_quant[5,], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_max,700)), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[2, ], type = "l", lty = 3) # 25% quantil
lines(c(0:(ncol(sim_quant)-1)), sim_quant[4, ], type = "l", lty = 3) # 75% quantil
#lines(c(0:(ncol(sim_quant)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(ncol(sim_quant)-1)), sim_mean, type = "l", lty = 1) # mean
dev.off()
par(cex = 1.0)

