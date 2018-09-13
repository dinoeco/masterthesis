# Recovery assessment - Auswertung und Plots


#### ------------------------ Plots of scenarios without exposure for all 3 food levels ----- ####


### Plot Control pointing days of application and 1st of October
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-09-09_14.58_simresults_MC=50_conc=0_peak=213_food=1_popsize.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/scenarios_control_applications.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     #ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
     ylim = c(0,400), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
# arrows pointing at days of application
arrows(c(213,273,343), c(242,264,299), x1 = c(213,273,343), y1 = c(172, 194, 229), length = 0.20, angle = 30,
       code = 2, lwd = 2, lty = 1)
# arrows pointing at 1st October
arrows(c(0,365,365*2,365*3,365*4), c(50,160,130,130,110), x1 = c(0,365,365*2,365*3,365*4), y1 = c(90,200,180,180,160), length = 0.25, angle = 30,
       code = 2, lwd = 2, lty = 3)
text(1500, 350, "'normal food'", cex = 1.2)
dev.off()
par(cex = 1.0)


### Plot 1.2_0_213 (high food scenario control)
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-09-12_02.26_simresults_MC=50_conc=0_peak=213_food=1.2_popsize.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/scenarios_control_high_food.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     #ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
     ylim = c(0,400), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
# arrows pointing at days of application
arrows(c(213,273,343), c(242,264,299), x1 = c(213,273,343), y1 = c(172, 194, 229), length = 0.20, angle = 30,
       code = 2, lwd = 2, lty = 1)
# arrows pointing at 1st October
arrows(c(0,365,365*2,365*3,365*4), c(50,160,140,140,140), x1 = c(0,365,365*2,365*3,365*4), y1 = c(90,200,190,190,190), length = 0.25, angle = 30,
       code = 2, lwd = 2, lty = 3)
text(1500, 350, "'high food'", cex = 1.2)
dev.off()
par(cex = 1.0)


### Plot 0.8_0_213 (low food scenario control)
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-09-10_21.34_simresults_MC=50_conc=0_peak=213_food=0.8_popsize.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/scenarios_control_low_food.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     #ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
     ylim = c(0,400), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
# arrows pointing at days of application
arrows(c(213,273,343), c(242,264,299), x1 = c(213,273,343), y1 = c(172, 194, 229), length = 0.20, angle = 30,
       code = 2, lwd = 2, lty = 1)
# arrows pointing at 1st October
arrows(c(0,365,365*2,365*3,365*4), c(50,160,130,100,50), x1 = c(0,365,365*2,365*3,365*4), y1 = c(90,200,180,150,100), length = 0.25, angle = 30,
       code = 2, lwd = 2, lty = 3)
text(1500, 350, "'low food'", cex = 1.2)
dev.off()
par(cex = 1.0)


#### ---------------------- Plots for timing -------------------------------- ####


### Plot 1_0.7_213 (spring)
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-09-10_16.36_simresults_MC=50_conc=0.7_peak=213_food=1_popsize.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/scenarios_1_0.7_213.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     #ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
     ylim = c(0,400), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
# arrows pointing at days of application
arrows(c(213), c(242), x1 = c(213), y1 = c(172), length = 0.20, angle = 30,
       code = 2, lwd = 2, lty = 1)
# arrows pointing at 1st October
#arrows(c(0,365,365*2,365*3,365*4), c(50,160,130,100,50), x1 = c(0,365,365*2,365*3,365*4), y1 = c(90,200,180,150,100), length = 0.25, angle = 30,
 #      code = 2, lwd = 2, lty = 3)
text(1500, 350, "spring", cex = 1.2)
dev.off()
par(cex = 1.0)


### Plot 1_0.7_273 (summer)
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-09-10_18.41_simresults_MC=50_conc=0.7_peak=273_food=1_popsize.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/scenarios_1_0.7_273.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     #ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
     ylim = c(0,400), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
# arrows pointing at days of application
arrows(c(273), c(264), x1 = c(273), y1 = c(194), length = 0.20, angle = 30,
       code = 2, lwd = 2, lty = 1)
# arrows pointing at 1st October
#arrows(c(0,365,365*2,365*3,365*4), c(50,160,130,100,50), x1 = c(0,365,365*2,365*3,365*4), y1 = c(90,200,180,150,100), length = 0.25, angle = 30,
#       code = 2, lwd = 2, lty = 3)
text(1500, 350, "summer", cex = 1.2)
dev.off()
par(cex = 1.0)


### Plot 1_0.7_343 (autumn)
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-09-10_20.27_simresults_MC=50_conc=0.7_peak=343_food=1_popsize.csv", header = TRUE, sep = ",", dec = ".")
# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)

pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/scenarios_1_0.7_343.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     #ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
     ylim = c(0,400), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
# arrows pointing at days of application
arrows(c(343), c(299), x1 = c(343), y1 = c(229), length = 0.20, angle = 30,
       code = 2, lwd = 2, lty = 1)
# arrows pointing at 1st October
#arrows(c(0,365,365*2,365*3,365*4), c(50,160,130,100,50), x1 = c(0,365,365*2,365*3,365*4), y1 = c(90,200,180,150,100), length = 0.25, angle = 30,
#       code = 2, lwd = 2, lty = 3)
text(1500, 350, "autumn", cex = 1.2)
dev.off()
par(cex = 1.0)





#### ------------------ Plots for appendix --------------- ####


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


