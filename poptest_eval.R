# Population experiment - Auswertung und Plots

# read data from asellus simulation 
sim_results <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Results_sim/Population/2018_01_26_result_sim_final_1000mc.csv", header = TRUE, sep = ",", dec = ".")
# read data from population experiment (laboratory data)
test.data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/poptest_data.csv", header = TRUE, sep = ";", dec = ".")

# evaluation of simulated data
sim_mean <- rowMeans(sim_results[,-1], na.rm = TRUE)
sim_min <- apply(sim_results[,-1], 1, min, na.rm = TRUE)
sim_max <- apply(sim_results[,-1], 1, max, na.rm = TRUE)
sim_quant <- apply(sim_results[,-1], 1, quantile, na.rm = TRUE)


# Plot
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/evaluation_poptest_comp.pdf")
par(cex = 1.2)
plot(c(0:(nrow(sim_results)-1)), sim_quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", 
     ylim = c(0,max(sim_quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
lines(c(0:(nrow(sim_results)-1)), sim_quant[1, ], type = "l", lty = 2) # min 0% quantil
#lines(c(0:(nrow(sim_results)-1)), sim_quant[3, ], type = "l", lty = 1) # median
lines(c(0:(nrow(sim_results)-1)), sim_mean, type = "l", lty = 1) # mean
lines(test.data$t, test.data$R1, type = "p")
lines(test.data$t, test.data$R2, type = "p")
lines(test.data$t, test.data$R3, type = "p")
lines(test.data$t, test.data$R4, type = "p")
lines(test.data$t, test.data$R5, type = "p")
lines(test.data$t, test.data$R6, type = "p")
lines(test.data$t, test.data$R7, type = "p")
lines(test.data$t, test.data$R8, type = "p")
lines(test.data$t, test.data$R9, type = "p")
lines(test.data$t, test.data$R10, type = "p")
lines(test.data$t, test.data$R11, type = "p")
lines(test.data$t, test.data$R12, type = "p")
dev.off()
par(cex = 1.0)






