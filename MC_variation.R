# Monte Carlo Calculator

sim_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-19_03.15_simresults_0.1qm_200mc_5years_sd=0.05.csv", header = T, sep = ",")
mc_data <- as.matrix(as.numeric(sim_data[1826, -1]))

sd_v <- c()
cv_v <- c()
mean_cv <-c()

# calculate standard deviations for the following number of MC simulationsm = number of MC runs
for (m in 2:200){
  
  mean_v <- c()
  # choosing m rows randomly
  for (j in 1:500){
    rows <- sample(1:200, m, replace = FALSE)
    
    # get population sizes of these rows and save in vector "popsize_v"
    popsize_v <- c()
    for (i in 1:length(rows)){
      popsize_v[i] <- mc_data[rows[i], 1]
    }
    
    # save mean value of one MC run in mean_v
    mean_v[j] <- mean(popsize_v)
    
    # calculate sd for the means of the MC simulations
    sd_v[m] <- sd(mean_v)
  
    # calculate coefficient of variation (CV)
    mean_cv[m] <- mean(mean_v) # mean value of mean values of m MC runs to calculate cv
    cv_v[m] <- sd_v[m] / mean_cv[m]
  }
}


#plot(sd_v, type = "l", xlab = "Number of Monte-Carlo runs", ylab = "Standard deviation")
plot(cv_v, type = "l", xlab = "Number of Monte-Carlo runs [#]", ylab = "Coefficient of variation [-]")
#plot(mean_cv, type = "l", xlab = "Number of Monte-Carlo runs [#]", ylab = "Mean population size [#]")
# plot shows the mean CV (of 500 iterations) as a function of the number of MC runs

# save plot
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/MC_variation.pdf")
par(cex = 1.2)
plot(cv_v, type = "l", xlab = "number of Monte-Carlo iterations (#)", ylab = "coefficient of variation (-)", 
     cex.axis = 1.2, cex.lab = 1.2)
dev.off()
par(cex = 1.0)



