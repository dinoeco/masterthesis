# compare model to data

setwd("/Users/dino/Dropbox/Uni/Master/Masterarbeit/")

# Data from Literature
#test.data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/gergs2014_0.5.csv", header = TRUE, sep = ";", dec = ".")
#test.data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/gergs2016_control_1.csv", header = TRUE, sep = ";")
test.data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/poptest_data.csv", header = TRUE, sep = ";", dec = ".")
#test.data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/daten_landau_2011.csv", header = TRUE, sep = ";", dec = ".")
#test.data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/starvation.csv", header = TRUE, sep = ";", dec = ".")

# Data from DEB Model
model.data <- mc.pop.quant

# Plot
if(mc.on == T){
  plot(c(0:days), mc.pop.quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", ylim = c(0,max(mc.pop.quant[5, ], test.data$R4)), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
  lines(c(0:days), mc.pop.quant[1, ], type = "l", lty = 2) # min 0% quantil
  #lines(c(0:days), mc.pop.quant[3, ], type = "l", lty = 1) # median
  lines(c(0:days), mc.pop.mean, type = "l", lty = 1) # mean
}
if(mc.on == F){
  plot(c(0:days), pop.size[,2], type = "l", lty = 1, xlab = "time (d)", ylab = "population size (#)", ylim = c(0,max(pop.size[,2], test.data$R4)),cex.axis = 1.2, cex.lab = 1.2)
}
# each of the following lines represent one replicate -> leave out lines if experiment had less replicates
if(ncol(test.data)-1 > 0) {lines(test.data$t, test.data$R1, type = "p")}
if(ncol(test.data)-1 > 1) {lines(test.data$t, test.data$R2, type = "p")}
if(ncol(test.data)-1 > 2) {lines(test.data$t, test.data$R3, type = "p")}
if(ncol(test.data)-1 > 3) {lines(test.data$t, test.data$R4, type = "p")}
if(ncol(test.data)-1 > 4) {lines(test.data$t, test.data$R5, type = "p")}
if(ncol(test.data)-1 > 5) {lines(test.data$t, test.data$R6, type = "p")}
if(ncol(test.data)-1 > 6) {lines(test.data$t, test.data$R7, type = "p")}
if(ncol(test.data)-1 > 7) {lines(test.data$t, test.data$R8, type = "p")}
if(ncol(test.data)-1 > 8) {lines(test.data$t, test.data$R9, type = "p")}
if(ncol(test.data)-1 > 9) {lines(test.data$t, test.data$R10, type = "p")}
if(ncol(test.data)-1 > 10) {lines(test.data$t, test.data$R11, type = "p")}
if(ncol(test.data)-1 > 11) {lines(test.data$t, test.data$R12, type = "p")}
















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
# text(days - days * 0.0, c(max(mc.pop.size) - 15,max(mc.pop.size) - 25,max(mc.pop.size) - 35, max(mc.pop.size) - 45),
#      labels = c(mc.no, steps, feed.sc[7], nse), cex = 0.8)
# text(days - days * 0.2, c(max(mc.pop.size) - 15,max(mc.pop.size) - 25,max(mc.pop.size) - 35, max(mc.pop.size) - 45),
#      labels = c("MC runs","Steps","Feeding", "NSE = "), cex = 0.8)








### mit Loop ####

#for (i in 1:(ncol(test.data)-1)) {
 # lines(test.data$t, paste0("test.data$R", i) , type = "p")
 # lines(paste0("test.data$t,", "test.data$R", "i" ,  ",type = "p""))
  #lines(test.data$t, test.data$R[[i]], type = "p")
#}


