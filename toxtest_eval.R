# Toxtest - Auswertung und Plots

# load data
d.immob <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Labortests/2_Toxtest/toxtest_data_immob.csv", header = TRUE, sep = "", dec = ".")
d.mort <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Labortests/2_Toxtest/toxtest_data_mort.csv", header = TRUE, sep = "", dec = ".")

# plot immobile individuals
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/toxtest_immob.pdf")
par(cex = 1.6)
plot(d.immob$time, d.immob$X0, type = "b", lty = 1, pch = 1, ylim = c(0,30), xlim = c(0,100), xlab = "time (h)", ylab = "individuals mobile (#)")
lines(d.immob$time, d.immob$X1.86, type = "b", lty = 1, pch = 2)
lines(d.immob$time, d.immob$X4.34, type = "b", lty = 1, pch = 3)
lines(d.immob$time, d.immob$X8.25, type = "b", lty = 1, pch = 4)
lines(d.immob$time, d.immob$X10.78, type = "b", lty = 1, pch = 5)
lines(d.immob$time, d.immob$X13.48, type = "b", lty = 1, pch = 6)
legend(70,18, legend = c("control", "1.86 µg/L","4.34 µg/L", "8.25 µg/L", "10.78 µg/L", "13.48 µg/L"), pch = c(1,2,3,4,5,6), 
       cex = 0.8, y.intersp = 1.0, horiz = F, bty = "n")
dev.off()
par(cex = 1.0)

# plot mortality
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/toxtest_mortality.pdf")
par(cex = 1.6)
plot(d.mort$time, d.mort$X0, type = "b", lty = 1, pch = 1, ylim = c(0,30), xlim = c(0,100), xlab = "time (h)", ylab = "individuals alive (#)")
lines(d.mort$time, d.mort$X1.86, type = "b", lty = 1, pch = 2)
lines(d.mort$time, d.mort$X4.34, type = "b", lty = 1, pch = 3)
lines(d.mort$time, d.mort$X8.25, type = "b", lty = 1, pch = 4)
lines(d.mort$time, d.mort$X10.78, type = "b", lty = 1, pch = 5)
lines(d.mort$time, d.mort$X13.48, type = "b", lty = 1, pch = 6)
legend(0,18, legend = c("control", "1.86 µg/L","4.34 µg/L", "8.25 µg/L", "10.78 µg/L", "13.48 µg/L"), pch = c(1,2,3,4,5,6), 
       cex = 0.8, y.intersp = 1.0, horiz = F, bty = "n")
dev.off()
par(cex = 1.0)


#### EC50 and LC 50 ####
library("readxl")
require(drc)
require(plotrix)

# EC50 - 48 h
daten.ec48 <- read_xlsx("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Labortests/2_Toxtest/2_Toxtest.xlsx", sheet="EC50_48h")
model.ec48 <- drm(prop.mobile~conc, data=daten.ec48, fct=LL.4())
mselect(model.ec48, list(LN.2(),LN.3(),LN.4(),LL.2(),LL.3(),LL.3u(),LL.4(), LL.5(), W1.2(),W1.3(),W1.4(),W2.2(), W2.3(),W2.4()))
best_model.ec48 <- drm(prop.mobile~conc, data=daten.ec48, fct=W1.2())
# plot
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/tox_ec50_48h.pdf")
par(cex = 1.6)
plot(best_model.ec48, xlab = "concentration (µg/L)", ylab = "proportion mobile")
ED.ec48 <- ED(best_model.ec48, 50, interval="delta")
points(ED.ec48[1], max(predict(best_model.ec48))/2,pch=19, cex=1.5)
plotCI(ED.ec48[1], max(predict(best_model.ec48))/2,pch=32, ui=ED.ec48[4], li=ED.ec48[3], err="x", add=T)
dev.off()
par(cex = 1.0)

# EC50 - 96h
daten.ec96 <- read_xlsx("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Labortests/2_Toxtest/2_Toxtest.xlsx", sheet="EC50_96h")
model.ec96 <- drm(prop.mobile~conc, data=daten.ec96, fct=LL.4())
mselect(model.ec96, list(LN.2(),LN.3(),LN.4(),LL.2(),LL.3(),LL.3u(),LL.4(), LL.5(), W1.2(),W1.3(),W1.4(),W2.2(), W2.3(),W2.4()))
best_model.ec96 <- drm(prop.mobile~conc, data=daten.ec96, fct=W1.2())
# plot
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/tox_ec50_96h.pdf")
par(cex = 1.6)
plot(best_model.ec96, xlab = "concentration (µg/L)", ylab = "proportion mobile")
ED.ec96 <- ED(best_model.ec96,50,interval="delta")
points(ED.ec96[1], max(predict(best_model.ec96))/2, pch=19, cex=1.5)
plotCI(ED.ec96[1], max(predict(best_model.ec96))/2, pch=32, ui=ED.ec96[4], li=0.1, err="x", add=T) # li changed manually as a negative limit is not plotted 
dev.off()
par(cex = 1.0)


# LC50 - 48h
daten.lc48 <- read_xlsx("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Labortests/2_Toxtest/2_Toxtest.xlsx", sheet="LC50_48h")
model.lc48 <- drm(prop.alive~conc, data=daten.lc48, fct=LL.4())
mselect(model.lc48, list(LN.2(),LN.3(),LN.4(),LL.2(),LL.3(),LL.3u(),LL.4(), LL.5(), W1.2(),W1.3(),W1.4(),W2.2(), W2.3(),W2.4()))
best_model.lc48 <- drm(prop.alive~conc, data=daten.lc48, fct=W2.2())
# plot
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/tox_lc50_48h.pdf")
par(cex = 1.6)
plot(best_model.lc48, xlab = "concentration (µg/L)", ylab = "proportion alive")
ED.lc48 <- ED(best_model.lc48,50,interval="delta")
points(ED.lc48[1], max(predict(best_model.lc48))/2, pch=19, cex=1.5)
plotCI(ED.lc48[1], max(predict(best_model.lc48))/2, pch=32, ui=ED.lc48[4], li=ED.lc48[3], err="x", add=T)
dev.off()
par(cex = 1.0)


# LC50 - 96h
daten.lc96 <- read_xlsx("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Labortests/2_Toxtest/2_Toxtest.xlsx", sheet="LC50_96h")
model.lc96 <- drm(prop.alive~conc, data=daten.lc96, fct=LL.4())
mselect(model.lc96, list(LN.2(),LN.3(),LN.4(),LL.2(),LL.3(),LL.3u(),LL.4(), LL.5(), W1.2(),W1.3(),W1.4(),W2.2(), W2.3(),W2.4()))
best_model.lc96 <- drm(prop.alive~conc, data=daten.lc96, fct=LL.3u())
# plot
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/tox_lc50_96h.pdf")
par(cex = 1.6)
plot(best_model.lc96, xlab = "concentration (µg/L)", ylab = "proportion alive")
ED.lc96 <- ED(best_model.lc96,50,interval="delta")
points(ED.lc96[1], max(predict(best_model.lc96))/2, pch=19, cex=1.5)
plotCI(ED.lc96[1], max(predict(best_model.lc96))/2, pch=32, ui=ED.lc96[4], li=ED.lc96[3], err="x", add=T)
dev.off()
par(cex = 1.0)




