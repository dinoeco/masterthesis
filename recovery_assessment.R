# Assessment of recovery and effect size 
# from scenarios of the asellus model


# -------------------------------------- Recovery ------------------------------------------
# load d_frame file containing all results
d_frame_1 <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-08-13_18.18_d_frame.csv", header = TRUE, sep = ",", dec = ".")
d_frame_2 <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-08-14_21.22_d_frame.csv", header = TRUE, sep = ",", dec = ".")
d_frame <- c()
d_frame <- rbind(d_frame_1, d_frame_2)

d_frame <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/Results/2018-09-13_08.17_d_frame.csv", header = TRUE, sep = ",", dec = ".")
d_frame <- d_frame[, -1]

# delete useless scenarios
d_frame <- d_frame[d_frame$treat != '1_0_273', ]
d_frame <- d_frame[d_frame$treat != '1_0_343', ]
d_frame <- d_frame[d_frame$treat != '0.8_0_273', ]
d_frame <- d_frame[d_frame$treat != '0.8_0_343', ]
d_frame <- d_frame[d_frame$treat != '1.2_0_273', ]
d_frame <- d_frame[d_frame$treat != '1.2_0_343', ]

d_frame <- subset(d_frame, cw_ini < 1)

d_frame$treat <- factor(d_frame$treat)


# plot all scenarios
pdf(file="/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/all_scenarios_plot_neu.pdf", width = 6, height = 9)
par(mfcol = c(8,5), mar = c(2,2,1,1))
for (i in 1:length(levels(d_frame$treat))) {
  if(i == 14){plot(d_frame[d_frame$treat == levels(d_frame$treat)[i], 2], type = "l", main = "control", ylim = c(0,max(d_frame$pop.size)))} # mark control scenario
  else{
    plot(d_frame[d_frame$treat == levels(d_frame$treat)[i], 2], type = "l", main = levels(d_frame$treat)[i], xlab = "time (d)", ylab = "population size (#)", ylim = c(0,max(d_frame$pop.size)))
  }
}
dev.off()
par(mfcol = c(1,1), mar = c(5, 4, 4, 2) + 0.1) # restore default values


## Recovery Quotient for total population size
# vector with control data (scenario 1-0-213)
control_d <- rep(d_frame[1:(1826), 2], length(levels(d_frame$treat))) ### 1826 must be changed according to the number of days simulated!!!!!!!!!!!!!!!!!!!!!!!!
# calculate recovery quotients for each time step and each treatment
rec_q <- c()
for (r in 1:nrow(d_frame)) {
  rec_q[r] <- d_frame[r,2] / control_d[r]
}
d_frame <- cbind(d_frame, rec_q)


# find day when recovery quotient falls below lower limit of recovery quotient (= 0.9)
rec_quot <- 0.9
# subset d3 to find recovery after application of pesticide
d_frame_sub <-c()
d_frame_sub_1 <- d_frame[d_frame$tox_start == 213, ]
d_frame_sub_1 <- subset(d_frame_sub_1, day > 213)

d_frame_sub_2 <- d_frame[d_frame$tox_start == 273, ]
d_frame_sub_2 <- subset(d_frame_sub_2, day > 273)

d_frame_sub_3 <- d_frame[d_frame$tox_start == 343, ]
d_frame_sub_3 <- subset(d_frame_sub_3, day > 343)

d_frame_sub <- rbind(d_frame_sub_1, d_frame_sub_2, d_frame_sub_3)

v1_d3 <- c()
v1_d3 <- do.call("rbind", (as.list(by(d_frame_sub[,7], factor(d_frame_sub$treat), function(x) which(x < rec_quot)[1]))))
v1_d3[is.na(v1_d3)] <- 0

# time until recovery quotient reaches >=0.9 again = recovery time
rec_time3 <- data.frame(matrix(data = NA, ncol = 2, nrow = length(v1_d3)))
names(rec_time3) <- c("Rec_time", "treat")
temp_rec3 <- NULL

# loop for each treatment
for(i in 1:length(levels(d_frame_sub$treat))) {  
  temp_rec3 <- d_frame_sub[d_frame_sub$treat == levels(d_frame_sub$treat)[i],7]
  rec_time3[i,1] <- which(temp_rec3[v1_d3[i]:length(temp_rec3)] >= rec_quot)[1]
  rec_time3[i,2] <- levels(d_frame_sub$treat)[i]
  
  # replace 1 with 0  (recovery quotient always > 0.9 -> only slight or no effects)
  if((rec_time3[i,1] == 1) & (is.na(rec_time3[i,1]) == FALSE)){
    rec_time3[i,1] <- 0
  }
}     

# merge results
final <- cbind(rec_time3[,2], rec_time3[,1] )
colnames(final) <- c("treat", "rec_time")
# add treatment columns (needed for plots and analysis)
food.c <- c(rep(0.8, 13), rep(1.0, 13), rep(1.2, 13))
cw_ini.c <- rep(c(rep(0, 1), rep(0.05, 3), rep(0.135, 3), rep(0.3, 3),  rep(0.7, 3)), 3)
tox_start.c <- rep(c(rep(213,1), rep(c(213,273,343),4)), 3)
final <- cbind(final, food.c, cw_ini.c, tox_start.c)
colnames(final) <- c("treat", "rec_time", "food", "cw_ini", "tox_start")


## fancy plot
library('ggplot2')

dat_f <- as.data.frame(final)
dat_f$rec_time <- as.character(dat_f$rec_time)
dat_f$rec_time <- as.numeric(dat_f$rec_time)
dat_f$tox_start <- factor(dat_f$tox_start, labels =c("application in spring", "application in summer", "application in autumn"))

ggplot(dat_f, aes(food, cw_ini, fill = rec_time)) + 
  geom_raster(aes(fill = rec_time)) +
  facet_wrap(~tox_start) +
  labs(x = "available food (factor)", y = "concentration of cpf in water (µg/L)", fill = "recovery time (days)") +
  geom_text(aes(label = rec_time)) +
  scale_fill_gradientn(colours = c("gray95", "gray60")) +
  theme_minimal(base_size=15)

# uncomment to save  
ggsave("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Plots/recovery_times_plot_neu.png",device="png", height=5, width=10)


# colors in R: http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf




