# DEB IBM Model with hourly calculated feeding and iterations (final)
# v1.3 "dopar"
# parallelisation function implemented

# set working directory 
setwd("/Users/dino/Dropbox/Uni/Master/Masterarbeit/R_Masterarbeit")

library(foreach)
library(doParallel)

#num_cores <- detectCores()-2
num_cores <- 3
cl<-makeCluster(num_cores)
registerDoParallel(cl)

# Define number of Monte-Carlo simulations
mc.no <- 10
days <- 365 * 5

# matrix for results of each monte carlo simulation
mc.pop.size <- matrix(data = NA, nrow = days + 1, ncol = mc.no)


#### Parallel execution ####
result <- foreach(m = 1:mc.no, .combine = 'cbind') %dopar% {

# -------------------------------Options -------------------------------------------------
# days to be simulated
days      <- 365 * 5
# Size of environment (dm^3)
env.size  <- 300  / 10     # 1 L Beaker -> d = 10 cm


## Temperature scenario
# scenario (= T) or constant (= F)
temp.sc <- T
# load temperature scenario
temp.v <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/Temperatur/temp_mean_oct.csv", header = TRUE, sep = ";", dec = ".")$mean_temp
temp.v <- rep(temp.v, c(max(1, days/365)))
# increase daily temperature
#temp.v <- temp.v + 2
# set temperature (°C) for constant scenario
temp <- 20


# Reproduction on/off
repro <- T


## Monte Carlo method
# MC on / off
mc.on <- T

## Start Population
# size class 1
l.start_1 <- 0.4    # physical size in cm
no.start_1 <- 13    # quantity            # 127     
sd.start_1 <- 0.05   # standard deviation
# size class 2
l.start_2 <- 0.6     # physical size in cm 
no.start_2 <- 64     # quantity       # 648
sd.start_2 <- 0.05    # standard deviation
# size class 3
l.start_3 <-  0.8    # physical size in cm
no.start_3 <- 23    # quantity      # 225
sd.start_3 <- 0.05    # standard deviation


# set sexes of individuals of start population
#sex.v <- c('m','f','m', 'f', 'm', 'f', 'm', 'f', 'm', 'f', 'm','m','m','m','m', 'f','f','f','f','f') # laboratory test
#sex.v <- rep(c('m','f'),  c(ceiling((no.start_1 + no.start_2 + no.start_3) / 2))) # 50:50 frequency
#sex.v <- rep(c('m'), c(no.start_1 + no.start_2 + no.start_3)) # males or females only
#sex.v <- sample(c('m', 'f'), c(no.start_1 + no.start_2 + no.start_3), replace = T) # random distribution
# 1 m^2 field scenario
#sex.v <- c(rep('f', 79), rep('m', 48), rep('f', 320), rep('m', 328), rep('f', 30), rep('m', 195)) # field scenario
# 1/4 m^2 field scenario
#sex.v <- c(rep('f', 20), rep('m', 12), rep('f', 80), rep('m', 82), rep('f', 8), rep('m', 48))
# 1/10 m^2 field scenario
sex.v <- c(rep('f', 8), rep('m', 5), rep('f', 32), rep('m', 32), rep('f', 3), rep('m', 20))


## Feeding
# medium renewal on/off
feed.med.change <- F
# days after which medium and food is renewed (7 = weekly)
feed.med.days   <- 7
# feeeding scenario (amount of food in Joule)
#feed.sc <- rep(c(50,0,0,50,0,0,0), trunc(days/7) + 1) # scenario for population experiment - 1 leaf disc = 16 J (Graca et al., 1993)
#feed.sc <- rep(c(1000,1000,1000,1000,1000,1000,1000), trunc(days/7) + 1)
#feed.sc <- rep(30685, days + 1) # same amount each day
# read csv file with feeding data
# 1/4 m^2
#feed.sc <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Recovery_scenario/food_scen_seasons.csv", header = FALSE, sep = ";", dec = ",")$V1
# 1/10 m^2
feed.sc <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Recovery_scenario/food_scen_seasons_0.1qm.csv", header = FALSE, sep = ";", dec = ",")$V1
feed.sc <- rep(trunc(feed.sc * 0.63), (days / 365))
# adjust amount of food
#feed.sc <- feed.sc * 0.5


## Toxicity
# include toxic effects (on/off)
tox.on <- T
# Use scenario (= T) or enter start and end days  (= F)
tox.scn <- T
# Individual Tolerance (= 1) or Stochastic death (= 2)
tox.opt <- 2
# Exposure Start (day)
tox.t.start <- 1
# Exposure End (day)
tox.t.end <- 4
# concentration of chlorpyrifos in water
cw <- 8.25 # µg/L
# load scenario
tox.sc <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Recovery_scenario/recovery_tox_scenario_0.135_single.csv", header = FALSE, sep = ";", dec = ".")$V1

# ------------------------------Constant Parameters (for all individuals) ----------------
# Growth and Reproduction
EG      <- 4442.31     # volume-specific cost for structure (J / cm^3)
g       <- 1.581882    # energy investment ratio (-)
km      <- 0.047295    # somatic maintencance rate coefficient (d^-1)
v       <- 0.021591    # energy conductance (cm/d)
dm      <- 0.28176     # shape coefficient (-)
Lm_f    <- 1.0 * dm    # ultimate total structural length for female (cm)
Lm_m    <- 1.2 * dm    # ultimate total structural length for male (cm)
Li      <- 0.0000001   # Initial volumetric structural length (cm)
kappa   <- 0.82104     # allocation fraction to soma (-)
kap_x   <- 0.8         # digestion efficiency of food to reserve (-)
kj      <- 0.002       # maturity maintenance rate coefficient (d^-1)
kr      <- 0.95        # reproduction efficiency (-)
ubh     <- 0.003392066 # scaled maturity at birth (cm^2 * d)
uph     <- 0.027244857 # scaled maturity at puberty (cm^2 * d)
u0e     <- 0.029357688 # cost of an egg (cm^2 * d)

# Food dependent survival
ha      <- 0.0000005597 # weibull aging acceleration constant (d^-2)
sg      <- 0.0001       # gompertz aging stress coefficient (-)
e_kd    <- 0.02         # damage recovery rate constant (d^-1) 
e_alpha <- 0.2          # median of threshold distribution (-)
e_beta  <- 7            # slope of threshold distribution (d^-1) 

# Feeding and Assimilation
xk      <- 14.2016923  # half saturation constant (J/L)
pAm_f   <- 73.8488     # Surface-area-specific maximum assimilation rate for female (J / (cm^2 * d))
pAm_m   <- 75.4559     # Surface-area-specific maximum assimilation rate for male (J / (cm^2 * d))
px_max  <- kap_x       # maximum assimilation efficiency (-)

# GUTS
ke_sd   <- 0.0070211 * 24  # dominant rate constant for SD (h^-1)
kk      <- 0.0105648 * 24  # killig rate for SD (h^-1)
z       <- 0.1856509       # threshold for effect for SD (µg/L)
ke_it   <- 1.46e-07 * 24   # dominant rate constant for IT (h^-1) 
t_alpha <- 5.71e-05        # median of threshold distribution for IT (µg/L)
t_beta  <- 2.34836595      # shape parameter of threshold distribution for IT (h^-1)
L_par   <- 0.7             # physical body size of animals used in toxicity test for parameterisation (cm)

# Arrhenius temperature [K]
TA <- 8000


# ----------------------- Parameters, Matrices, Vectors and Monte Carlo -----------------------
# body size-dependent toxicity (according to Gergs et al. 2015)
# ke for maximum body size [d^-1]
ke_sd_x_f <- ke_sd * ((L_par * dm) / Lm_f)
ke_sd_x_m <- ke_sd * ((L_par * dm) / Lm_m)
ke_it_x_f <- ke_it * ((L_par * dm) / Lm_f)
ke_it_x_m <- ke_it * ((L_par * dm) / Lm_m)


# vector for sizes of start population
s1 <- rep(l.start_1, no.start_1)
s2 <- rep(l.start_2, no.start_2)
s3 <- rep(l.start_3, no.start_3)
l.start <- c(s1, s2, s3)

# vector for water concentration cw
if ((tox.on == T) & (tox.scn == F)){
  cw_1 <- rep(0, tox.t.start)
  cw_2 <- rep(cw, c(tox.t.end - tox.t.start + 1))
  cw_3 <- rep(0, days - tox.t.end)
  cw <- c(cw_1, cw_2, cw_3)
}
if ((tox.on == T) & (tox.scn == T)){
  cw <- tox.sc  
}

# no. of steps for calculation of changes in smaller steps
steps <- 100

# create temperature scenario with constant temperature
if (temp.sc == F){
  temp.v <- rep(temp, days + 1)
}
# save initial values (needed for temperature adjustment below)
km0 <- km
v0 <- v
kj0 <- kj
u0e0 <- u0e
ha0 <- ha
e_kd0 <- e_kd
e_beta0 <- e_beta
pAm_f0 <- pAm_f
pAm_m0 <- pAm_m


## Monte Carlo
# matrix for each monte carlo step
#mc.pop.size <- matrix(data = NA, nrow = days + 1, ncol = mc.no)

# set number of mc simulations to 1 if Monte Carlo is switched off
if (mc.on == F) {mc.no <- 1}


  # Randomly chooses start sizes from a normal distribution inside one standard deviation
  l.rnorm <- c()
  # lengths for size class 1
  if (no.start_1 > 0){
    for (x in 1:length(s1)) {
      l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_1)
      repeat {if ((l.rnorm[x] < (l.start[x] + 1 * sd.start_1)) | (l.rnorm[x] > (l.start[x] - 1 * sd.start_1)) | (sd.start_1 == 0)) break else {
        l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_1)
      }}
    }
  }
  # lengths for size class 2
  if (no.start_2 > 0){
    for (x in x:c(x + length(s2))) {
      l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_2)
      repeat {if ((l.rnorm[x] < (l.start[x] + 1 * sd.start_2)) | (l.rnorm[x] > (l.start[x] - 1 * sd.start_2)) | (sd.start_2 == 0)) break else {
        l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_2)
      }}
    }
  }
  
  # lengths for size class 3
  if (no.start_3 > 0){
    for (x in x:c(x + length(s3))) {
      l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_3)
      repeat {if ((l.rnorm[x] < (l.start[x] + 1 * sd.start_3)) | (l.rnorm[x] > (l.start[x] - 1 * sd.start_3)) | (sd.start_3 == 0)) break else {
        l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_3)
      }}
    }
  }
  
  
  # matrix for population size
  pop.size <- matrix(data = NA, nrow = days + 1, ncol = 2)
  colnames(pop.size) <- c("t", "pop.size")
  
  # vectors - also clears vectors after each monte carlo step
  Px <- c()
  pxm <- c()
  pa <- c()
  f <- c()
  sl <- c()
  Sc <- c()
  dL <- c()
  L <- c()
  UE <- c()
  e <- c()
  UH <- c()
  UR <- c()
  R <- c()
  q <- c()
  h <- c()
  D_e <- c()
  Fe <- c()
  s <- c()
  rs <- c()
  sex <- c()
  alive <- c()
  breeding <- c()
  b.L <- c()
  b.e <- c()
  b.UH <- c()
  b.Sc <- c()
  pop.feed <- c()
  env.foodav <- c()
  L.s <- c()
  ci <- c()
  Ft <- c()
  ht <- c()
  sd_F <- c()
  sd_UE <- c()
  foodav <- c()
  age <- c()
  ke_it_s <- c()
  ke_sd_s <- c()
  
  
  # ------------------------------BEGIN Code for Simulation ----------------------------------------
  #### Simulating Start Population until desired start size of individuals
  
  ### start values for start population (day 0 for start individuals)
  for (i in 1:length(l.start)) {
    # desired structural length at test start for each individual in start population
    L.s[i] <- l.rnorm[i] * dm
    
    # age
    age[i] <- 0
    
    # initial structural length
    L[i]  <- Li
    
    ## Feeding
    Px[i] <- 0
    pxm[i] <- 0
    pa[i] <- 0
    f[i] <- 1 #0.7
    
    # reserve dynamics
    e[i]  <- 1 #0.0446106 # 0.7 # 0.4
    sl[i] <- 0 
    Sc[i] <- 0
    dL[i]  <- 0
    UE[i] <- (e[i] * L[i] ^ 2) / v
    
    # Maturation and Reproduction
    UH[i] <- ubh
    UR[i] <- 0
    R[i]  <- 0
    breeding[i] <- F
    b.L[i] <- 0
    b.e[i] <- 0
    b.UH[i] <- 0
    b.Sc[i] <- 0
    sex[i] <- sex.v[i]
    
    # survival
    q[i]   <- 0   # aging acceleration q at birth
    h[i]   <- 0   # hazard rate h at birth
    D_e[i] <- 0   # maximum scaled damage e_D at birth
    Fe[i]  <- 0
    s[i]   <- 1  
    rs[i] <- sample(0:1000000, 1) / 1000000   # individual survival probability
    alive[i] <- T
    
    # toxicity
    ke_it_s[i] <- ke_it  # scaled elimination rate for IT
    ke_sd_s[i] <- ke_sd  # scaled elimination rate for SD
    ci[i]  <- 0  # scaled internal concentration
    ht[i]  <- 0  # hazard rate for effects
    Ft[i]  <- 0
    
  } # loop for each start individual
  
  ### further life cycle of start individuals until desired length
  for (i in 1:length(l.start)) {
     repeat {if (L[i] >= L.s[i] | L[i] > 0.28838) break else {
      ##  Growth, maturation, survival and reproduction for each individual
      # age (for debugging purposes only)
      age[i] <- age[i] + 1 
       
      ## growth
      dL[i] <- 0
      ## Reserve dynamics
      for(y in seq(steps)){
        # scaled body length (-)
        if (e[i] > 0) {
          sl[i]  <- (L[i] * g * km) / (v * e[i])
        } else {
          sl[i] <- 0
        }
        
        # for growth conditions
        if (e[i] >= sl[i]) {
          # mobilisation flux (mm^2)
          Sc[i] <- L[i] ^ 2 * ((g * e[i]) / (g + e[i])) * (1 + (km * L[i] / v))
          # increase in length (mm)
          dL[i] <- dL[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g) * Sc[i]) - km * L[i] ^ 3)) / steps )
          # new length
          L[i]  <- L[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g) * Sc[i]) - km * L[i] ^ 3)) / steps )
        }
        else {
          # for non-growth conditions
          Sc[i] <- (kappa * km * g * L[i] ^ 3) / v
          dL[i] <- 0
        }
        
        # Scaled amount of reserve (d * mm^2)
        UE[i] <- UE[i] + (((f[i] * L[i] ^ 2) - Sc[i]) / steps)
        if (UE[i] < 0) {UE[i] <- 0}
        # scaled reserve density (-)
        e[i] <- v * (UE[i] / L[i] ^ 3)
        # keep e between 0 and 1
        if (e[i] < 0) {e[i] <- 0}
        if (e[i] > 1) {e[i] <- 1}
      } # for steps
      
      
      # energy allocation to maturation and reproduction
      #if(is.nan(UR[i]) == T){UR[i] <- 0}
      if (e[i] > (f[i] * (L[i] / (v / (km * g))))){
        # if individual has not yet reaced puberty, then energy flux to maturation
        if (UH[i] < uph) {UH[i] <- UH[i] + (((1 - kappa) * Sc[i]) - (kj * UH[i]))}   
        # if individual has reached puberty then energy flux into reproductive buffer   
        if (UH[i] >= uph)  {UR[i] <- UR[i] + (((1 - kappa) * Sc[i]) - (kj * uph))}
        if (UR[i] < 0) {UR[i] <- 0}
        
      }
      
      ## Reproduction
      # breeding if puberty is reached
      if (UH[i] >= uph) { 
        if (breeding[i] == T) {
          b.Sc[i] <- b.L[i] ^ 2 * ((g * b.e[i]) / (g + b.e[i])) * (1 + (km * b.L[i] / v))
          b.L[i] <- b.L[i] + ((1 / (3 * b.L[i] ^ 2)) * (((v / g) * b.Sc[i]) - km * b.L[i] ^ 3))
          b.UH[i] <- b.UH[i] + (((1 - kappa) * b.Sc[i]) - (kj * b.UH[i]))
        }
        
        if (breeding[i] == F) {
          b.L[i] <- Li # structural length at egg formation
          b.e[i] <- e[i]  # scaled reserve density of brood equals those of the mother at egg formation (different to DEB Theory)
          b.UH[i] <- 0  # scaled maturity at egg formation
          R[i]  <- trunc(kr * UR[i] / u0e)  # calculation of brood size
          #if(R[i] < 0){R[i] <- 0}
          UR[i] <- UR[i] - ((R[i] - u0e) / kr) # resets reproduction buffer but leaves rest of the buffer that could not be used to produce full egg
          breeding[i] <- T
        }
        
        # set breeding back to false without releasing a brood
        if ((b.UH[i] >= ubh)) {
          breeding[i] <- F
          }
      } # Reproduction if ((UH[i] >= uph) & (UR[i] > 0))
      
      
      ## Survival
      # aging acceleration
      q[i] <- q[i] + (((q[i] * (L[i] ^ 3 / (v / (g * km)) ^ 3) * sg ) + ha ) * e[i] * ((v / L[i]) - ((3 * dL[i]) / L[i])) - ((3 * dL[i]) / L[i]) * q[i]) 
      # hazard rate
      h[i] <- h[i] + (q[i] - (3 * dL[i] / L[i]) * h[i])
      if(h[i] < 0){h[i] <- 0}
      # maximum scaled damage for starvation
      D_e[i] <- D_e[i] + (e_kd * ((1 - e[i]) - D_e[i]))
      
      # individual tolerance distribution for starvation
      if (D_e[i] > 0){
        Fe[i]  <- 1 / (1 + ((D_e[i] / e_alpha) ^ -e_beta))} 
      else {Fe[i] <- 0}
      
      # survival probability
      s[i] <- (1 - Fe[i]) * exp(-h[i])
      # set alive to false if s < rs
      if (s[i] <= rs[i]) {alive[i] <- F}
      
      # if start individual would die assign new survival probability rs
      while (alive[i] == F) {
        rs[i] <- sample(0:1000000, 1) / 1000000
        ifelse(s[i] <= rs[i], alive[i] <- F, alive[i] <- T)
      }
    }} # while L[i] < L.s[i]
  } # for i
  
  
  ## adult females carrying new borns at test start (adjustment for laboratory test)
 # for (i in 16:20) {
  #  R[i] <- 15
 # }

  
  #### Simulating each individual on every day t -----------------------------------------------------
  pop.count <- length(l.start) # population size (including dead ones) at first day - needed for loop i
  for (t in seq(days + 1)) {
    
    ## adaptation of parameters based on temperature
    # Arrhenius function
    f_T <- exp((TA/(20 + 273.15)) - (TA/(temp.v[t] + 273.15)))
    
    # adjust parameters
    km <- km0 * f_T
    v <- v0 * f_T
    kj <- kj0 * f_T
    u0e <- u0e0 / f_T
    ha <- (sqrt(ha0) * f_T) ^ 2
    e_kd <- e_kd0 * f_T
    e_beta <- e_beta0 * f_T
    pAm_f <- pAm_f0 * f_T
    pAm_m <- pAm_m0 * f_T
    
    
    ## Feeding 
    env.foodav[1] <- feed.sc[1]
    
    for (i in 1:pop.count) {
      if (alive[i] == T){
        
        # maximum surface-area-specific ingestion rate (J/(d * cm^2))
        if (sex[i] == 'f'){
          pxm[i] <- pAm_f / kap_x
        } 
        if (sex[i] == 'm'){
          pxm[i] <- pAm_m / kap_x
        }
        
        # food ingestion rate (J/d)
        Px[i] <- pxm[i] * L[i] ^ 2  * ((env.foodav[t] / env.size) / (xk + (env.foodav[t] / env.size)))
      } # if alive 
      
      if (alive[i] == F){
        pxm[i] <- -1
        Px[i] <- -1
      }
    } # for i
    
    # sum of feeding rates of all individuals (potential)
    pop.feed[t] <- sum(Px[which(alive == T)], na.rm = TRUE)
    
    # updates food ingestion rate if population would eat more than available
    if (pop.feed[t] > env.foodav[t]) {
      for (i in 1:pop.count) {
        if(alive[i] == T){
          Px[i] <- (env.foodav[t] / pop.feed[t]) * Px[i]
        }
        if(alive[i] == F){
          Px[i] <- -1}
      }
      # new population feeding rate (real)
      pop.feed[t] <- sum(Px[which(alive == T)], na.rm = TRUE)
    } # if pop.feed > env.foodav
    
    # available food on next day
    if (((t + 1) / feed.med.days) == trunc((t + 1) / feed.med.days) & (feed.med.change == TRUE)){
      env.foodav[t + 1] <- feed.sc[t + 1]
    } else {
      env.foodav[t + 1] <- env.foodav[t] - sum(Px[which(alive == T)]) + feed.sc[t + 1]
    }
    if (env.foodav[t] < 0){env.foodav[t] <- 0}
    
    # final assimilation rate and scaled functional response
    for (i in 1:pop.count) {
      if(alive[i] == T){
        # (final) assimilation rate (J/d)
        pa[i] <- Px[i] * kap_x
        
        # scaled functional response (-)
        if (sex[i] == 'f') {f[i] <- pa[i] / (pAm_f * L[i] ^ 2)}
        if (sex[i] == 'm') {f[i] <- pa[i] / (pAm_m * L[i] ^ 2)}
        if(f[i] > 1){f[i] <- 1}
      } # if alive
      
      if(alive[i] == F){
        f[i] <- -1
        pa[i] <- -1
      }
    } # for i
    
  
    ##  Growth, maturation, survival and reproduction for each individual
    for (i in 1:pop.count) {
      if (alive[i] == T) {
        #age
        age[i] <- age[i] + 1
        ## growth
        dL[i] <- 0
        
        ## Reserve dynamics
        for(y in seq(steps)){
          # scaled body length (-)
          if (e[i] > 0) {
            sl[i]  <- (L[i] * g * km) / (v * e[i])
          } else {
            sl[i] <- 0
          }
          
          # for growth conditions
          if (e[i] >= sl[i]) {
            # mobilisation flux (mm^2)
            Sc[i] <- L[i] ^ 2 * ((g * e[i]) / (g + e[i])) * (1 + (km * L[i] / v))
            # increase in length (mm)
            dL[i] <- dL[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g) * Sc[i]) - km * L[i] ^ 3)) / steps)
            # new length
            L[i]  <- L[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g) * Sc[i]) - km * L[i] ^ 3)) / steps)
          }
          else {
            # for non-growth conditions
            Sc[i] <- (kappa * km * g * L[i] ^ 3) / v
            dL[i] <- 0
          }
          
          # Scaled amount of reserve (d * mm^2)
          UE[i] <- UE[i] + ((f[i] * L[i] ^ 2 - Sc[i]) / steps)
          if (UE[i] < 0){UE[i] <- 0}
          # scaled reserve density (-)
          e[i]  <- v * (UE[i] / L[i] ^ 3)
          # keep e between 0 and 1
          if (e[i] < 0) {e[i] <- 0}
          if (e[i] > 1) {e[i] <- 1}
        } # for steps
        
        # energy allocation to maturation and reproduction
        if (e[i] > (f[i] * (L[i] / (v / (km * g))))){
          # if individual has not yet reaced puberty, then energy flux to maturation
          if (UH[i] < uph) {UH[i] <- UH[i] + (((1 - kappa) * Sc[i]) - (kj * UH[i])) }   
          # if individual has reached puberty then energy flux into reproductive buffer
          if (UH[i] >= uph)  {UR[i] <- UR[i] + (((1 - kappa) * Sc[i]) - (kj * uph)) }
          if (UR[i] < 0) {UR[i] <- 0}
        }
        
        ## Reproduction
        # assumption: males need same energy to produce sperm as females need to produce eggs
        # start reproduction when puberty is reached 
        if (UH[i] >= uph) {
          if (breeding[i] == T) {
            b.Sc[i] <- b.L[i] ^ 2 * ((g * b.e[i]) / (g + b.e[i])) * (1 + (km * b.L[i] / v))
            b.L[i] <- b.L[i] + ((1 / (3 * b.L[i] ^ 2)) * (((v / g) * b.Sc[i]) - km * b.L[i] ^ 3)) 
            b.UH[i] <- b.UH[i] +  (((1 - kappa) * b.Sc[i]) - (kj * b.UH[i]))
          }
          
          if (breeding[i] == F) {
            b.L[i] <- Li # structural length at egg formation
            b.e[i] <- e[i]  # scaled reserve density of brood equals those of the mother at egg formation (different to DEB Theory)
            b.UH[i] <- 0  # scaled maturity at egg formation
            R[i]  <- trunc(kr * UR[i] / u0e)  # calculation of brood size
            UR[i] <- UR[i] - ((R[i] - u0e) / kr) # resets reproduction buffer but leaves rest of the buffer that could not be used to produce full egg
            breeding[i] <- T
          }
        } # Reproduction if UH[i] >= uph
        
        
        ## Survival
        # aging acceleration
        q[i] <- q[i] + (((q[i] * (L[i] ^ 3 / (v / (g * km)) ^ 3) * sg ) + ha ) * e[i] * ((v / L[i]) - ((3 * dL[i]) / L[i])) - ((3 * dL[i]) / L[i]) * q[i])
        # maximum scaled damage for starvation
        D_e[i] <- D_e[i] + (e_kd * ((1 - e[i]) - D_e[i]))
        # hazard rate
        h[i] <- h[i] + (q[i] - (3 * dL[i] / L[i]) * h[i])
        if(h[i] < 0){h[i] <- 0}
        
        # individual tolerance distribution for starvation
        if (D_e[i] > 0){
          Fe[i]  <- 1 / (1 + ((D_e[i] / e_alpha) ^ -e_beta))} 
        else {Fe[i] <- 0}
        
        # without toxic effects
        if(tox.on == FALSE){
          # survival probability
          s[i] <- (1 - Fe[i]) * exp(-h[i])
        }
        
        # Toxicity
        if(tox.on == TRUE){
          for(y in seq(steps)){
            # Individual Tolerance
            if(tox.opt == 1){
              # scaling the elimination rate ke depending on the surface to volume ratio
              if(sex[i] == 'f'){ke_it_s[i] <- ke_it_x_f * (Lm_f / L[i])}
              if(sex[i] == 'm'){ke_it_s[i] <- ke_it_x_m * (Lm_m / L[i])}
              # maximum scaled internal concentration
              ci[i] <- max(ci[i], ci[i] + ((ke_it_s[i] / steps) * (cw[t] - ci[i])))
              # threshold for effect
              Ft[i] <- 1 / (1 + ((ci[i] / t_alpha) ^ -t_beta))
              # survival probability
              s[i] <- (1 - Ft[i]) * (1 - Fe[i]) * exp(-h[i])
            }
            
            # Stochastic death  
            if(tox.opt == 2){
              # scaling the elimination rate ke depending on the surface to volume ratio
              if(sex[i] == 'f'){ke_sd_s[i] <- ke_sd_x_f * (Lm_f / L[i])}
              if(sex[i] == 'm'){ke_sd_s[i] <- ke_sd_x_m * (Lm_m / L[i])}
              # scaled internal concentration at time step t
              ci[i] <- ci[i] + ((ke_sd_s[i] * (cw[t] - ci[i])) / steps) 
              # hazard rate for effects
              ht[i] <- ht[i] + (((kk * max(ci[i] - z, 0)) + h[i]) / steps)
              if(ht[i] < 0){ht[i] <- 0}
              # survival probability
              s[i] <- (1 - Fe[i]) * exp(-ht[i])
            }
          } # for steps
        } # if tox.on
        
        # set alive to false if s <= rs
        if(s[i] <= rs[i]) {alive[i] <- F}
      } # if alive == T
        
      
      # keep all vectors at same length (poses problem if last entry of alive vector is false)
      if (alive[i] == F){
        Px[i] <- -1
        pxm[i] <- -1
        pa[i] <- -1
        f[i] <- -1
        sl[i] <- -1
        Sc[i] <- -1
        dL[i] <- -1
        L[i] <- -1
        UE[i] <- -1
        e[i] <- -1
        UH[i] <- -1
        UR[i] <- -1
        R[i] <- -1
        q[i] <- -1
        h[i] <- -1
        D_e[i] <- -1
        Fe[i] <- -1
        s[i] <- -1
        rs[i] <- -1
        breeding[i] <- F
        sex[i] <- NA
        b.L[i] <- -1
        b.e[i] <- -1
        b.UH[i] <- -1
        b.Sc[i] <- -1
        ci[i] <- -1
        Ft[i] <- -1
        ht[i] <- -1
        age[i] <- age[i]
        ke_it_s[i] <- -1
        ke_sd_s[i] <- -1
      }
      
      ## Adding new born individuals
      # add new values of brood to individual vectors when birth maturity of brood is reached -> after calculation of survival -> new borns of dead individuals will not be added
      if ((alive[i] == T) & (b.UH[i] >= ubh)){       
        if ((R[i] >= 1) & (sex[i] == 'f') & (repro == T)) { 
          for (b in 1:(R[i])) {
            # parameters from mother
            L[length(L) + 1] <- b.L[i]
            e[length(e) + 1] <- b.e[i]
            UH[length(UH) + 1] <- b.UH[i]
            Sc[length(Sc) + 1] <- 0
            # remaining parameters
            Px[length(Px) + 1] <- 0
            pxm[length(pxm) + 1] <- 0
            pa[length(pa) + 1] <- 0
            f[length(f) + 1] <-  b.e[i]
            UE[length(UE) + 1] <- (e[length(e)] * L[length(L)] ^ 3) / v
            sl[length(sl) + 1] <- (L[length(L)] * g * km) / (v * e[length(e)])
            dL[length(dL) + 1] <- 0
            UR[length(UR) + 1] <- 0
            R[length(R) + 1] <- 0
            breeding[length(breeding) + 1] <- F
            sex[length(sex) + 1] <- ifelse(sample(1:100, 1) < 26, sex[i] <- 'f', sex[i] <- 'm') # birth frequency after Bloor 2010
            b.L[length(b.L) + 1] <- 0
            b.e[length(b.e) + 1] <- 0
            b.UH[length(b.UH) + 1] <- 0
            b.Sc[length(b.Sc) + 1] <- 0
            q[length(q) + 1] <- 0
            h[length(h) + 1] <- 0
            D_e[length(D_e) + 1] <- 0
            Fe[length(Fe) + 1] <- 0
            rs[length(rs) + 1] <- sample(0:1000000, 1) / 1000000
            alive[length(alive) + 1] <- T
            age[length(age) + 1] <- 0
            
            # Toxicity Parameters
            if(tox.on == FALSE){
              s[length(s) + 1] <- (1 - Fe[length(Fe)]) * exp(-h[length(h)])
              ci[length(ci) + 1] <- -1
              Ft[length(Ft) + 1] <- -1
              ht[length(ht) + 1] <- -1
            }
            
            if(tox.on == TRUE){
              # Individual Tolerance
              if(tox.opt == 1){
                ke_it_s[length(ke_it_s) + 1 ] <- ke_it
                ci[length(ci) + 1] <- 0 
                Ft[length(Ft) + 1] <- 0
                s[length(s) + 1] <- 1
                ht[length(ht) + 1] <- -1
              }
              # Stochastic death  
              if(tox.opt == 2){
                ke_sd_s[length(ke_sd_s) + 1 ] <- ke_sd
                ci[length(ci) + 1] <- 0 
                ht[length(ht) + 1] <- 0
                s[length(s) + 1] <- 1
                Ft[length(Ft) + 1] <- -1
              }
            } # if tox.on is true
          } # for loop brood
        } # if R >= 1
        # set breeding back to false after brood is released
        breeding[i] <- F
      } # adding brood if alive and b.UH[i] >= ubh
    } # for loop - individual life cycles
    
    # count population size in each time step
    pop.size[t, 1] <- t
    pop.size[t, 2] <- sum(alive == T)
    # length of population vectors (= number of all individuals, including dead ones)
    pop.count <- length(s)
    
    
  } # for t loop

  pop.size[,2]
  
} # monte carlo foreach

return(result)

# Shutdown cluster
if(!is.null(cl)) {
  stopCluster(cl)
  stopImplicitCluster()
  cl <- c()
}

mc.pop.size <- result
days <- nrow(result) - 1
# ------------------------------- END of Simulation ---------------------------------------

# ------------------------------ Results --------------------------------------------------
## Matrices and Plots with Monte Carlo

  mc.pop.mean <- rowMeans(mc.pop.size, na.rm = TRUE)
  mc.pop.min <- apply(mc.pop.size, 1, min, na.rm = TRUE)
  mc.pop.max <- apply(mc.pop.size, 1, max, na.rm = TRUE)
  mc.pop.quant <- apply(mc.pop.size, 1, quantile, na.rm = TRUE)
  
  plot(c(0:days), mc.pop.quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", ylim = c(0,max(mc.pop.quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
  lines(c(0:days), mc.pop.quant[1, ], type = "l", lty = 2) # min 0% quantil
  lines(c(0:days), mc.pop.quant[3, ], type = "l", lty = 1) # median
  #lines(c(0:days), mc.pop.quant[2, ], type = "l", lty = 3) # 25% quantil
  #lines(c(0:days), mc.pop.quant[4, ], type = "l", lty = 3) # 75% quantil
  
# -------------------------------- Profiling and Data Export ----------------------------------

 mc.median <- cbind(c(0:days), mc.pop.quant[3, ])
# write.csv(mc.median[, c(1:2)], file = "median_temp.csv")
# write.csv(mc.pop.size, file = "2018_01_29_tox_simresults_it_T5.csv")

# auto file name (example: 2018-01-29_15.51_simresults_13.48.csv)
write.csv(mc.pop.size, file = paste0(format(Sys.time(), "%Y-%m-%d_%H.%M"),"_simresults_","MC=",mc.no,"_tox=0.135single_food=std_temp=std", ".csv", sep = ""))


# -------------------------------------- Recovery ------------------------------------------
# load data from baseline scenario
base_sc <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/shared/2018-06-15_13.25_simresults_0.1qm_50mc_10years.csv", header = TRUE, sep = ",", dec = ".")






