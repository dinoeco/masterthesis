# Calculation of starvation parameters
# test every combination of parameters by performing a simulation with each combination and calculating nse value

# set working directory 
setwd("/Users/dino/Dropbox/Uni/Master/Masterarbeit/R_Masterarbeit")

# for calculation of nse
library("hydroGOF")
obs_data <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/Daten/starvation.csv", header = TRUE, sep = ";", dec = ".")
ts <- obs_data[,1]
obs_data_mean <- (obs_data[, -1])  


# sequences
#e_kd_v <- seq(0.01,0.99,0.01)
#e_alpha_v <- seq(0.1,0.4,0.01)
#e_beta_v <- seq(0.1,10,0.1)

 e_kd_v <- seq(0.01,0.4,0.02)
 e_alpha_v <- seq(0.2,0.4,0.02)
 e_beta_v <- seq(1,12,0.5)

#e_kd_v <- seq(0.01,0.04,0.01)
#e_alpha_v <- seq(0.2,0.3,0.01)
#e_beta_v <- seq(5,8,0.2)

# e_kd_v <- seq(0.1,0.2,0.1)
# e_alpha_v <- seq(0.1,0.2,0.1)
# e_beta_v <- seq(0.1,0.2,0.1)

combinations <- length(e_kd_v) * length(e_alpha_v) * length(e_beta_v)
combinations

# matrix with results of all combinations
nse_m <- matrix(data = NA, ncol = 4)
colnames(nse_m) <- c("e_kd", "e_alpha", "e_beta", "nse")


# -------------------------------Options -------------------------------------------------
# days to be simlated
days      <- 80
# Size of environment
env.size  <- 0.250
# Temperature [°C]
temp      <- 11
# no. of steps for calculation of changes in smaller steps
steps     <- 100

# Reproduction on/off
repro <- F

## Monte Carlo
# MC on / off (1/0 or TRUE/FALSE)
mc.on <- F
# Number of Monte Carlo Simulations
mc.no <- 5

## Start Population
# size class 1
l.start_1 <- 0.787    # physical size in cm 
no.start_1 <- 100     # quantity          
sd.start_1 <- 0.02   # standard deviation
# size class 2
l.start_2 <- 0.6    # physical size in cm 
no.start_2 <- 0     # quantity       
sd.start_2 <- 0.02   # standard deviation
# size class 3
l.start_3 <-  0.4   # physical size in cm
no.start_3 <- 0     # quantity
sd.start_3 <- 0.02   # standard deviation

## Feeding
# medium renewal on/off
feed.med.change <- F
# days after which medium and food is renewed (7 = weekly)
feed.med.days   <- 7
# feeeding scenario
#feed.sc <- rep(c(1000,0,0,1000,0,0,0), trunc(days/7) + 1)
#feed.sc <- rep(c(1000,1000,1000,1000,1000,1000,1000), trunc(days/7) + 1)
feed.sc <- rep(0, days + 1)
# read csv file with feeding data
#feed.sc <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/food_scenario_0.5.csv", header = FALSE, sep = "", dec = ",")$V1

## Toxicity
# include toxic effects (on/off)
tox.on <- F
# Exposure Start (day)
tox.t.start <- 20
# Exposure End (day)
tox.t.end <- 24
# Individual Tolerance (= 1) or Stochastic death (= 2)
tox.opt <- 1
# concentration of substance in water
cw <- 10 # µg/L


#---------------------------  Parameterisation -----------------------------

a <- 1
c <- 1
d <- 1



#### ------------ Continue ---- ####



for (a in a:length(e_kd_v)){
  d <- 1 
  c <- 1
  for(c in c:length(e_alpha_v)){
    d <- 1
    for (d in d:length(e_beta_v)) {
      e_beta <- e_beta_v[d]
      e_alpha <- e_alpha_v[c]
      e_kd    <- e_kd_v[a]
      
      




# ------------------------------Constant Parameters (for all individuals) ----------------
# Growth and Reproduction
EG      <- 4442.31     # Volume-specific cost for structure (J / cm^3)
g       <- 1.581882    # energy investment ratio (-)
km      <- 0.047295    # somatic maintencance rate coefficient (d^-1)
v       <- 0.021591    # energy conductance (cm/d)
dm      <- 0.28176     # shape coefficient (-)
Li      <- 0.0000001   # Initial volumetric structural length (cm)
kappa   <- 0.82104     # allocation fraction to soma (-)
kap_x   <- 0.8         # digestion efficiency of food to reserve (-)
kj      <- 0.002       # maturity maintenance rate coefficient (d^-1)
kr      <- 0.95        # reproduction efficiency (-)
ubh     <- 0.003392066 # scaled maturity at birth (cm^2 * d)
uph     <- 0.027244857 # scaled maturity at puberty (cm^2 * d)
u0e    <- 0.029357688 # cost of an egg (cm^2 * d)

# Food dependent survival
ha      <- 0.0000005597# weibull aging acceleration constant (d^-2)
sg      <- 0.0001      # gompertz aging stress coefficient (-)
#e_kd    <- 0.01       # damage recovery rate constant (d^-1)
#e_alpha <- 0.1       # median of threshold distribution (-)
#e_beta  <- 5          # slope of threshold distribution (d^-1)

# Feeding and Assimilation
xk      <- 14.2016923  # half saturation constant (J/L)
pAm_f   <- 73.8488     # Surface-area-specific maximum assimilation rate for female (J / (cm^2 * d))
pAm_m   <- 75.4559     # Surface-area-specific maximum assimilation rate for male (J / (cm^2 * d))
px_max  <- kap_x       # maximum assimilation efficiency (-)

# GUTS
ke_it <- 0.0016 * 24  # dominant rate constant for IT [h^-1] (Gergs et al. 2016)
ke_sd <- 0.0029 * 24  # dominant rate constant for SD [h^-1] (Gergs et al. 2016)
kk <- 0.0133 * 24     # h^-1 killig rate (SD) (from Gergs et al. 2016)
z <- 0.2102           # µg/L  threshold for effect (SD) (from Gergs et al. 2016)
t_alpha <- 1.5614     # µg/L median of threshold distribution (IT) (Gergs et al. 2016)
t_beta <- 2.1420 * 24 # h^-1 shape parameter of threshold distribution (IT) (Gergs et al. 2016)

# Arrhenius temperature [K]
TA <- 8000


# -----------------------Parameters, Matrices, Vectors and Monte Carlo Start -------------------

# adaptation of parameters based on temperature
# Arrhenius function
f_T <- exp((TA/(20 + 273.15)) - (TA/(temp + 273.15)))

# adjust parameters
km <- km * f_T
v <- v * f_T
kj <- kj * f_T
u0e <- u0e / f_T
ha <- (sqrt(ha) * f_T) ^ 2
e_kd <- e_kd * f_T
e_beta <- e_beta * f_T
pAm_f <- pAm_f * f_T
pAm_m <- pAm_m * f_T


# vector for sizes of start population
s1 <- rep(l.start_1, no.start_1)
s2 <- rep(l.start_2, no.start_2)
s3 <- rep(l.start_3, no.start_3)
l.start <- c(s1, s2, s3)

# vector for water concentration cw
if (tox.on == T){
  cw_1 <- rep(0, tox.t.start)
  cw_2 <- rep(cw, c(tox.t.end - tox.t.start + 1))
  cw_3 <- rep(0, days - tox.t.end)
  cw <- c(cw_1, cw_2, cw_3)
}

## Monte Carlo
# matrix for each monte carlo step
mc.pop.size <- matrix(data = NA, nrow = days + 1, ncol = mc.no)

# set number of mc simulations to 1 if Monte Carlo is switched off
if (mc.on == F) {mc.no <- 1}

# loop for MC
for (m in 1:mc.no) {
  # Start Sizes with normal distribution and defined standard deviation
  l.rnorm <- c()
  # lengths for size class 1
  for (x in 1:length(s1)) {
    l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_1)
    repeat {if ((l.rnorm[x] > (l.start[x] + 1 * sd.start_1)) | (l.rnorm[x] < (l.start[x] - 1 * sd.start_1))) break else {
      l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_1)
    }}
  }
  
  # lengths for size class 2
  for (x in x:c(x + length(s2))) {
    l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_2)
    repeat {if ((l.rnorm[x] > (l.start[x] + 1 * sd.start_2)) | (l.rnorm[x] < (l.start[x] - 1 * sd.start_2))) break else {
      l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_2)
    }}
  }
  
  # lengths for size class 3
  for (x in x: c(x + length(s3))) {
    l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_3)
    repeat {if ((l.rnorm[x] > (l.start[x] + 1 * sd.start_3)) | (l.rnorm[x] < (l.start[x] - 1 * sd.start_3))) break else {
      l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = sd.start_3)
    }}
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
  
  
  # ------------------------------BEGIN Code for Simulation ----------------------------------------
  #### Simulating Start Population until desired start size of individuals
  
  ### start values for start population (day 0 for start individuals)
  for (i in 1:length(l.start)) {
    # desired structural length at test start for each individual in start population
    L.s[i] <- l.rnorm[i] * dm
    
    # initial structural length
    L[i]  <- Li
    
    ## Feeding
    Px[i] <- 0
    pxm[i] <- 0
    pa[i] <- 0
    f[i] <- 1 #0.7
    
    # reserve dynamics
    e[i]  <- 0.0446106 # 0.7  #
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
    # assign sex (50:50 ratio in start population)
    # ifelse(sample(1:100, 1) < 50, sex[i] <- 'f', sex[i] <- 'm')
    
    #sex[i] <- sex.v[i]
    sex[i] <- 'm'
    
    
    # survival
    q[i]   <- 0   # aging acceleration q at birth
    h[i]   <- 0   # hazard rate h at birth
    D_e[i] <- 0   # maximum scaled damage e_D at birth
    Fe[i]  <- 0
    s[i]   <- 1  
    rs[i] <- sample(0:1000000, 1) / 1000000   # individual survival probability
    alive[i] <- T
    
    # toxicity
    ci[i]  <- 0  # scaled internal concentration
    ht[i]  <- 0  # hazard rate for effects
    Ft[i]  <- 0
    
  } # loop for each start individual
  
  ### further life cycle of start individuals until desired length
  for (i in 1:length(l.start)) {
    #while (L[i] <= L.s[i]) {
    repeat {if (L[i] >= L.s[i]) break else {
      ##  Growth, maturation, survival and reproduction for each individual
      ## growth
      dL[i] <- 0
      f[i] <- 1 #0.7
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
        UE[i] <- UE[i] + (((f[i] * L[i] ^ 2) - Sc[i]) / steps )
        if (UE[i] < 0) {UE[i] <- 0}
        # scaled reserve density (-)
        e[i] <- v * (UE[i] / L[i] ^ 3)
        # keep e between 0 and 1
        if (e[i] < 0) {e[i] <- 0}
        if (e[i] > 1) {e[i] <- 1}
      } # for steps
      
      
      # energy allocation to maturation and reproduction
      if(is.nan(UR[i]) == T){UR[i] <- 0}
      if (e[i] > (f[i] * (L[i] / (v / (km * g))))){
        # if individual has not yet reaced puberty, then energy flux to maturation
        if (UH[i] < uph) {UH[i] <- UH[i] + (((1 - kappa) * Sc[i]) - (kj * UH[i])) }   
        # if individual has reached puberty then energy flux into reproductive buffer   
        if (UH[i] >= uph)  {UR[i] <- UR[i] + (((1 - kappa) * Sc[i]) - (kj * uph)) }
        if (UR[i] < 0) {UR[i] <- 0}
        
      }
      
      
      ## Reproduction
      # breeding if puberty is reached
      if ((UH[i] >= uph)) {         
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
          if(R[i] < 0){R[i] <- 0}
          UR[i] <- UR[i] - ((R[i] - u0e) / kr) # resets reproduction buffer but leaves rest of the buffer that could not be used to produce full egg
          breeding[i] <- T
        }
        
        # set breeding back to false without releasing a brood
        if (b.UH[i] >= ubh) {breeding[i] <- F}
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
  


  
  
  #### Simulating each individual on every day t -----------------------------------------------------
  pop.count <- length(l.start) # population size (including dead ones) at first day - needed for loop i
  for (t in seq(days + 1)) {
    
    ## Feeding 
    env.foodav[1] <- feed.sc[1]
    
    for (i in 1:pop.count) {
      if (alive[i] == T){
        
        # maximum ingestion rate (J/d)
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
        # breeding if puberty is reached 
        if ((UH[i] >= uph)) {
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
          # Individual Tolerance
          if(tox.opt == 1){
            # maximum scaled internal concentration
            ci[i] <- max(ci[i], ci[i] + (ke_it * (cw[t] - ci[i])))
            # threshold for effect
            Ft[i] <- 1 / (1 + ((ci[i] / t_alpha) ^ -t_beta))
            # survival probability
            s[i] <- (1 - Ft[i]) * (1 - Fe[i]) * exp(-h[i])
          }
          
          # Stochastic death  
          if(tox.opt == 2){
            # scaled internal concentration at time step t
            ci[i] <- ci[i] + (ke_sd * (cw[t] - ci[i])) 
            # hazard rate for effects
            ht[i] <- ht[i] + (kk * max(ci[i] - z, 0)) + h[i]
            if(ht[i] < 0){ht[i] <- 0}
            # survival probability
            s[i] <- (1 - Fe[i]) * exp(-ht[i])
          } 
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
      }
      
      ## Adding new born individuals
      # add new values of brood to individual vectors when birth maturity of brood is reached -> after calculation of survival -> new borns of dead individuals will not be added
      if (alive[i] == T){
        if ((b.UH[i] >= ubh)){       
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
              sex[length(sex) + 1] <- ifelse(sample(1:100, 1) < 50, sex[i] <- 'f', sex[i] <- 'm') # 50:50 ratio
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
                  ci[length(ci) + 1] <- 0 
                  Ft[length(Ft) + 1] <- 0
                  s[length(s) + 1] <- 1
                  ht[length(ht) + 1] <- -1
                }
                # Stochastic death  
                if(tox.opt == 2){
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
        } # adding brood if ((b.UH[i] >= ubh))
      } # if alive
    } # for loop - individual life cycles
    
    # count population size in each time step
    pop.size[t, 1] <- t
    pop.size[t, 2] <- sum(alive == T)
    # length of population vectors (= number of all individuals, including dead ones)
    pop.count <- length(s)
    
    
    # Progress Information
   # if(mc.on == F){
   #  print(c('Progress (%):',  round((t/(days+1))*100, digits = 1)), quote = FALSE)
   # }
    
  } # for t loop
  
  # matrix with results for each mc step
  mc.pop.size[, m] <- pop.size[, 2]
  
  # Progress Information
 # if(mc.on == T){
  #  print(c('Progress (%):',  round((m/mc.no)*100, digits = 0)), quote = FALSE)
  #}
  
} # monte carlo

# ------------------------------- END Simulation -----------------------------------------


# NSE
if (mc.on == T){mc.median <- cbind(c(0:days), mc.pop.quant[3, ])}
if (mc.on == F){mc.median <- cbind(c(0:days), pop.size[, 2])}
model_data <- mc.median[, c(1:2)]
sim_data <- model_data[ts+1,]
sim_data_v <- as.vector(sim_data[,2])
nse <- NSE(sim_data_v, obs_data_mean)

cb <- (c(e_kd, e_alpha, e_beta, nse))
nse_m <- rbind(cb, nse_m)


print(c('Progress (%):',  ((nrow(nse_m)-1)/combinations) * 100), digits = 2, quote = FALSE)

    }
  }
}
write.csv(nse_m, file = "nse_matrix_run3_11grad.csv")

