# DEB IBM Model with hourly calculated feeding and iterations (final)


#set.seed(42)

# set working directory 
setwd("/Users/dino/Dropbox/Uni/Master/Masterarbeit/R_Masterarbeit")

# -------------------------------Options -------------------------------------------------
# days to be simlated
days      <- 42
# Volume of environment
env.size  <- 900  # mL
# no. of steps for calculation of changes in smaller steps
steps     <- 100
iter      <- 2

# Crowing
crowding  <- T
# Adaptation
adaptation <- F

## Monte Carlo
# MC on / off (1/0 or TRUE/FALSE)
mc.on <- T
# Number of Monte Carlo Simulations
mc.no <- 100

## Start Population
# size class 1
l.start_1 <- 1.0    # physical size in mm # 1.0
no.start_1 <- 5     # quantity           # 5
# size class 2
l.start_2 <- 3.2    # physical size in mm # 3.2
no.start_2 <- 3     # quantity           # 3
# size class 3
l.start_3 <- 1.8    # physical size in mm
no.start_3 <- 0     # quantity
# standard deviation for size classes
start.size.sd <- 0.2

## Feeding
# medium renewal on/off
feed.med.change <- T
# days after which medium and food is renewed (7 = weekly)
feed.med.days   <- 7 
# feeeding scenario
#feed.sc <- rep(c(0.5,0.5,0.5,0.5,0.5,0.5,0.5), trunc(days/7) + 1)
#feed.sc <- rep(c(0.5,0.5,0.5,0.5,1.5,0,0), trunc(days/7) + 1)
feed.sc <- rep(c(0.5,0.5,1.5,0,0,0.5,0.5), trunc(days/7) + 1)
#feed.sc <- rep(c(1.3,1.3,1.3,1.3,3.9,0,0), trunc(days/7) + 1)
#feed.sc <- rep(c(1.3,1.3,1.3,1.3,1.3,1.3,1.3), trunc(days/7) + 1)
#feed.sc <- rep(c(1.3,1.3,3.9,0,0,1.3,1.3), trunc(days/7) + 1)
# read csv file with feeding data
#feed.sc <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/food_scenario_0.5.csv", header = FALSE, sep = "", dec = ",")$V1
#feed.sc <- read.csv("/Users/dino/Dropbox/Uni/Master/Masterarbeit/food_scenario_1.3.csv", header = FALSE, sep = "", dec = ",")$V1


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


# ------------------------------Constant Parameters (for all individuals) ----------------
# Growth and Reproduction
EG      <- 0.00179     # Volume-specific cost for structure
g       <- 0.1502136   # energy investment ratio    =  (0.00179 / (0.678 * (0.0145/0.825)))   # 0.17
km      <- 1.599       # somatic maintencance rate coefficient (d^-1)
v       <- 0.825       # energy conductance (mm/d)
dm      <- 0.54        # shape coefficient (-)
Li      <- 0.0000001   # Initial volumetric structural length (mm)
kappa   <- 0.678       # allocation fraction to soma (-)
kj      <- 0.969       # maturity maintenance rate coefficient (d^-1)
kr      <- 0.95        # reproduction efficiency (-)
ubh     <- 0.012       # scaled maturity at birth (mm^2 * d)
uph     <- 0.1997      # scaled maturity at puberty (mm^2 * d)
u0e0    <- 0.089       # cost of an egg (mm^2 * d)

# Food dependent survival
ha      <- 0.00029     # weibull aging acceleration constant (d^-2)
sg      <- 0.40        # gompertz aging stress coefficient (-)
e_kd    <- 0.09        # damage recovery rate constant (d^-1)
e_alpha <- 0.41        # median of threshold distribution (-)
e_beta  <- 4           # slope of threshold distribution (d^-1)

# Feeding and Assimilation
Fx0     <- 2.97        # Surface-area-specific filtration rate (mL / (mm^2 * h))
xk      <- 0.00022     # half saturation constant (mg/mL)
ill     <- 0.00036     # Incipient limiting level (mg/mL)
e0      <- 0.63        # Scaled reserved density threshold for plasticity
e_t     <- 0.139       # tolerance scaled reserve density for plasticity
fa      <- 10.4        # Surface-area-specific filtration rate adaptive plasticity
pa_m    <- 0.0145      # Surface-area-specific maximum assimilation rate (mg / (mm^2 * d))
px_min  <- 0.58        # minimum assimilation efficiency (-)
px_max  <- 0.95        # maximum assimilation efficiency (-)

# GUTS
ke_it <- 0.0016 * 24  # dominant rate constant for IT [h^-1] (Gergs et al. 2016)
ke_sd <- 0.0029 * 24  # dominant rate constant for SD [h^-1] (Gergs et al. 2016)
kk <- 0.0133 * 24     # h^-1 killig rate (SD) (from Gergs et al. 2016)
z <- 0.2102           # µg/L  threshold for effect (SD) (from Gergs et al. 2016)
t_alpha <- 1.5614     # µg/L median of threshold distribution (IT) (Gergs et al. 2016)
t_beta <- 2.1420 * 24 # h^-1 shape parameter of threshold distribution (IT) (Gergs et al. 2016)

# Crowding and Adaptation Parameters
d0_F   <- 0.37        # no-effect population density for Fx (#/mL) # 0.37
dT_F   <- 0.95        # tolerance population density for Fx (#/mL) # 0.95
d0_UE  <- 0.021       # no-effect population density for u0e (#/mL) # 0.021
dT_UE  <- 0.59        # tolerance population density for u0e (#/mL) # 0.59

# brood
g0 <- g
km0 <- km
kappa0 <- kappa
kj0 <- kj
q0    <- 0            # q at birth
h0    <- 0            # hazard rate at birth
D_e0  <- 0            # scaled damage at birth


# ------------------------------Matrices, Vectors and Monte Carlo Start -------------------

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
  for (x in 1:length(l.start)) {
    l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = start.size.sd)
    repeat {if ((l.rnorm[x] < (l.start[x] + 1 * start.size.sd)) | (l.rnorm[x] > (l.start[x] - 1 * start.size.sd))) break else {
      l.rnorm[x] <- rnorm(1, mean = l.start[x], sd = start.size.sd)
    }}
  }
  
  # matrix for population size
  pop.size <- matrix(data = NA, nrow = days + 1, ncol = 2)
  colnames(pop.size) <- c("t", "pop.size")
  
  # vectors - also clears vectors after each monte carlo step
  Jx <- c()
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
  px <- c()
  Jxm <- c()
  pa <- c()
  foodav <- c()
  pop.feed.pot <- c()
  pop.feed.real <- c()
  Fx <- c()
  kappa <- c()
  g <- c()
  u0e <- u0e0
  
  
  
  # ------------------------------BEGIN Code for Simulation ----------------------------------------
  #### Simulating Start Population until desired start size of individuals
  
  ### start values for start population (day 0 for start individuals)
  for (i in 1:length(l.start)) {
    # desired structural length at test start for each individual in start population
    L.s[i] <- l.rnorm[i] * dm
    
    # length at start
    L[i]  <- dm * sample(95:115, 1) / 100
    
    ## Feeding
    Fx[i] <- Fx0
    kappa[i] <- kappa0
    g[i] <- g0
    Jx[i] <- 0
    Jxm[i] <- 0
    pa[i] <- 0
    f[i] <- 0.7
    
    # reserve dynamics
    e[i]  <- 0.7
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
    
    # survival
    q[i]   <- q0    # aging acceleration q
    h[i]   <- h0    # hazard rate h
    D_e[i] <- D_e0  # maximum scaled damage e_D
    Fe[i]  <- 0   # 1 / (1 + ((max(D_e[i]) / e_alpha) ^ -e_beta)) # threshold for starvation Fe
    s[i]   <- 1  #  (1 - Fe[i]) * exp(-h[i]) # survival probability s
    rs[i] <- sample(0:1000000, 1) / 1000000   # individual survival probability
    alive[i] <- T
    
    # toxicity
    ci[i]  <- 0  # scaled internal concentration
    ht[i]  <- 0  # hazard rate for effects
    Ft[i]  <- 0
    
  } # loop for each start individual
  
  ### further life cycle of start individuals until desired length
  for (i in 1:length(l.start)) {
    repeat {if (L[i] >= L.s[i]) break else {
      ##  Growth, maturation, survival and reproduction for each individual
      ## growth
      dL[i] <- 0
      f[i] <- 0.7
      ## Reserve dynamics
      for(y in seq(steps)){
        # scaled body length (-)
        if (e[i] > 0) {
          sl[i]  <- (L[i] * g0 * km) / (v * e[i])
        } else {
          sl[i] <- 0
        }
        
        # for growth conditions
        if (e[i] >= sl[i]) {
          # mobilisation flux (mm^2)
          Sc[i] <- L[i] ^ 2 * ((g0 * e[i]) / (g0 + e[i])) * (1 + (km * L[i] / v))
          # increase in length (mm)
          dL[i] <- dL[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g0) * Sc[i]) - km * L[i] ^ 3)) / steps )
          # new length
          L[i]  <- L[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g0) * Sc[i]) - km * L[i] ^ 3)) / steps )
        }
        else {
          # for non-growth conditions
          Sc[i] <- (kappa0 * km * g0 * L[i] ^ 3) / v
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
      if (e[i] > (f[i] * (L[i] / (v / (km * g0))))){
        # if individual has not yet reaced puberty, then energy flux to maturation
        if (UH[i] < uph) {UH[i] <- UH[i] + (((1 - kappa0) * Sc[i]) - (kj * UH[i])) }   
        # if individual has reached puberty then energy flux into reproductive buffer   
        if (UH[i] >= uph)  {UR[i] <- UR[i] + (((1 - kappa0) * Sc[i]) - (kj * uph)) }
        if (UR[i] < 0) {UR[i] <- 0}
      }
      
      
      ## Reproduction
      # breeding if puberty is reached 
      if ((UH[i] >= uph)) {         
        if (breeding[i] == T) {
          b.Sc[i] <- b.L[i] ^ 2 * ((g0 * b.e[i]) / (g0 + b.e[i])) * (1 + (km0 * b.L[i] / v))
          b.L[i] <- b.L[i] + ((1 / (3 * b.L[i] ^ 2)) * (((v / g0) * b.Sc[i]) - km0 * b.L[i] ^ 3))
          b.UH[i] <- b.UH[i] + (((1 - kappa0) * b.Sc[i]) - (kj0 * b.UH[i]))  
        }
        
        if (breeding[i] == F) {
          b.L[i] <- Li # structural length at egg formation
          b.e[i] <- e[i]  # scaled reserve density of brood equals those of the mother at egg formation (different to DEB Theory)
          b.UH[i] <- 0  # scaled maturity at egg formation
          R[i]  <- trunc(kr * UR[i] / u0e0)  # calculation of brood size
          UR[i] <- UR[i] - ((R[i] * u0e0) / kr) # resets reproduction buffer but leaves rest of the buffer that could not be used to produce full egg
          breeding[i] <- T
        }
        
        # set breeding back to false without releasing a brood
        if (b.UH[i] >= ubh) {breeding[i] <- F}
      } # Reproduction if ((UH[i] >= uph) & (UR[i] > 0))
      
      
      ## Survival
      # aging acceleration
      q[i] <- q[i] + (((q[i] * (L[i] ^ 3 / (v / (g0 * km)) ^ 3) * sg ) + ha ) * e[i] * ((v / L[i]) - ((3 * dL[i]) / L[i])) - ((3 * dL[i]) / L[i]) * q[i]) 
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
  
  
  # one adult without eggs at test start
  for (i in 1:length(l.start)) {
    if (i == 8){
      b.UH[i] <- 0
      R[i] <- 0
      UR[i] <- 0
    }
  }
  
  
  
  #### Simulating each individual on every day t -----------------------------------------------------
  pop.count <- length(l.start) # population size (including dead ones) at first day - needed for loop i
  for (t in seq(days + 1)) {
    for (i in 1:pop.count){
      if (alive[i] == T){
        ## Adaptive plasticity
        if (adaptation == T){
          # adaptation of engery fraction allocated to soma
          kappa[i] <- min(1, kappa0 * (1 + (1 / e_t) * max(0, ((1 - e[i]) - (1 - e0))))) 
          g[i] <- EG * v / (kappa[i] * pa_m)
          # adaptation of filtration rate
          Fx[i] <- Fx0 * (1 + fa * L[i] ^ 2 * max(0, ((1 - e[i]) - (1 - e0)))) 
        }
        
        ## Crowding
        if (crowding == T){
          sd_F <- 1 + (1 / dT_F) * max(0, ((sum(alive == T) / env.size) - d0_F))
          # Stress function for cost of an egg
          sd_UE <- 1 + (1 / dT_UE) * max(0, ((sum(alive == T) / env.size) - d0_UE))
          # increased cost of an egg due to density stress
          u0e <- u0e0 * sd_UE
          
          if (adaptation == F){
            Fx[i] <- Fx0 / sd_F}
          if (adaptation == T){
            Fx[i] <- Fx[i] / sd_F}
        }
      } # if alive
      if(alive[i] == F){
        kappa[i] <- -1
        g[i] <- -1
        Fx[i] <- -1
      }
    } # for i
    
    
    ## Feeding 
    env.foodav[1] <- feed.sc[1]   # available food in environment on day 1 = first entry of vector containing feeding rates per day
    pop.feed[t] <- 0              # set population feeding rate to 0
    # reset daily individual assimilation (reseted on each day)
    for (i in 1:pop.count){
      pa[i] <- 0
    }
    # reset hourly feeding rates (reseted on each day)
    for (c in 1:24){
      pop.feed.pot[c] <- 0
      pop.feed.real[c] <- 0
    }
    
    # Calculate daily feeding of population and individual
    # loop for 24 hours
    for (c in 1:24){
      if (c == 1){
        foodav[c] <- env.foodav[t]  # at first hour whole food of day before (or of first day) is available
      }
      else{
        foodav[c] <- foodav[c-1] # rest of food of prior hour
      }
      
      
      if (foodav[c] > 0){
        # calculate potential feeding rate assuming unlimited food
        for (i in 1:pop.count){
          Jx[i] <- 0  # resets individual ingestion rate (reseted per hour)
          # calculate the sum of individual feeding rates for each individual i at hour c 
          if (alive[i] == T){
            pop.feed.pot[c] <- pop.feed.pot[c] + ((ill * Fx[i] * L[i] ^ 2) * ((foodav[c] / env.size) / (xk + (foodav[c] / env.size))))
          }
        }
        
        # if enough food is available
        if (pop.feed.pot[c] <= foodav[c]){
          # assimilation efficiency
          px <- px_max - (((px_max - px_min) * (foodav[c] / env.size)) / (xk + (foodav[c] / env.size)))
          # calculate ingestion and assimilation rate for each living individual
          for (i in 1:pop.count){
            if (alive[i] == T){
              # maximum ingestion rate (mg/h)
              Jxm[i] <- ill * Fx[i] * L[i] ^ 2
              # food ingestion rate (mg/h)
              Jx[i] <- Jxm[i] * ((foodav[c] / env.size) / (xk + (foodav[c] / env.size)))
              # assimilation rate (mg/d)
              pa[i] <- pa[i] + (px * Jx[i])  
              
            } # if alive
            # keep all vectors at same length
            if (alive[i] == F){
              Jx[i] <- -1
              Jxm[i] <- -1
              pa[i] <- -1
            }
          } # for i
          # realized feeding of population equals potential feeding
          pop.feed.real[c] <- pop.feed.pot[c]
          foodav[c] <- foodav[c] - pop.feed.real[c]
        }
        
        # if population wants to eat more then available -> reduce time interval
        if (pop.feed.pot[c] > foodav[c]){
          # assimilation efficiency
          px <- px_max - (((px_max - px_min) * (foodav[c] / env.size)) / (xk + (foodav[c] / env.size)))
          # calculate feeding for shorter timesteps by means of iterations d
          for (d in 1:iter){
            if (foodav[c] > 0){
              for (i in 1:pop.count){
                  # ingestion rate of individual
                  Jx[i] <- Jx[i] + (((ill * Fx[i] * L[i] ^ 2) * ((foodav[c] / env.size) / (xk + (foodav[c] / env.size)))) / iter) 
                  
                  if (alive[i] == T){
                  # population feeding rate
                   pop.feed.real[c] <- pop.feed.real[c] + ( ( (ill * Fx[i] * L[i] ^ 2)* ((foodav[c] / env.size) / (xk + (foodav[c] / env.size))) ) / iter)
                  # actual food availability
                   foodav[c] <- foodav[c] - (( (ill * Fx[i] * L[i] ^ 2) * ((foodav[c] / env.size) / (xk + (foodav[c] / env.size))) ) / iter)
                  }
                  
                   # assimilation rate (mg/d)
                   pa[i] <- pa[i] + (px * (((ill * Fx[i] * L[i] ^ 2) * ((foodav[c] / env.size) / (xk + (foodav[c] / env.size)))) / iter))
              } # for i
            } # if foodav > 0
          } # for d
          if (foodav[c] < 0){foodav[c] <- 0}
        } # else (not enough food)
      } # if foodav > 0
    } # for c in 1:24
    
    
    # available food on next day
    # if medium is changed on this day
    if (((t / feed.med.days) == trunc(t / feed.med.days)) & (feed.med.change == TRUE)){
      env.foodav[t + 1] <- feed.sc[t + 1]  # available food in environment on next day = value for feeding in feeding scenario vector for next day
    }
    # medium not changed on this day
    else{
      env.foodav[t + 1] <- foodav[24] + feed.sc[t + 1] # available food in environment on next day = rest of food after 24 hours + added food on next day 
    }
    
    # sum up hourly realized feeding rate to daily feeding rate
    for (c in 1:24){
      pop.feed[t] <- pop.feed[t] + pop.feed.real[c]
    }
    
    # scaled functional response (-) per individual
    for (i in 1:pop.count) {
        f[i] <- pa[i] / (pa_m * L[i] ^ 2)}
        if (f[i] > 1){f[i] <- 1}

     
    ##  Growth, maturation, survival and reproduction for each individual
    for (i in 1:pop.count) {
      if (alive[i] == T) {
        ## growth
        dL[i] <- 0
        
        ## Reserve dynamics
        for(y in seq(steps)){
          # scaled body length (-)
          if (e[i] > 0) {
            sl[i]  <- (L[i] * g[i] * km) / (v * e[i])
          } else {
            sl[i] <- 0
          }
          
          # for growth conditions
          if (e[i] >= sl[i]) {
            # mobilisation flux (mm^2)
            Sc[i] <- L[i] ^ 2 * ((g[i] * e[i]) / (g[i] + e[i])) * (1 + (km * L[i] / v))
            # increase in length (mm)
            dL[i] <- dL[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g[i]) * Sc[i]) - km * L[i] ^ 3)) / steps)
            # new length
            L[i]  <- L[i] + ((((1 / (3 * L[i] ^ 2))) * (((v / g[i]) * Sc[i]) - km * L[i] ^ 3)) / steps)
          }
          else {
            # for non-growth conditions
            Sc[i] <- (kappa[i] * km * g[i] * L[i] ^ 3) / v
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
        if (e[i] > (f[i] * (L[i] / (v / (km * g[i]))))){
          # if individual has not yet reaced puberty, then energy flux to maturation
          if (UH[i] < uph) {UH[i] <- UH[i] + (((1 - kappa[i]) * Sc[i]) - (kj * UH[i])) }   
          # if individual has reached puberty then energy flux into reproductive buffer
          if (UH[i] >= uph)  {UR[i] <- UR[i] + (((1 - kappa[i]) * Sc[i]) - (kj * uph)) }
          if (UR[i] < 0) {UR[i] <- 0}
        }
        
        
        ## Reproduction
        # breeding if puberty is reached 
        if ((UH[i] >= uph)) {
          if (breeding[i] == T) {
            b.Sc[i] <- b.L[i] ^ 2 * ((g0 * b.e[i]) / (g0 + b.e[i])) * (1 + (km0 * b.L[i] / v))
            b.L[i] <- b.L[i] + ((1 / (3 * b.L[i] ^ 2)) * (((v / g0) * b.Sc[i]) - km0 * b.L[i] ^ 3)) 
            b.UH[i] <- b.UH[i] +  (((1 - kappa0) * b.Sc[i]) - (kj0 * b.UH[i]))
          }
          
          if (breeding[i] == F) {
            b.L[i] <- Li # structural length at egg formation
            b.e[i] <- e[i]  # scaled reserve density of brood equals those of the mother at egg formation (different to DEB Theory)
            if (crowding == T) {b.e[i] <- min(1, e[i] * u0e / u0e0)}
            b.UH[i] <- 0  # scaled maturity at egg formation
            R[i]  <- trunc(kr * UR[i] / u0e)  # calculation of brood size
            UR[i] <- UR[i] - ((R[i] * u0e) / kr) # resets reproduction buffer but leaves rest of the buffer that could not be used to produce full egg
            breeding[i] <- T
          }
        } # Reproduction if UH[i] >= uph
        
        
        ## Survival
        # aging acceleration
        q[i] <- q[i] + (((q[i] * (L[i] ^ 3 / (v / (g[i] * km)) ^ 3) * sg ) + ha ) * e[i] * ((v / L[i]) - ((3 * dL[i]) / L[i])) - ((3 * dL[i]) / L[i]) * q[i])
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
        Jx[i] <- -1
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
        b.L[i] <- -1
        b.e[i] <- -1
        b.UH[i] <- -1
        b.Sc[i] <- -1
        ci[i] <- -1
        Ft[i] <- -1
        ht[i] <- -1
        Jxm[i] <- -1
        pa[i] <- -1
        Fx[i] <- -1
        kappa[i] <- -1
        g[i] <- -1
      }
      
      ## Adding new born individuals
      # add new values of brood to individual vectors when birth maturity of brood is reached -> after calculation of survival -> new borns of dead individuals will not be added
      if (alive[i] == T){
        if ((b.UH[i] >= ubh)){       
          if (R[i] >= 1) {
            for (b in 1:(R[i])) {
              # parameters from mother
              L[length(L) + 1] <- b.L[i]
              e[length(e) + 1] <- b.e[i]
              UH[length(UH) + 1] <- b.UH[i]
              Sc[length(Sc) + 1] <- 0
              # remaining parameters
              Jx[length(Jx) + 1] <- 0
              Jxm[length(Jxm) + 1] <- 0
              pa[length(pa) + 1] <- 0
              f[length(f) + 1] <-  b.e[i]
              UE[length(UE) + 1] <- (e[length(e)] * L[length(L)] ^ 3) / v
              sl[length(sl) + 1] <- (L[length(L)] * g0 * km) / (v * e[length(e)])
              dL[length(dL) + 1] <- 0
              UR[length(UR) + 1] <- 0
              R[length(R) + 1] <- 0
              breeding[length(breeding) + 1] <- F
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
              Fx[length(Fx) + 1] <- Fx0
              kappa[length(kappa) + 1] <- kappa0
              g[length(g) + 1] <- g0
              
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
    if(mc.on == F){
      print(c('Progress (%):',  round((t/(days+1))*100, digits = 1)), quote = FALSE) 
    }
    
  } # for t loop
  
  # matrix with results for each mc step
  mc.pop.size[, m] <- pop.size[, 2]
  
  # Progress Information
  if(mc.on == T){
    print(c('Progress (%):',  round((m/mc.no)*100, digits = 0)), quote = FALSE)
  }
  
} # monte carlo

# ------------------------------- END Simulation -----------------------------------------

# ------------------------------ Results --------------------------------------------------
## Matrices and Plots with Monte Carlo
if (mc.on == TRUE) {
  mc.pop.mean <- rowMeans(mc.pop.size, na.rm = TRUE)
  mc.pop.min <- apply(mc.pop.size, 1, min, na.rm = TRUE)
  mc.pop.max <- apply(mc.pop.size, 1, max, na.rm = TRUE)
  mc.pop.quant <- apply(mc.pop.size, 1, quantile, na.rm = TRUE)
  
  
  plot(c(0:days), mc.pop.quant[5, ], type = "l", lty = 2, xlab = "time (d)", ylab = "population size (#)", ylim = c(0,max(mc.pop.quant[5, ])), cex.axis = 1.2, cex.lab = 1.2) # max 100% quantil
  lines(c(0:days), mc.pop.quant[1, ], type = "l", lty = 2) # min 0% quantil
  lines(c(0:days), mc.pop.quant[3, ], type = "l", lty = 1) # median
  # lines(seq(days + 1), mc.pop.quant[2, ], type = "l", lty = 3) # 25% quantil
  # lines(seq(days + 1), mc.pop.quant[4, ], type = "l", lty = 3) # 75% quantil
  # text(days - days * 0.0, c(max(mc.pop.size) - 15,max(mc.pop.size) - 25,max(mc.pop.size) - 35,max(mc.pop.size) - 45,max(mc.pop.size) - 55),
  #     labels = c(tox.on, tox.opt, tox.t.start, tox.t.end, max(cw)),cex = 0.8)
  #text(days - days * 0.2, c(max(mc.pop.size) - 15,max(mc.pop.size) - 25,max(mc.pop.size) - 35,max(mc.pop.size) - 45,max(mc.pop.size) - 55),
  #     labels = c("tox.on","tox.opt","tox.t.start","tox.t.end","cw"),cex = 0.8)
  text(days - days * 0.0, c(max(mc.pop.size) - 15,max(mc.pop.size) - 25,max(mc.pop.size) - 35),
       labels = c(mc.no, steps, feed.sc[7]), cex = 0.8)
  text(days - days * 0.2, c(max(mc.pop.size) - 15,max(mc.pop.size) - 25,max(mc.pop.size) - 35),
       labels = c("MC runs","Steps","Feeding"), cex = 0.8)
}

## Matrices and Plots for single simulation
if (mc.on == FALSE) {
  m.pop.size <- cbind(c(0:days), pop.size)
  colnames(m.pop.size) <- c("day", "t", "pop.size")
  m.ind <-cbind(L,Jx,Jxm,pa,f,UE,e,sl,Sc,dL,UH,UR,R,breeding,b.L,b.e,b.UH,b.Sc,q,h,D_e,Fe,s,rs,ci,Ft,ht,alive)
  m.food <- cbind(c(0:(days)), pop.feed, env.foodav[1:c(days + 1)])
  colnames(m.food) <- c("day", "consumed", "available")
  
  plot(m.pop.size[, 1], m.pop.size[, 3], type = "l", xlab = "time (d)", ylab = "Population size (#)")
  plot(m.food[, 1], m.food[, 3], type = "l", lty = 1, xlab = "time (d)", ylab = "Amount of food (mgC/mL)", ylim = c(0,max(m.food[, 3])))
  lines(m.food[, 1], m.food[, 2], type = "l",  lty = 2)
  #legend("topright", legend = c("available", "consumed"), lty = c(1, 2))
}


# -------------------------------- Profiling and Data Export ----------------------------------

#Rprof(NULL)
#summaryRprof(filename="Rprof_vector11.out")


# export data
#write.csv(m.ind, file = "deb_model_v10_ind.csv")
#write.csv(mc.pop.size, file = "deb_model_v10_mc_pop_size.csv")
#write.csv(m.food, file = "deb_model_v10_mc_food.csv")


if (mc.on == T){mc.median <- cbind(c(0:days), mc.pop.quant[3, ])}
if(mc.on == F){mc.median <- cbind(c(0:days), pop.size[, 2])}

write.csv(mc.median[, c(1:2)], file = "median_temp.csv")


write.csv(mc.pop.size, file = "2018_01_29_result_daph_final_1000mc.csv")




