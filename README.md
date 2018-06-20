# masterthesis
Repo for my masterthesis concerning individual-based models and the dynamic energy budget theory, containing all R-code.

## Description of material

 - *Asellus* model: *model_assel_final_v1.3.R*
 - *Asellus* model (parallelised code): *model_assel_final_v1.3_dopar.R*
 




## How to use the model


1. Main options can be set in the options section beginning in line 11. For options, which can be activated or deactivated,   use T (TRUE => on) or F (FALSE => off).

- days:number of days the population will be simulated
- Size of environment: Volume of environment in liters
- Temperature: Temperature of environment in °C. Influences the metabolism of the organisms.

- Reproduction: Here, you can exclude reproduction. New individuals are not added to the population. Processes on the individual level are not changed.

- Monte Carlo: If this is activated, the simulation will be run repeatedly, simulating a random sampling. You can turn this function on or off and specify the number of simulations. Note that this increases the time to finish the simulation.

- Start Population: You can define up to three size classes for the start population. For each class you can specify the length in cm, the standard deviation of the length, and the quantity.

- Sexes: Here you can choose the sex ratio of the start population. There are four possibilities:
	- equal to those of the laboratory test
	- a 50:50 frequency allocating the sex alternately from the first to the last individual
	- only males or females
	- sex is allocated randomly

- Feeding: You can define how food is added to the environment using several options:
	- Medium renewal: the medium used in the environment can be changed regularly. This option can be tured on or off.
	  Renewing the medium also removes remaining food.
	- changing period in days when the medium is renewed
	- feeding scenario: 
