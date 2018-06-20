# masterthesis
Repo for my masterthesis concerning individual-based models and the dynamic energy budget theory, containing all R-code.

## Description of material

 - *Asellus* model: *model_assel_final_v1.3.R*
 - *Asellus* model: (parallelised code): *model_assel_final_v1.3_dopar.R*
 - shiny App of the *Asellus* model: *app.R*




## How to use the model

By executing the whole code the model will be run as defined in the *Options* section (see below). The progress of the simulation is shown in the console. After the simulation has finished a graph is plotted and the results are saved as a csv file in your working directory.


### Options:

*Note: Main options can be set in the options section in the beginning of the code. For options, which can be activated or deactivated, use T (TRUE => on) or F (FALSE => off).*

- **days:** number of days the population will be simulated

- **size of environment:** volume of environment in liters

- **temperature:** you can either use a constant temperature or a scenario, where the temperature can be defined for each day as a vector. A csv file can be read in, as well.

- **reproduction:** here you can exclude reproduction. New individuals are not added to the population. Processes on the individual level are not changed (for testing purposes and starvation model).

- **Monte Carlo iterations:** If this option is activated, the simulation will be run repeatedly, simulating a random sampling. You can turn this function on or off and specify the number of simulations (mc.no). Note that this increases the time to finish the simulation.

- **start population:** You can define up to three size classes for the start population. For each class you can specify the length in cm, the standard deviation of the length, and the quantity.

- **sex of individuals:** Here you can choose the sex ratio of the start population. There are four possibilities:
	- equal to those of the laboratory test
	- a 50:50 frequency allocating the sex alternately from the first to the last individual
	- only males or females
	- sex is allocated randomly

- **feeding:** You can define how food is added to the environment using several options:
	- Medium renewal: the medium used in the environment can be changed regularly. This option can be tured on or off.
	  Renewing the medium also removes remaining food.
	- changing period in days when the medium is renewed
	- feeding scenario: 
	
- **Toxic exposure:** You can define a toxic scenario by either:
	- entering day of start and end of exposure with a constant concentration of CPF *or*
	- loading a scenario from a csv file, where the concentration is set for each day. It is imported as a vector.
