# masterthesis
Repo for my masterthesis. An individual-based model based on the dynamic energy budget theory was implemented in R, simulating populations of *Asellus aquaticus*. All final R-scripts can be found in this repo.

## Description of material

 - *Asellus* model: *model_assel_v1.4.R*
 - *Asellus* model with parallelised code: *model_assel_v1.4_dopar.R*
 - *Asellus* model for ecological scenarios: *model_assel_scenarioloop_v1.4.R*
 - *Daphnia* model: *model_daphnia.R*
 - shiny App of the *Asellus* model: *app.R*
 
 - R-Script for evaluation of recovery of ecological scenarios: *recovery_assessement.R*
 - Creation of graph comparing results of simulation model and population test: *poptest_eval.R*
 - Evaluation of toxicity test (EC50, LC50, plots): *toxtest_eval.R*
 - Evaluation of variation as a function of the number of Monte Carlo simulations (figure A6): *MC_variation.R*
 - Determination of parameters for starvation: *starvation_paramteres.R*
 - Calculation of Nash–Sutcliffe model efficiency coefficient: *NSE.R*
 
## How to use the model

By executing the whole code the model will be run as defined in the *Options* section (see below). The progress of the simulation is shown in the console. After the simulation has finished three graphs, showing the population size, the available food, and the consumed food over time, are plotted and the results are saved as a csv file in your working directory.


### Options:

*Note: Main options can be set in the options section in the beginning of the code. For options, which can be activated or deactivated, use T (TRUE => on) or F (FALSE => off).*

- **Monte Carlo iterations:** Set the number of Monte-Carlo iterations. Note that this increases the time to finish the simulation. In the R-script without parallelisation you can turn this option on or off additionaly.

- **days:** number of days the population will be simulated

- **size of environment:** volume of environment in liters

- **temperature:** you can either set a constant temperature or load a scenario from a csv file, where the temperature can be defined for each day.

- **reproduction:** here you can exclude reproduction. Then, new individuals are not added to the population. Processes on the individual level are not changed (for testing purposes and starvation model).



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
	
- **Toxic exposure:** You can choose how the toxic exposure should be defined. You can either:
	- use the temperature dependent DT50 for chlorpyrifos and define the start day and the initial concentration. Only  	      single exposure is possible here (option 'd') *or*
	- loading an external scenario from a csv file, where the concentration is set for each day. It is imported as a 	   vector (option 'l') *or*
	- create a simplified scenario by setting start and end day of exposure. Water concentration will be constant over 	     time (option 's')
