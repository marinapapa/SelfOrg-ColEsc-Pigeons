# Self-Organized Collective Escape of Pigeons

This repository contains code and data used in the project on the distance-dependent pattern of collective escape in flocks of pigeons, currently submitted for peer review:

*Papadopoulou, M., Hildenbrandt H., Sankey, D.W.E., Portugal S.J.,and Hemelrijk, C.K. "Self-organization of collective escape in pigeon flocks". Submitted.*

The code provided is open source, but we ask you to cite the above paper if you make use of it. 

## Code

The computational model (in C++) used to produce the simulated data can be found here: https://github.com/marinapapa/HoPE-model .
The analysis on empirical and simulated data is performed in _R_, version 3.6 or later. See DESCRIPTION for details on depedencies. 

- Files that reproduce the respective figures of the manuscript: *figX.R* 
- Files with helper functions for analysis and ploting: *count_switch_esc_dir.R, load_data.R, prepare_sim_data_for_turn_dir.R, subplots_functions.R* 
- Files for statistical analysis: *analyze_empirical.R*, *analyze_turn_direction_trend.R* 

## Data

You can download all data from this Zenodo repository: 
To smoothly run the code files, the subdirectories of the downloaded data should be copied locally in this *Data* folder.

**Empirical data** are collected and preprocessed by Daniel W. E. Sankey, first published in:  
*Sankey DWE, Storms RF, Musters RJ, Russell WT, Hemelrijk CK, Portugal SJ. (2021) Absence of “selfish herd” dynamics in bird flocks under threat. Current Biology. https://doi.org/10.1016/j.cub.2021.05.009.*
They comprise analysed GPS tragectories of flocks of homing pigeons.

- turning_direction: contains the data for the analysis on turning direction frequency
- transformed: contains the data restructured according to the output of the computational model

**Simulated data** are extracted by the computational model [*HoPE*](https://github.com/marinapapa/HoPE-model) (Homing Pigeons Escape), an agent-based model adjusted to the collective escape of homing pigeons. 

- simulated_raw: the folder contains a link to the repository with all the simulated data of all our experiments & an example output folder that is also used in Figure 6.
- simulated: the imported data from our main simulations (*sim_datas_chase.RData* and *sim_datas_noavoid.RData*), supplementary simulations, as well as the results of the turning direction frequency analysis.

## Results
Folder in which *figX.R* scripts export the figures.  

## Contact
* For any issue or more information on this repository, email **Marina Papadopoulou** at: <m.papadopoulou.rug@gmail.com>
