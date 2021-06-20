# Self-Organized Collective Escape of Pigeons

This repository contains code and data used in the project on the distance-dependent pattern of collective escape in flocks of pigeons, currently submitted for peer review:

*Papadopoulou, M., Hildenbrandt H., Sankey, D.W.E., Portugal S.J.,and Hemelrijk, C.K. "Self-organization of collective escape in pigeon flocks". Submitted.*

The code provided is open source, but we ask you to cite the above paper if you make use of it. 

## Code

The computational model (in C++) used to produce the simulated data can be found here: https://github.com/marinapapa/HoPE-model .
The analysis on empirical and simulated data is performed in _R_, version 3.6 or later. See DESCRIPTION for details on depedencies. 

- Files that reproduce the respective figures of the manuscript: *figX.R* 
- Files with helper functions for analysis and ploting: *count_switch_esc_dir.R, load_data.R, neighb_rel_pos.R, prepare_sim_data_for_turn_dir.R, subplots_functions.R* 
- Files for statistical analysis: *analyze_empirical.R*, *analyze_turn_direction_trend.R* 

## Data

You can download all data from this Zenodo repository:
 
To smoothly run the code files, the subdirectories of the downloaded data should be copied locally in this *Data* folder. The dataset includes:

**Empirical data**: collected and preprocessed by Daniel W. E. Sankey, first published in:  
*Sankey DWE, Storms RF, Musters RJ, Russell WT, Hemelrijk CK, Portugal SJ. (2021) Absence of “selfish herd” dynamics in bird flocks under threat. Current Biology. https://doi.org/10.1016/j.cub.2021.05.009.*
They comprise analysed GPS tragectories of flocks of homing pigeons. 

**Simulated data**: extracted by the computational model [*HoPE*](https://github.com/marinapapa/HoPE-model) (Homing Pigeons Escape), an agent-based model adjusted to the collective escape of homing pigeons. 

The files/folders necessary for the code to run are:

1. **turning_direction** folder in the *empirical* folder: contains the empirical data from the analysis on turning direction frequency
2. **transformed** folder in the *empirical* folder: contains the empirical data of Sankey *et al.* (2021) restructured according to the output of the computational model
3. **HoPE_track_eg** folder in the *simulated_raw* folder: an example output folder that is also used in Figure 6. The repository contains also all the raw output of all our experiments.
4. **turning_direction** in the *simulated* folder: the results of the turning direction frequency analysis on the simulated data
5. & 6. **sim_datas_chase.RData** and **sim_datas_noavoid.RData** in the *simulated* folder: the imported data from our main simulations (the supplementary simulations are also included in this folder).

## Results
Folder in which *figX.R* scripts export the figures.  

## Contact
* For any issue or more information on this repository, email **Marina Papadopoulou** at: <m.papadopoulou.rug@gmail.com>
