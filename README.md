# Description

This is analysis pipline for taking data from participants navigating a virtual robot through a maze environment. Participants engaged in 4 conditions where they experienced
possible degredation of visual ability while navigating, or the possible addition of haptic feedback when approaching ojects (see figure below for all combinations of effects). 
This analysis piplines explores performance metrics including velocity, force input, percieved workload, and time to completion.

<img width="373" alt="Screenshot 2024-09-05 at 7 55 51 AM" src="https://github.com/user-attachments/assets/f01512dc-1f24-4943-a206-dfb84360940b">

Please see publication for more of a general overview of how this pipeline is being used to conduct research: 
https://ieeexplore.ieee.org/document/10555743/


# Overview

This analysis pipeline makes use of the targets package to keep track of analysis and script order. To view the pipeline, do the following: 

Run the sript < _targets.R > 

in the command line, run the following
> tar_make()

> tar_visnetwork()

example of output (you version you will be able to zoom and explore the various nodes):

<img width="797" alt="Screenshot 2024-09-05 at 8 09 48 AM" src="https://github.com/user-attachments/assets/b54e9dfe-d52b-46c4-9615-288347caee1a">


