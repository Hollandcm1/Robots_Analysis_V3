# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)

# Set target options:
tar_option_set(
  packages = c("tibble", "R.matlab", "here", "openxlsx", "dplyr", "stringr", 
               "ggplot2", "tidyr", "purrr", "flexplot", "lme4", "sjPlot", "zoo", 
               "grid", "cowplot", "readxl","lmerTest", "ggbeeswarm"), # Packages that your targets need
  error = "continue"
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source(files = "R_targets_functions")

# Target List
list(
  
  ####################
  ### EXperiment 1 ###
  ####################
  
  # Codebook
  tar_target(
    name = codes,
    command = import_codebook()
  ),
  # Separate targets to extract each element
  tar_target(
    name = codes_conditions,
    command = codes[[1]]
  ),
  tar_target(
    name = codes_participant_conditions,
    command = codes[[2]]
  ),
  # Maps
  tar_target(
    name = maps,
    command = pull_maps(codes_participant_conditions, codes_conditions, environments)
  ),
  tar_target(
    name = maps_plotted,
    command = plot_maps(maps),
    #cue = tar_cue(mode = "always")
  ),
  # Condense Data
  tar_target(
    name = compiled_data,
    command = condense_data(codes_participant_conditions)
  ),
  # Participant Error Correction
  tar_target(
    name = corrected_data,
    command = participant_error_correction(compiled_data, codes_participant_conditions)
  ),
  # Mark Within Maze
  # tar_target(
  #   name = within_maze_marked_data,
  #   command = mark_within_maze(bad_trials_removed_data)
  # ),
  # Plot All Trials
  tar_target(
    name = all_trials_plots,
    command = plot_all_trials(corrected_data)
  ),
  # Build Data Long
  tar_target(
    name = data_long,
    command = build_data_long(bad_trials_removed_data, codes_conditions)
  ), 
  tar_target(
    name = bad_trials_removed_data,
    command = remove_bad_trials(corrected_data)
  ), 
  tar_target(
    name = data_long_calculated,
    command = data_long_calculations(data_long, maps)
  ),
  tar_target(
    name = average_force_ANVOA,
    command = run_ANOVA_average_force(data_long_calculated)
  ), 
  tar_target(
    name = max_force_ANVOA,
    command = run_ANOVA_max_force(data_long_calculated)
  ), 
  tar_target(
    name = velocity_ANVOA,
    command = run_ANOVA_velocity(data_long_calculated)
  ), 
  tar_target(
    name = time_ANVOA,
    command = run_ANOVA_time(data_long_calculated)
  ),
  tar_target(
    name = proximity_ANVOA,
    command = run_ANOVA_proximity(data_long_calculated)
  ),
  # tar_target(
  #   name = average_force_Regression,
  #   command = run_Regression_average_force(data_long_calculated)
  # ),
  # tar_target(
  #   name = max_force_Regression,
  #   command = run_Regression_max_force(data_long_calculated)
  # ),
  tar_target(
    name = possible_fighting_flagged_data,
    command = flag_possible_fighting(data_long_calculated)
  ), 
  tar_target(
    name = all_trials_force_plots,
    command = plot_all_trial_force(possible_fighting_flagged_data, maps)
  ),
  tar_target(
    name = all_trials_force_by_proximity_plots,
    command = plot_all_trial_force_by_proximity(possible_fighting_flagged_data, maps)
  ), 
  # tar_target(
  #   name = strategic_data,
  #   command = import_strategic_data()
  # ), 
  # tar_target(
  #   name = strategic_data_appended,
  #   command = append_strategic_behaviour(possible_fighting_flagged_data, strategic_data)
  # ),
  # tar_target(
  #   name = strategic_LME_analysis_results,
  #   command = strategic_LME_analysis(strategic_data_appended)
  # ),
  # tar_target(
  #   name = within_maze_plotted,
  #   command = plot_within_maze(within_maze_marked_data)
  # ),
  # tar_target(
  #   name = strategic_LME_analysis_only_haptic_results,
  #   command = strategic_LME_analysis_only_haptic(strategic_data_appended)
  # ), 
  #tar_target(
  #  report,
  #  tar_render("report.Rmd")
    #name = LME_report,
    #command = tar_render("create_LME_report.RMD")
  #)
  
  
  ####################
  ### Experiment 2 ###
  ####################
  
  # Codebook
  # tar_target(
  #   name = codes_exp2,
  #   command = import_codebook_exp2()
  # ),
  # Separate targets to extract each element
  # tar_target(
  #   name = codes_conditions_exp2,
  #   command = codes_exp2[[1]]
  # ),
  # tar_target(
  #   name = codes_participant_conditions_exp2,
  #   command = codes_exp2[[2]]
  # ),
  # Condense Data
  # tar_target(
  #   name = compiled_data_exp2,
  #   command = condense_data_exp2(codes_participant_conditions_exp2)
  # ),
  # Participant Error Correction
  # tar_target(
  #   name = corrected_data_exp2,
  #   command = participant_error_correction_exp2(compiled_data_exp2, codes_participant_conditions_exp2)
  # ),
  # plot
  # tar_target(
  #   name = all_trials_plots_exp2,
  #   command = plot_all_trials(corrected_data_exp2)
  # ),
  # tar_target(
  #   name = bad_trials_removed_exp2,
  #   command = remove_bad_trials_exp2(corrected_data_exp2)
  # ),
  # tar_target(
  #   name = within_maze_marked_data_exp2,
  #   command = mark_within_maze_exp2(bad_trials_removed_exp2)
  # ),
  # tar_target(
  #   name = within_maze_plotted_exp2,
  #   command = plot_within_maze(within_maze_marked_data_exp2)
  # ),
  # tar_target(
  #   name = data_long_exp2,
  #   command = build_data_long_exp2(within_maze_marked_data_exp2, codes_conditions_exp2)
  # ),
  # tar_target(
  #   name = data_long_calculated_exp2,
  #   command = data_long_calculations(data_long_exp2, maps)
  # ), 
  # tar_target(
  #   name = LME_analysis_results_exp2,
  #   command = LME_analysis_exp2(data_long_calculated_exp2)
  # ),
  
  ################
  ### Combined ###
  ################
  
  # tar_target(
  #   name = combined_analysis_results,
  #   command = combined_analysis(strategic_data_appended, data_long_calculated_exp2)
  # ),
  # tar_target(
  #   name = standard_figures,
  #   command = create_standard_figures(combined_analysis_results, codes_conditions, codes_participant_conditions, codes_participant_conditions_exp2)
  # )

  ###############
  ### Explore ###
  ###############
  tar_target(
    name = explore_data,
    command = explore_data_long(data_long_calculated)
  ), 

  tar_target(
    name = all_trials_velocity_plots,
    command = plot_all_trials_velocity(possible_fighting_flagged_data, maps)
  ),

  ################
  ### Workload ###
  ################
  tar_target(
    name = workload_data,
    command = pull_workload(codes_participant_conditions, codes_conditions)
  ),
  tar_target(
    name = workload_ANOVA,
    command = run_ANOVA_workload(workload_data)
  ), 
  tar_target(
    name = workload_data_explored,
    command = explore_workload_data(workload_data)
  ),

  ##############
  ### 3-back ###
  ##############
  tar_target(
    name = three_back_data,
    command = import_three_back()
  ),
  tar_target(
    name = summary_three_back_data,
    command = format_three_back(three_back_data)
  ),
  tar_target(
    name = three_back_data_with_codes,
    command = append_codes_to_three_back(summary_three_back_data, codes_participant_conditions, codes_conditions)
  ), 
  tar_target(
    name = three_back_data_explored,
    command = explore_three_back_data(three_back_data_with_codes)
  ),
  tar_target(
    name = three_back_data_with_composites,
    command = calculate_three_back_composites(three_back_data_with_codes)
  ),
  tar_target(
    name = three_back_data_composites_explored,
    command = explore_three_back_composites(three_back_data_with_composites)
  ),

  ############
  ### Ryan ###
  ############

  tar_target(
    name = save_data_for_Ryan,
    command = save_data_for_Ryan(workload_data, three_back_data_with_composites)
  )
  
)

# tar_manifest()
# tar_visnetwork()
# tar_make()

