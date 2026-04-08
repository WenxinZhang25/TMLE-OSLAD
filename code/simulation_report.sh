########################################################
# Simulation Report Script  
########################################################

## Simulation for Scenarios 1 and 2
# Rscript code/simulation_report.R "DGD_hetero_d1" "default" "t_end_50_batch_size_50_n_runs_500"

## Simulation for ADAPT-R
# Rscript code/simulation_report.R "DGD_realSim" "default" "t_end_19_batch_size_NA_n_runs_500"
 
## Sensitivity analysis
### Different treatment functions
# treat_types=(logistics sharp_inv_S mild_inv_S)
# for treat in "${treat_types[@]}"; do
#   Rscript code/simulation_report.R "DGD_hetero_d1" "${treat}" "t_end_50_batch_size_50_n_runs_500"
# done

### Different confidence interval levels for CATE
# Rscript code/simulation_report.R "DGD_hetero_d1-conf-0.9" "default" "t_end_50_batch_size_50_n_runs_500"
# Rscript code/simulation_report.R "DGD_hetero_d1-conf-0.99" "default" "t_end_50_batch_size_50_n_runs_500"
 
### Higher dimension
# Rscript code/simulation_report.R "DGD_hetero_d3_scenario1" "default" "t_end_50_batch_size_50_n_runs_500"
# Rscript code/simulation_report.R "DGD_hetero_d3_scenario2" "default" "t_end_50_batch_size_50_n_runs_500"
