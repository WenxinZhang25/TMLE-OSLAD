########################################################
# Simulation Script for Running CV-TMLE
########################################################


## Simulation for Scenarios 1 and 2
# treat_type=(default)
# dir_name="t_end_50_batch_size_50_n_runs_500"
# for round in {1..25}; do
#   sbatch \
#     --export="sim_name=DGD_hetero_d1,subfolder_name=${treat_type}/all,dir_name=${dir_name},round=${round}" \
#     code/cv_tmle_simulation.sh
# done

## Simulation for ADAPT-R
# dir_name="t_end_19_batch_size_NA_n_runs_500"
# for round in {1..25}; do
#   sbatch \
#     --export="sim_name=DGD_realSim,subfolder_name=default/TMLE,dir_name=${dir_name},round=${round}" \
#     code/cv_tmle_simulation.sh
# done

## Sensitivity analyses
### Different treatment functions
# treat_types=(sharp_inv_S mild_inv_S logistics)
# dir_name="t_end_50_batch_size_50_n_runs_500"
# for treat in "${treat_types[@]}"; do
#   for round in {1..25}; do
#     sbatch \
#       --export="dir_name=${dir_name},sim_name=DGD_hetero_d1,subfolder_name=${treat}/TMLE,round=${round}" \
#       code/cv_tmle_simulation.sh
#   done
# done

### Different confidence interval levels for CATE
# dir_name="t_end_50_batch_size_50_n_runs_500"
# for conf in 0.9 0.99; do
#   for round in {1..25}; do
#     sbatch \
#       --export="sim_name=DGD_hetero_d1-conf-${conf},subfolder_name=default/TMLE,dir_name=${dir_name},round=${round}" \
#       code/cv_tmle_simulation.sh
#   done
# done

### Higher dimension
# dir_name="t_end_50_batch_size_50_n_runs_500"
# d3_types=("scenario1" "scenario2")
# for d3_type in "${d3_types[@]}"; do
#   for round in {1..25}; do
#     sbatch \
#       --export="sim_name=DGD_hetero_d3_${d3_type},subfolder_name=default/TMLE,dir_name=${dir_name},round=${round}" \
#       code/cv_tmle_simulation.sh
#   done
# done

mkdir -p code/logs/cv_tmle

safe_subfolder_name=${subfolder_name//\//_}
Rscript code/OTR_analysis.R \
  "$sim_name" "$subfolder_name" "$dir_name" "$round" \
  > "code/logs/cv_tmle/${sim_name}-${safe_subfolder_name}-${dir_name}-round_${round}.Rout" 2>&1
