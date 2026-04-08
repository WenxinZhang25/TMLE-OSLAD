########################################################
# Simulation Script for Running Different Designs
########################################################

## Simulation for Scenarios 1 and 2
# treat_func_type=("default")
# for round in {1..20}; do
#    sbatch \
#      --export=t_end=50,batch_size=50,total_runs=500,n_rounds=20,round=$round,treat_func_type="${treat_func_type}",design="all",DGD_name="DGD_hetero_d1" \
#      code/simulation.sh &
#    sleep 1
# done

## Simulation for ADAPT-R
# t_end=19
# total_runs=500
# n_rounds=25
# treat_func_type=default
# DGD_name="DGD_realSim"
# for design in TMLE RCT Y1 Y2 Y3 Y4 Y5 naive; do
#   for round in $(seq 1 $n_rounds); do
#     sbatch \
#       --export=t_end=$t_end,batch_size=NA,total_runs=$total_runs,n_rounds=$n_rounds,round=$round,treat_func_type=$treat_func_type,design=$design,DGD_name=$DGD_name \
#       code/simulation.sh
#     sleep 0.5
#   done
# done

## Sensitivity analyses
### Different treatment functions
# treat_types=(logistics sharp_inv_S mild_inv_S)
# for round in {1..25}; do
#   for treat in "${treat_types[@]}"; do
#     sbatch --export=t_end=50,batch_size=50,total_runs=500,n_rounds=25,round=$round,treat_func_type="$treat",design="TMLE",DGD_name="DGD_hetero_d1" code/simulation.sh &
#   done
# done

### Different confidence interval levels for CATE
# for round in {1..25}; do
#   for conf_level in 0.9 0.99; do
#     sbatch --export=t_end=50,batch_size=50,total_runs=500,n_rounds=25,round=$round,treat_func_type="default",design="TMLE",DGD_name="DGD_hetero_d1",conf_level="$conf_level" code/simulation.sh &
#   done
# done

### Higher dimension
# for round in {1..25}; do
#    sbatch --export=t_end=50,batch_size=50,total_runs=500,n_rounds=25,round=$round,treat_func_type="default",design="TMLE",DGD_name="DGD_hetero_d3_scenario1" code/simulation.sh &
# done
# for round in {1..25}; do
#    sbatch --export=t_end=50,batch_size=50,total_runs=500,n_rounds=25,round=$round,treat_func_type="default",design="TMLE",DGD_name="DGD_hetero_d3_scenario2" code/simulation.sh &
# done

mkdir -p code/logs

Rscript code/run_sims_all.R $t_end $batch_size $total_runs $n_rounds $round $treat_func_type $design $DGD_name $conf_level > code/logs/Simulation_t_end_${t_end}_batch_size_${batch_size}_n_runs_${total_runs}_round_${round}_treat_func_${treat_func_type}_conf_level_${conf_level}.Rout
