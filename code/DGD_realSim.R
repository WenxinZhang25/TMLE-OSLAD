# Data Generator for Adaptive Experiments using realSim reference data
#
# Arguments:
# - `ti`: current entry time
# - `batch_size`: (unused) placeholder to match interface in general simulation modules

# Returns: 
# - covariates and counterfactual outcomes for subjects enrolled at time `ti`

D2_realSim <- function(ti, batch_size = NULL, ...){
  df_ref <- read.csv("data/realSimData/realSim_data.csv")
  print(sprintf("data ti = %s \n", ti))
  if (ti <= max(df_ref$ti)){
    ti_for_return_data <- ti
    df <- df_ref[df_ref$ti == ti_for_return_data, setdiff(colnames(df_ref),c("X","ID"))]
  } else{
    # If current time ti is out of the real data range, return placeholder data (not used for final analysis)
    ti_for_return_data <- 1
    df <- df_ref[df_ref$ti == ti_for_return_data, setdiff(colnames(df_ref),c("X","ID"))]
    df$ti <- ti
  }
  df$ID <- 1:nrow(df) 
  return (data.table::as.data.table(df))
}

