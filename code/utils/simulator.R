# Module for setting up simulation configurations and generating data
adaptSim <- R6::R6Class(
  "adaptSim",
  inherit = t3s_Simulation,
  public = list(
    data_new = NULL,
    data_full = NULL,
    pA_candidates = NULL,
    selected_nodes = NULL,
    lepski_outcome = NULL,
    initialize = function(params = NULL, ..., sim_fun) {
      if (is.null(params)) {
        params <- list()
      }
      params$sim_fun <- sim_fun
      private$.sim_fun <- sim_fun
      super$initialize(params = params, ...)
    },
    sample = function() {
      sim_fun_args <- list()
      sim_fun_args$rndseed <- self$params$rngseeds[self$step]
      sim_fun_args$ti <- self$step
      sim_fun_args$batch_size <- self$params$E_t[sim_fun_args$ti]
      sim_fun_args$simulation <- self
      if (self$step == 1){
        set.seed(self$seed)
      }
      sim_fun_args$seed <- self$seed
      data_new <- do.call(self$sim_fun, sim_fun_args)
      if (self$step > 1){
        data_new$ID <- data_new$ID + max(self$data_full$ID)
      }
      self$data_new <- data_new
    }
  ),
  active = list(
    sim_fun = function() {
      return(private$.sim_fun)
    },
    t = function(){
      return(self$step)
    },
    pA_all = function(){
      return(self$data_full$pA)
    }
  ),
  private = list(.sim_fun = NULL)
)

sim_from_fun <- function(sim_fun, ...) {
  return(t3s_Simulation_Functional$new(..., sim_fun = sim_fun))
}
