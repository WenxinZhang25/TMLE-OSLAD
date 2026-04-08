# Module for generating reports along the experiment
adaptReporter <- R6::R6Class(
  "adaptReporter",
  inherit = t3s_Reporter,
  public = list(
    initialize = function(params = list()) {
      if (is.null(params$report_names)) {
        params$report_names <- TRUE
      }

      if (is.null(params$report_uuids)) {
        params$report_uuids <- TRUE
      }

      if(is.null(params$log)){
        params$log <- TRUE
      }

      if(params$log && is.null(params$log_path)){
        params$log_path <- "Logs"
      }

      if (is.null(params$path)) {
        params$path <- "Results"
      }
      if (is.null(params$save_images)) {
        params$save_images <- FALSE
      }
      if(params$save_images && is.null(params$image_path)){
        params$image_path <- "images"
      }

      private$.params <- params
      private$.reports <- list()
    },
    report = function(...) {
      cat("Start Reporting at Step",self$simulation$step,"\n")
      new_report <- self$simulation$last_estimate
      private$.reports <- c(private$.reports, list(new_report))
      cat("Finishing Reporting at Step",self$simulation$step,"\n")
    },
    make_final = function(...) {
      sp_dt <- list()
      if (self$params$report_names) {
        sp_dt$simulation_name <- self$simulation$name
        sp_dt$estimator_name <- self$simulation$estimator$name
      }

      if (self$params$report_uuids) {
        sp_dt$simulation_uuid <- self$simulation$uuid
        sp_dt$estimator_uuid <- self$simulation$estimator$uuid
      }

      sp_dt$runtime <- self$simulation$runtime
      sp_dt$seed <- self$simulation$seed
      sp_dt$t_end <- self$simulation$params$t_end
      sp_dt$final_outcome_tp <- self$simulation$params$Y_tp
      sp_dt$sd <- paste(self$simulation$params$sd, collapse = ",")
      sp_dt <- as.data.table(sp_dt)

      # Report regret
      data_full <- self$simulation$data_full
      node_list <- self$simulation$params$node_list
      col_pretreat <- c("ID", "ti", node_list$W)
      Y_node <- node_list$Y
      A_node <- node_list$A
      Y_tp <- self$simulation$params$node_list$Y_tp
      node_EY1 <- paste("EY1", Y_tp, sep = "_")
      node_EY0 <- paste("EY0", Y_tp, sep = "_")
      node_Y1 <- paste("Y1", Y_tp, sep = "_")
      node_Y0 <- paste("Y0", Y_tp, sep = "_")

      # Calcuklate true CATE, probability of selecting suboptimal arm and regret
      blip_EY <- data_full[[node_EY1]] - data_full[[node_EY0]]
      blip_Y <- data_full[[node_Y1]] - data_full[[node_Y0]]
      A_opt <- as.numeric(blip_EY > 0)
      hamming <- (data_full[[A_node]] != A_opt)
      regret <- hamming * abs(blip_EY)
      regret_table <- cbind(data_full[, col_pretreat, with = FALSE], regret, hamming)
      regret_table <- regret_table[, .(regret_sum = sum(regret),
                                       hamming_sum = sum(hamming),
                                       n = .N), by = ti]
      regret_table <- regret_table[,c("regret_avg", "regret_avg_c", "hamming_loss", "hamming_loss_c") :=
                                     list(regret_sum / n, cumsum(regret_sum) / cumsum(n), hamming_sum / n, cumsum(hamming_sum) / cumsum(n))]
      report <- regret_table[, c("ti", "n", "regret_avg", "regret_avg_c", "hamming_loss","hamming_loss_c"), with = FALSE]
      tmle_fit <- rbindlist(lapply(private$.reports, `[[`, "tmle_result"), fill=TRUE)
      blip_pred_results <- rbindlist(lapply(private$.reports, `[[`, "blip_pred_results"), fill=TRUE)
      names(self$simulation$selected_nodes) <- paste0("time_", seq_along(self$simulation$selected_nodes)) 

      private$.final_report <- list(report = data.frame(c(sp_dt, report)),
                                    data_full = data.frame(c(sp_dt, data_full)),
                                    pA_candidates = data.frame(c(sp_dt, self$simulation$pA_candidates)),
                                    selected_nodes = data.frame(c(sp_dt,self$simulation$selected_nodes)),
                                    tmle_fit = data.frame(c(sp_dt, tmle_fit)),
                                    blip_pred_results = data.frame(c(sp_dt, blip_pred_results)))
    }
  )
)
