predictCATE <- function(blip_fit, blip_predict_task, b_learner = "hal"){
  # Get nodes for covariates and outcomes
  nodes <- blip_fit$training_task$nodes

  # Point estimate of CATE
  cate_pred <- blip_fit$predict(blip_predict_task)

  # Estimate SE of CATE by delta method
  trainY <- blip_fit$training_task$data[[nodes$outcome]]
  trainX <- blip_fit$training_task$data[, nodes$covariates, with = FALSE]
  residuals_train <- trainY - blip_fit$predict()
  newX <- blip_predict_task$data[, nodes$covariates, with = FALSE]
  fit_object <- blip_fit$fit_object
  coefs <- fit_object$coefs[-1]
  selected_basis <- coefs != 0
  x_basis_train <- cbind(1, hal9001::make_design_matrix(as.matrix(trainX), fit_object$basis_list[selected_basis]))
  x_basis_pred <- cbind(1, hal9001::make_design_matrix(as.matrix(newX), fit_object$basis_list[selected_basis]))
  lambda <- 0.01
  EHW_bread <- solve(Matrix::crossprod(x_basis_train) + diag(lambda,ncol(x_basis_train)))
  EHW_meat <- Matrix::crossprod(x_basis_train, Matrix::crossprod(diag(residuals_train ** 2), x_basis_train))
  cov_beta_hat <- Matrix::crossprod(EHW_bread, Matrix::crossprod(EHW_meat, EHW_bread))
  cate_pred_cov <- as.matrix(x_basis_pred) %*% as.matrix(Matrix::tcrossprod(cov_beta_hat, x_basis_pred))
  cate_pred_se <- sqrt(diag(cate_pred_cov))
  return(list(fit = cate_pred,
              se = cate_pred_se,
              upper = cate_pred + 1.96 * cate_pred_se,
              lower = cate_pred - 1.96 * cate_pred_se))
}

treatByCATE <- function(fit, se.fit, conf_level = 0.95, Gmin = 0.1,
                        treat_func_type = "default"){
  if(Gmin >= 0.5 | Gmin < 0){
    stop("`Gmin` must be larger than 0 and smaller than 0.5.")
  }
  Gmax <- 1 - Gmin
  z <- qnorm(0.5 + conf_level/2, 0, 1)
  xi <- z * se.fit
  std_fit <- fit/xi
  if (treat_func_type == "default"){
    pA <- Gmin * (fit < -xi) + Gmax * (fit > xi) + ((-(0.5 - Gmin) / (2 * xi ^ 3)) * fit ^ 3 + ((0.5 - Gmin) / (2 * xi / 3)) * fit + 0.5) * (fit <= xi & fit >= - xi)
  } else if (treat_func_type == "mild_inv_S"){
    pA <- Gmin * (fit < -xi) + Gmax * (fit > xi) + ((+(0.5 - Gmin) / (2 * xi ^ 3)) * fit ^ 3 + ((0.5 - Gmin) / (2 * xi)) * fit + 0.5) * (fit <= xi & fit >= - xi)
  } else if (treat_func_type == "sharp_inv_S"){
    pA <- Gmin * (fit < -xi) + Gmax * (fit > xi) + (Gmin + (Gmax - Gmin)*(-1/4 * std_fit^5 + 3 / 4 * std_fit^3 + 1/2))* (fit <= xi & fit >= - xi)
  } else if(treat_func_type == "logistics"){
    pA <- Gmin * (fit < -xi) + Gmax * (fit > xi) + (Gmin + (Gmax - Gmin)*1/(1+exp(-8*std_fit)))* (fit <= xi & fit >= - xi)
  }
  else{
    stop("treat_func_type is not found!")
  }
  return (pA)
}


