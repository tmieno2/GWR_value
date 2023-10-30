# /*===========================================================
#' # GWR analysis
# /*===========================================================
#! Run GWR and do economic analysis

estimate_GWR <- function(reg_data_sp, N_levels, price_ls) {

  
  ## ====================
  ## Transformed GWR
  ## ====================
  
  #=== transferred regression functional form ===#
  reg_formula <- formula(yield_tr ~ N)
  
  #=== initial data transfer ===#
  reg_data_sp$yield_tr =  reg_data_sp$yield - mean(reg_data_sp$b2) * reg_data_sp$N2
  
  #=== search for optimal bandwidth ===#
  # obw <- bw.gwr(
  #   reg_formula,
  #   data=reg_data_sp,
  #   approach="AICc",
  #   kernel= kernel_choice,
  #   adaptive=T
  # )
  obw=obw_choice

  #=== loop over transfer coefficients ===#
  b2_hat_ls <- seq(min(reg_data_sp$b2), max(reg_data_sp$b2), by=0.05)
  R2_ls <- c()
  for(i in 1:length(b2_hat_ls)){
    
    #=== data transfer ===#
    reg_data_sp$yield_tr =  reg_data_sp$yield - b2_hat_ls[i] * reg_data_sp$N2

    #=== gwr estimation (use the same obw) ===#
    gwr_est <-
      gwr.basic(
        reg_formula,
        data = reg_data_sp,
        bw = obw,
        kernel = kernel_choice,
        adaptive = T
      )
    
    #=== goodness of fit measure ===#
    R2_ls[i] <- 
      summary(lm(reg_data_sp$yield_tr ~ gwr_est$SDF$yhat))$r.squared
  }
  
  #=== pick the b2_hat by max R2
  b2_hat <- R2_ls %>% unlist() %>% which.max() %>% b2_hat_ls[.]

  #----------------------
  # final GWR estimation
  #----------------------
  {
    #=== data transfer ===#
    reg_data_sp$yield_tr =  reg_data_sp$yield - b2_hat * reg_data_sp$N2
    
    #=== gwr estimation (use the same obw) ===#
    gwr_est <-
      gwr.basic(
        reg_formula,
        data = reg_data_sp,
        bw = obw,
        kernel = kernel_choice,
        adaptive = T
      )
  }

  #----------------------------
  # estimated GWR coefficients
  #----------------------------
  gwr_beta <- 
    data.table(
      aunit_id = reg_data_sp$aunit_id,
      b0_hat = gwr_est$SDF$Intercept,
      b1_hat = gwr_est$SDF$N,
      b2_hat = b2_hat
    ) %>% 
    #===duplicate data for different price scenarios===#
    expand_grid_df(., price_ls)

  #=== GWR optimal N rates ===#
  gwr_beta <- 
    gwr_beta %>%
    # === concave responses ===#
    .[b2_hat < 0 & b1_hat > 0, opt_N_gwr := (b1_hat - pN / pCorn) / (-2 * b2_hat)] %>%
    # === convex responses: corner solution ===#
    .[
      b2_hat >= 0 | b1_hat <= 0,
      yield_left := gen_yield_QD(b0_hat, b1_hat, b2_hat, min(N_levels))
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      pi_left := pCorn * yield_left - pN * min(N_levels)
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      yield_right := gen_yield_QD(b0_hat, b1_hat, b2_hat, max(N_levels))
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      pi_right := pCorn * yield_right - pN * max(N_levels)
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      opt_N_gwr :=
        as.numeric(pi_left > pi_right) * min(N_levels) +
        as.numeric(pi_left <= pi_right) * max(N_levels)
    ] %>%
    # === limit the range of opt_N_gwr ===#
    .[, opt_N_gwr := pmin(opt_N_gwr, max(N_levels))] %>%
    .[, opt_N_gwr := pmax(opt_N_gwr, min(N_levels))] %>%
    # === keep columns ===#
    .[, .(aunit_id, pCorn, pN, b0_hat, b1_hat, b2_hat, opt_N_gwr)]

  gwr_beta_tr <- copy(gwr_beta) %>% 
    .[, model := "GWR-T"]
  
  

  ## ====================
  ## Regular GWR
  ## ====================
  
  #=== quadratic regression functional form ===#
  reg_formula <- formula(yield ~ N + N2)
  
  #=== search for optimal bandwidth ===#
  # obw <- bw.gwr(
  #   reg_formula,
  #   data=reg_data_sp,
  #   approach="AICc",
  #   kernel=kernel_choice,
  #   adaptive=T
  # )
  obw=obw_choice

  #=== gwr estimation (use the chosen obw) ===#
  gwr_est <-
    gwr.basic(
      reg_formula,
      data = reg_data_sp,
      bw = obw,
      kernel = kernel_choice,
      adaptive = T
    )
  
  #----------------------------
  # estimated GWR coefficients
  #----------------------------
  gwr_beta <- 
    data.table(
      aunit_id = reg_data_sp$aunit_id,
      b0_hat = gwr_est$SDF$Intercept,
      b1_hat = gwr_est$SDF$N,
      b2_hat = gwr_est$SDF$N2
    ) %>% 
    #===duplicate data for different price scenarios===#
    expand_grid_df(., price_ls)
    
  
  #=== GWR optimal N rates ===#
  gwr_beta <- 
    gwr_beta %>%
    # === concave responses ===#
    .[b2_hat < 0 & b1_hat > 0, opt_N_gwr := (b1_hat - pN / pCorn) / (-2 * b2_hat)] %>%
    # === convex responses: corner solution ===#
    .[
      b2_hat >= 0 | b1_hat <= 0,
      yield_left := gen_yield_QD(b0_hat, b1_hat, b2_hat, min(N_levels))
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      pi_left := pCorn * yield_left - pN * min(N_levels)
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      yield_right := gen_yield_QD(b0_hat, b1_hat, b2_hat, max(N_levels))
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      pi_right := pCorn * yield_right - pN * max(N_levels)
    ] %>%
    .[
      b2_hat >= 0 | b1_hat <= 0,
      opt_N_gwr :=
        as.numeric(pi_left > pi_right) * min(N_levels) +
        as.numeric(pi_left <= pi_right) * max(N_levels)
    ] %>%
    # === limit the range of opt_N_gwr ===#
    .[, opt_N_gwr := pmin(opt_N_gwr, max(N_levels))] %>%
    .[, opt_N_gwr := pmax(opt_N_gwr, min(N_levels))] %>%
    # === keep columns ===#
    .[, .(aunit_id, pCorn, pN, b0_hat, b1_hat, b2_hat, opt_N_gwr)] 

  gwr_beta_org <- copy(gwr_beta) %>% 
    .[, model := "GWR-R"]

  
  
  ## ====================
  ## Return data
  ## ====================
  gwr_beta <- rbind(gwr_beta_tr, gwr_beta_org) 
    # dcast(., pCorn + pN + aunit_id ~ model, 
    #       value.var = c("b0_hat", "b1_hat", "b2_hat", "opt_N_gwr"))
  
  return(gwr_beta)

}