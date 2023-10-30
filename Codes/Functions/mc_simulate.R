# /*===========================================================
#' # MC simulations
# /*===========================================================
#! Run GWR and BRF and do economic analysis

mc_simulate <- function(data, pCorn, pRatio_ls, sim, N_levels) {

  print(sim)
  
  #* price scenario data table
  price_ls <- CJ(pCorn = pCorn, 
                 pRatio = pRatio_ls) %>% 
    .[, pN := pCorn * pRatio_ls ] 
  
  
  #/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### URA: SCAM estimate
  #/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #* representing correct response form specification
  scam_res <- gam(yield~s(N, k=5) + s(X, k = 6) + s(Y, k = 6) + ti(X, Y, k = 6),
                  data=data)
  
  data_scam <- data.table(
      N = seq(min(N_levels), max(N_levels), by = 1)
  ) %>%
      .[, X := data[1, ] %>% pull(X)] %>%
      .[, Y := data[1, ] %>% pull(Y)] %>%
      .[, yhat := predict(scam_res, newdata = .)] %>%
      # ===profits under different prices===#
      cbind(
          .[rep(1:nrow(.), times = nrow(price_ls)), ],
          price_ls[rep(1:nrow(price_ls), each = nrow(.)), ]
      ) %>%
      .[, pi_hat := pCorn * yhat - pN * N] %>%
      .[, .SD[which.max(pi_hat)], by = .(pCorn, pN)] %>%
      .[, opt_N_scam := N] %>%
      .[, c("pCorn", "pN", "opt_N_scam")]
  
  
  #/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### URA: Quadratic Regression
  #/*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*/
  #* response form misspecification
  qd_res <- lm(yield ~ N + N2, 
                  data=data)
  
  data_qd <- data.table(
      N = seq(min(N_levels), max(N_levels), by = 1)
  ) %>%
      .[, N2 := N^2 ] %>%
      .[, yhat := predict(qd_res, newdata = .)] %>%
      #===profits under different prices===#
      cbind(
          .[rep(1:nrow(.), times = nrow(price_ls)), ],
          price_ls[rep(1:nrow(price_ls), each = nrow(.)), ]
      ) %>%
      #===estimated optimal N===#
      .[, pi_hat := pCorn * yhat - pN * N] %>%
      .[, .SD[which.max(pi_hat)], by = .(pCorn, pN)] %>%
      .[, opt_N_qd := N] %>%
      .[, c("pCorn", "pN", "opt_N_qd")] %>% 
      #===estimated response coef===#
      .[, b0_hat := qd_res$coef["(Intercept)"]] %>% 
      .[, b1_hat := qd_res$coef["N"]] %>% 
      .[, b2_hat := qd_res$coef["N2"]] %>% 
      .[, model := "QD"]
  

  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### VRA: GWR estimate
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #* response form misspecification
  
  #=== regression data in sp ===#
  reg_data_sp <- 
    data %>%
    st_as_sf(coords = c("X", "Y")) %>%
    as("Spatial")
  
  
  #=== gwr estimation with different bandwidth ===#
  #! note: buffer zone data are dropped from analysis
  tic()
  data_gwr <-
    estimate_GWR(
      reg_data_sp = reg_data_sp,
      N_levels = N_levels,
      price_ls = price_ls
    )
  toc()

  
  
  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Results data
  #/*~~~~~~~~~~~~~~~~~~~~~~*/

  #=== expand QD results to aunit level ===#
  data_qd <- data_gwr[, .(aunit_id, pCorn, pN)] %>% 
      unique() %>%
      .[data_qd, on = c("pCorn", "pN")] %>% 
      #=== make the column name consistent for stacking, though it's not gwr
      setnames("opt_N_qd", "opt_N_gwr")

  #=== stack GWR and QD results together ===#
  data_stack <- rbind(data_gwr, data_qd)

  #=== merge SCAM rate as baseline===#
  est_beta <- data_stack %>%
      .[data_scam, on = c("pCorn", "pN")]

  return(est_beta)

}