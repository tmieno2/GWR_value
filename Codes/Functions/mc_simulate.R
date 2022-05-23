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
  
  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### SCAM
  #/*~~~~~~~~~~~~~~~~~~~~~~*/

  #=== scam regression ===#
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
  

  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### GWR estimate
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  
  #=== regression data in sp ===#
  reg_data_sp <- 
    data %>%
    st_as_sf(coords = c("X", "Y")) %>%
    as("Spatial")
  
  
  #=== gwr estimation with different bandwidth ===#
  #! note: buffer zone data are dropped from analysis
  tic()
  gwr_beta <-
    estimate_GWR(
      reg_data_sp = reg_data_sp,
      N_levels = N_levels,
      price_ls = price_ls
    )
  toc()

  
  
  
  #/*~~~~~~~~~~~~~~~~~~~~~~*/
  #' ### Results data
  #/*~~~~~~~~~~~~~~~~~~~~~~*/

  est_beta <- data_scam %>%
    .[gwr_beta, on = c("pCorn", "pN")] 

  return(est_beta)

}