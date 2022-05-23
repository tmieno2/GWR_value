




###Similar as the basic GWR
gwr_est <-
  gwr.basic(
    reg_formula,
    data = reg_data_sp,
    bw = obw,
    kernel = "gaussian",
    adaptive = T
  )

# Similar as the basic GWR
gwr_est <-
  gwr.multiscale(
    reg_formula,
    data = reg_data_sp,
    bws0 = c(20, 20, 20),
    bw.seled = c(T, T, T),
    criterion = "dCVR",
    kernel = "gaussian",
    adaptive = T,
    dMats=list(EUDM,EUDM,EUDM)
  )

#FBGWR
gwr_est <-
  gwr.multiscale(
    reg_formula,
    data = reg_data_sp,
    bws0 = c(20, 20, 20),
    criterion = "dCVR",
    kernel = "gaussian",
    adaptive = T,
    dMats=list(EUDM,EUDM,EUDM)
  )

#Mixed GWR
gwr_est <-
  gwr.multiscale(
    reg_formula,
    data = reg_data_sp,
    bws0 = c(Inf, 20, 20, Inf),
    criterion = "dCVR",
    kernel = "gaussian",
    dMats=list(EUDM,EUDM,EUDM)
  )

#PSDM GWR
gwr_est <-
  gwr.multiscale(
    reg_formula,
    data = reg_data_sp,
    kernel = "gaussian",
    p.vals=c(1,2,3)
  )


