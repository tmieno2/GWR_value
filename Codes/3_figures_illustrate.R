

#'  Illustrate and play around with the graphs of simulation results.
#'  The final figures are knitted by the 3_make_figures_publication

## ==============================================================
##                          Preparation                        =
## ==============================================================

rm(list = ls())

# === Packages ===#
library(sp)
library(spdep)
library(spatialreg)
library(sf)
library(raster)
library(data.table)
library(tidyverse)
library(dplyr)
library(magrittr)
library(gstat)
library(GWmodel)
library(scam)
library(mgcv)
library(magic)
library(stringr)
library(ggplot2)
library(tictoc)
library(here)
options(stringsAsFactors = FALSE)

# === Set working directory ===#
setwd(here())

# =================================#
# choose the kernel used for GWR
# choices: "gaussian", "exponential", "boxcar", "bisquare", "tricube"
kernel_choice = "tricube"
# =================================#

# load estimation results
results <-
    here("Shared", "Results", kernel_choice, "pi_data.rds") %>%
    readRDS() %>%
    .[, type := ifelse(transfer == 0, "GWRR", "GWRT")] %>%
    .[, bias := pi_diff_est - pi_diff] %>%
    # only display the 25% (5.44), 50% (6.56), 75% (7.67) price ratio
    .[pRatio %in% c(5.44, 6.56, 7.67), ] %>%
    # === label price ratio
    .[, pLabelName := "Price Ratio (N/corn)"] %>%
    .[pRatio == 5.44, pLabel := "Low"] %>%
    .[pRatio == 6.56, pLabel := "Middle"] %>%
    .[pRatio == 7.67, pLabel := "High"] %>%
    .[, pLabel := factor(pLabel, levels = c("Low", "Middle", "High"))]

# /*===========================================================
#' # The value of GWR-based VRA over SCAM-based URA for GWR-R and GWR-T
# /*===========================================================
# fig.id = "pi-dif-dist",
# fig.cap = "The value of GWR-based VRA over SCAM-based URA for GWR-R and GWR-T"

mean_data_value <-
    results %>%
    .[, .(pi_diff = median(pi_diff)),
      by = c("field_col", "pLabel", "type")
    ] %>%
    print()
ggplot(data = results) +
    geom_histogram(
        aes(x = pi_diff),
        fill = NA,
        color = "blue",
        bins = 50,
        size = 0.2
    ) +
    geom_vline(
        data = mean_data_value,
        aes(xintercept = pi_diff),
        color = "red",
        size = 0.5
    ) +
    geom_text(
        data = mean_data_value, color = "red",
        aes(
            x = ifelse(pi_diff < 65, pi_diff + 4, pi_diff - 20), y = 100,
            label = paste0("Median = ", round(pi_diff, 2))
        ),
        angle = 0, hjust = -0.1, vjust = 0, size = 3
    ) +
    facet_grid(pLabel ~ type) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = 20 * (-2:3), limits = c(-45, 60)) +
    xlab("The value of VRA over URA ($ per ha)") +
    ylab("Number of Simulation Cases") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA)
    )
ggsave(here("Shared", "Figures", paste0("g_value_", kernel_choice, ".png")),
       height = 4, width = 5)

# /*===========================================================
#' # Bias in the estimation of the value of GWR-based VRA over SCAM-based URA
# /*===========================================================
# fig.id = "bias-est-pi",
# fig.cap = "Bias in the estimation of the value of GWR-based VRA over SCAM-based URA for GWR-R and GWR-T"

# mean_data_bias[type == "GWR-T" & pLabel == 10.35, bias] %>% hist

median_bias_data <-
    results %>%
    .[, bias := pi_diff_est - pi_diff] %>%
    .[, .(bias = median(bias)),
      by = c("field_col", "pLabel", "type")
    ] %>%
    print()
results %>%
    .[, bias := pi_diff_est - pi_diff] %>%
    # .[bias < 150, ] %>%
    ggplot(data = .) +
    geom_histogram(
        aes(x = bias),
        fill = NA,
        color = "blue",
        size = 0.2,
        bins = 50
    ) +
    geom_vline(
        data = median_bias_data, aes(xintercept = bias),
        color = "red",
        size = 0.5
    ) +
    geom_text(
        data = median_bias_data, color = "red",
        aes(
            x = ifelse(bias < 65, bias + 2, bias - 85), y = 100,
            label = paste0("Median = ", round(bias, 2))
        ),
        angle = 0, hjust = -0.1, vjust = 0, size = 3
    ) +
    facet_grid(pLabel ~ type) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(breaks = 30 * (-1:8), limits = c(-20, 160)) +
    xlab("Bias in the Estimation of the Value of VRA over URA ($ per ha)") +
    ylab("Number of Simulation Cases") +
    theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA)
    )
ggsave(here("Shared", "Figures", paste0("g_bias_", kernel_choice, ".png")),
       height = 4, width = 5)


# /*===========================================================
#' # Shapes of kernel functions
# /*===========================================================

# === kernel data set ===#
kernel_data <- CJ(
    name = c("Gaussian", "Exponential", "Box-car", "Bi-square", "Tri-cube"),
    x = seq(-1.5, 1.5, by = 0.01)
    ) %>% 
    .[, name := factor(name, levels = c("Gaussian", "Exponential", "Box-car", 
                                       "Bi-square", "Tri-cube"))] %>% 
    .[name=="Gaussian", kernel := exp(-0.5*x^2)] %>% 
    .[name=="Exponential", kernel := exp(-abs(x))] %>% 
    .[name=="Box-car", kernel := as.numeric(abs(x)<1)] %>% 
    .[name=="Bi-square", kernel := as.numeric(abs(x)<1) * (1 - x^2)^2] %>% 
    .[name=="Tri-cube", kernel := as.numeric(abs(x)<1) * (1 - abs(x)^3)^3]
ggplot(data = kernel_data) +
    geom_line(aes(x = x, y = kernel)) +
    facet_wrap(.~name, ncol = 1, scales = "free") +
    theme_bw() +
    scale_y_continuous(breaks = c(0, 0.5, 1), limits = c(0, 1.1)) +
    theme(
        panel.grid = element_blank(),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text = element_text(colour="black")
    )
ggsave(here("Shared", "Figures", "kernel_functions.png"),
       height = 5, width = 2, units = "in", dpi = 300)



################################################################################
################################################################################
################################################################################

# /*===========================================================
#' # EONR estimation tendency
# /*===========================================================

# ggplot(est_data) +
# geom_histogram(aes(x = opt_N_scam)) +
# facet_grid(round(pN, digits = 2) ~ .)

est_data <-
    readRDS(here("Shared", "Results", kernel_choice, "est_data.rds")) %>%
    unnest() %>%
    data.table() %>%
    .[, type := ifelse(transfer == 0, "GWR-R", "GWR-T")] 

eorn_ratio_data <-
    est_data %>%
    .[, pRatio := pN / pCorn] %>%
    # only display the 25% (5.44), 50% (6.56), 75% (7.67) price ratio
    .[pRatio %in% c(5.44, 6.56, 7.67), ] %>%
    # === label price ratio
    .[, pLabelName := "Price Ratio (N/corn)"] %>%
    .[pRatio == 5.44, pLabel := "Low"] %>%
    .[pRatio == 6.56, pLabel := "Middle"] %>%
    .[pRatio == 7.67, pLabel := "High"] %>%
    .[, pLabel := factor(pLabel, levels = c("Low", "Middle", "High"))] %>% 
    .[, .(eonr_ratio = mean(opt_N_gwr / opt_N_scam)), by = .(type, sim, pLabel)]

ggplot(data = eorn_ratio_data) +
    geom_histogram(
        aes(x = eonr_ratio),
        color = "blue",
        fill = NA,
        bins = 50,
        size = 0.2
    ) +
    geom_vline(xintercept = 1, color = "red") +
    facet_grid(pLabel ~ type) +
    xlab("Number of Simulation Cases") +
    xlab("Average Ratio of Estimated EONR to true EONR")
ggsave(here("Shared", "Figures", paste0("g_eonr_bias_", kernel_choice, ".png")),
       height=6, width=6)

# /*===========================================================
#' # Comparison of Estimated and True Coefficients
# /*===========================================================
# fig.id = "true-vs-estimated-coef-gwr-r",
# fig.cap = "Comparison of Estimated and True Coefficients"

#* read the simulation data for a sigle simulationused for illustration
single_sim <-
    here("Shared", "Results", "aunit_sim_single.rds") %>%
    readRDS() %>%
    .[, type := ifelse(transfer == 0, "GWR-R", "GWR-T")]

plot_data <-
    single_sim %>%
    .[type == "GWR-R", ] %>%
    .[, .(aunit_id, b1, b2, b1_hat, b2_hat)] %>%
    melt(id.var = "aunit_id") %>%
    .[, type := ifelse(str_detect(variable, "hat"), "Estimated", "True")] %>%
    .[, variable := gsub("_hat", "", variable)] %>%
    dcast(aunit_id + variable ~ type, value.var = "value")

g_b1 <-
    plot_data[variable == "b1", ] %>%
    ggplot(data = .) +
    geom_point(aes(y = Estimated, x = True), size = 0.3) +
    xlim(0, NA) +
    ylim(0, NA)

g_b2 <-
    plot_data[variable == "b2", ] %>%
    ggplot(data = .) +
    geom_point(aes(y = Estimated, x = True), size = 0.3) +
    geom_hline(yintercept = 0, color = "red") +
    xlim(NA, 0.1) +
    ylim(NA, 0.1)

g_comp_coef <- g_b1 / g_b2

# /*===========================================================
#' # Comparison of Estimated and True EONR
# /*===========================================================
# fig.id = "true-vs-estimated-optn-gwr-r",
# fig.cap = "Comparison of Estimated and True EONR"

# est_data[sim == 1 & pn_pc == 10.35, ] %>%
#   .[, type := ifelse(transfer == 0, "GWR-R", "GWR-T")] %>%
#   ggplot(data = .) +
#     geom_histogram(aes(x = opt_N_gwr)) +
#     facet_grid(. ~ type)

g_comp_eonr <-
    single_sim %>%
    ggplot(data = .) +
    geom_point(aes(y = opt_N_gwr, x = opt_N), size = 0.3) +
    geom_abline(slope = 1, color = "red") +
    xlab("True Optimal Nitrogen Rate (kg/ha)") +
    ylab("Estimated Optimal Nitrogen Rate (kg/ha)") +
    facet_grid(. ~ type) +
    coord_equal()



# /*===========================================================
#' # The cause of significant over-estimation of the value of GWR-based VRA
# /*===========================================================
# fig.id = "why-bias-many",
# fig.cap = "The cause of significant over-estimation of the value of GWR-based VRA"

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare parameters
# /*+++++++++++++++++++++++++++++++++++
field_parameters <- readRDS(here("Shared/Data/field_parameters.rds"))

pCorn <- field_parameters$pCorn
pN <- pCorn * field_parameters$pRatio_ls[[1]][2] # medium price scenario

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare yield response curves
# /*+++++++++++++++++++++++++++++++++++
il_data_oe <-
    readRDS(here("Shared/Results/il_data_oe.rds")) %>%
    .[,
      lapply(.SD, mean),
      by = aunit_id,
      .SDcols = c(
          "b0", "b1", "b2", "Nk",
          "b0_hat", "b1_hat", "b2_hat",
          "opt_N", "opt_N_gwr", "opt_N_scam"
      )
    ]

n_data <-
    data.table(
        N = seq(
            min(il_data_oe$opt_N_gwr) - 50,
            max(il_data_oe$opt_N_gwr, il_data_oe$opt_N_scam),
            length = 30
        )
    )

set.seed(710527)
aunit_id_ls <- sample(il_data_oe$aunit_id, 50)

gwr_curv_data <-
    expand_grid_df(il_data_oe, n_data) %>%
    .[, yield := (b0_hat + b1_hat * N + b2_hat * N^2) / 1000] %>%
    .[, profit := pCorn * yield * 1000 - pN * N] %>%
    .[aunit_id %in% aunit_id_ls, ]

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare point data
# /*+++++++++++++++++++++++++++++++++++

point_data <-
    il_data_oe %>%
    .[, y_hat_gwr := (b0_hat + b1_hat * opt_N_gwr + b2_hat * opt_N_gwr^2) / 1000] %>%
    .[, y_hat_scam := (b0_hat + b1_hat * opt_N_scam + b2_hat * opt_N_scam^2) / 1000] %>%
    .[aunit_id %in% aunit_id_ls, ] %>%
    .[, .(aunit_id, y_hat_gwr, y_hat_scam, opt_N_gwr, opt_N_scam)] %>%
    melt(id.var = "aunit_id") %>%
    .[, var_type := fifelse(str_detect(variable, "y_hat"), "yield", "N")] %>%
    .[, type := fifelse(str_detect(variable, "gwr"), "GWR", "SCAM")] %>%
    .[, variable := NULL] %>%
    dcast(aunit_id + type ~ var_type, value.var = "value") %>%
    .[, profit := pCorn * yield * 1000 - pN * N]

# /*+++++++++++++++++++++++++++++++++++
#' # Figure
# /*+++++++++++++++++++++++++++++++++++
g_why_bias_many <-
    ggplot() +
    geom_line(
        data = gwr_curv_data,
        aes(y = yield, x = N, group = aunit_id),
        size = 0.3,
        color = "grey"
    ) +
    geom_point(
        data = point_data,
        aes(y = yield, x = N, color = type),
        size = 0.6
    ) +
    ylab("Estimated Yield (ton/ha)") +
    xlab("Nitrogen Rate (kg/ha)") +
    theme(
        legend.position = "bottom"
    ) +
    scale_color_discrete(name = "Estiamted EONR")

# /*===========================================================
#' # An illustration of over-estimation of the value of GWR-based VRA over SCAM-based URA
# /*===========================================================
# fig.id = "why-bias-single",
# fig.cap = "An illustration of over-estimation of the value of GWR-based VRA over SCAM-based URA",
# fig.dim = c(6, 7)

# /*+++++++++++++++++++++++++++++++++++
#' # Preapare yeld response curves
# /*+++++++++++++++++++++++++++++++++++
true_curv_data <-
    il_data_oe %>%
    .[aunit_id %in% aunit_id_ls[1], ] %>%
    expand_grid_df(., n_data) %>%
    .[, y_true_curve_raw := (b0 + b1 * N + b2 * N^2) / 1000] %>%
    .[, y_true_curve_pl := (b0 + b1 * Nk + b2 * Nk^2) / 1000] %>%
    .[, yield := fifelse(N < Nk, y_true_curve_raw, y_true_curve_pl)] %>%
    .[, profit := pCorn * yield * 1000 - pN * N] %>%
    .[, .(yield, profit, N)] %>%
    .[, type := "True"]

gwr_curv_data_f <-
    gwr_curv_data[aunit_id %in% aunit_id_ls[1], ] %>%
    .[, .(yield, profit, N)] %>%
    .[, type := "GWR-estimated"]

curv_data <-
    rbind(true_curv_data, gwr_curv_data_f) %>%
    .[, type := factor(type, levels = c("True", "GWR-estimated"))]

# /*+++++++++++++++++++++++++++++++++++
#' # Prepare yield points data
# /*+++++++++++++++++++++++++++++++++++
true_yield_data <-
    il_data_oe %>%
    .[aunit_id %in% aunit_id_ls[1], ] %>%
    .[, .(b0, b1, b2, Nk, opt_N_gwr, opt_N_scam, opt_N)] %>%
    melt(id.vars = c("b0", "b1", "b2", "Nk")) %>%
    setnames("value", "N") %>%
    .[, y_true_raw := (b0 + b1 * N + b2 * N^2) / 1000] %>%
    .[, y_true_pl := (b0 + b1 * Nk + b2 * Nk^2) / 1000] %>%
    .[, yield := fifelse(N < Nk, y_true_raw, y_true_pl)] %>%
    .[, profit := pCorn * yield * 1000 - pN * N] %>%
    .[, type := fcase(
        variable == "opt_N_gwr", "GWR",
        variable == "opt_N_scam", "SCAM",
        variable == "opt_N", "True"
    )] %>%
    .[, .(yield, profit, N, type)]

point_data_gwr <-
    point_data[aunit_id %in% aunit_id_ls[1], ]

# /*+++++++++++++++++++++++++++++++++++
#' # Figure
# /*+++++++++++++++++++++++++++++++++++
g_yield <-
    ggplot() +
    geom_line(data = curv_data, aes(y = yield, x = N, linetype = type)) +
    geom_point(
        data = true_yield_data,
        aes(y = yield, x = N, color = type, shape = type)
    ) +
    geom_point(
        data = point_data_gwr,
        aes(y = yield, x = N, color = type, shape = type)
    ) +
    ylab("Yield (ton/ha)") +
    xlab("Nitrogen Rate (kg/ha)") +
    scale_color_discrete(name = "EONR") +
    scale_shape_discrete(name = "EONR") +
    scale_linetype_discrete(name = "Yield Response Fnctions") +
    ggtitle("(a) Estimated and True Yields")

g_profit <-
    ggplot() +
    geom_line(data = curv_data, aes(y = profit / 1000, x = N, linetype = type)) +
    geom_point(
        data = true_yield_data,
        aes(y = profit / 1000, x = N, color = type, shape = type)
    ) +
    geom_point(
        data = point_data_gwr,
        aes(y = profit / 1000, x = N, color = type, shape = type)
    ) +
    ylab("Partial Profit ($1000/ha)") +
    xlab("Nitrogen Rate (kg/ha)") +
    scale_color_discrete(name = "EONR") +
    scale_shape_discrete(name = "EONR") +
    scale_linetype_discrete(name = "Profit Response Fnctions") +
    ggtitle("(b) Estimated and True Profits")

g_why_bias_single <- g_yield / g_profit
