# WALMART EXAMPLES ----
# - Requires modeltime >= 0.6.1.9000

library(modeltime.gluonts)
library(tidymodels)
library(tidyverse)
library(timetk)

# SEED ----
# -Attempt to make reproducible
np <- reticulate::import("numpy", convert = FALSE)
mx <- reticulate::import("mxnet", convert = FALSE)

np$random$seed(123L)
mx$random$seed(123L)

# DATA ----

walmart_sales_weekly

splits <- walmart_sales_weekly %>%
    time_series_split(
        assess     = 26,
        cumulative = TRUE
    )

plot_walmart_model <- function(..., title = "Forecast Plot", show_ci = TRUE) {
    modeltime_table(
        ...
    ) %>%
        modeltime_calibrate(
            new_data = testing(splits),
            id       = "id"
        ) %>%
        modeltime_forecast(
            new_data      = testing(splits),
            actual_data   = walmart_sales_weekly,
            conf_interval = 0.95,
            conf_by_id    = TRUE
        ) %>%
        group_by(id) %>%
        plot_modeltime_forecast(
            .interactive = FALSE,
            .facet_ncol  = 2,
            .title       = title,
            .conf_interval_show = show_ci
        )
}

get_accuracy_table <- function(...) {
    modeltime_table(
        ...
    ) %>%
        modeltime_calibrate(
            new_data = testing(splits),
            id       = "id"
        ) %>%
        modeltime_accuracy(acc_by_id = TRUE) %>%
        group_by(id)
}


# * DeepAR ----
t0 <- Sys.time()
model_fit_deepar <- deep_ar(
    id                    = "id",
    freq                  = "W",
    prediction_length     = 26,
    lookback_length       = 52*2,
    epochs                = 10,
    scale                 = TRUE
) %>%
    set_engine("gluonts_deepar") %>%
    fit(Weekly_Sales ~ Date + id, training(splits))
t1 <- Sys.time()
diff_deepar <- t1-t0
diff_deepar

plot_walmart_model(model_fit_deepar, title = "DeepAR")

# * DeepAR (Torch) ----
t0 <- Sys.time()
model_fit_deepar_torch <- deep_ar(
    id                    = "id",
    freq                  = "W",
    prediction_length     = 26,
    lookback_length       = 52*2,
    epochs                = 10,
    scale                 = TRUE
) %>%
    set_engine("torch") %>%
    fit(Weekly_Sales ~ Date + id, training(splits))
t1 <- Sys.time()
diff_deepar_torch <- t1-t0
diff_deepar_torch

tibble(
    deepar_gluonts = diff_deepar,
    torch = diff_deepar_torch
)

plot_walmart_model(model_fit_deepar_torch, title = "DeepAR (Torch)")

plot_walmart_model(
    model_fit_deepar,
    model_fit_deepar_torch
)

get_accuracy_table(
    model_fit_deepar,
    model_fit_deepar_torch
) %>%
    arrange(id, .model_id)

# * DeepState ----
#   Notes:
#   - Reduced epochs to 2 to speed up

model_fit_deepstate <- deep_state(
    id                    = "id",
    freq                  = "W",
    prediction_length     = 26,
    lookback_length       = 52*2,
    add_trend             = FALSE,
    epochs                = 2,
    scale                 = TRUE
) %>%
    set_engine("gluonts_deepstate") %>%
    fit(Weekly_Sales ~ Date + id, training(splits))

plot_walmart_model(model_fit_deepstate, title = "DeepState")

# * N-BEATS ----
#   Notes:
#   - Reduced epochs to 5 to speed up

model_fit_nbeats <- nbeats(
    id                    = "id",
    freq                  = "W",
    prediction_length     = 26,
    lookback_length       = 52*2,

    # Decrease EPOCHS for speed
    epochs                = 5,

    scale                 = TRUE
) %>%
    set_engine("gluonts_nbeats") %>%
    fit(Weekly_Sales ~ Date + id, training(splits))

plot_walmart_model(model_fit_nbeats, title = "N-BEATS")


# * GP Forecaster -----

model_fit_gp <- gp_forecaster(
    id                    = "id",
    freq                  = "W",
    prediction_length     = 26,
    lookback_length       = 52*2,
    epochs                = 10,
    scale                 = TRUE
) %>%
    set_engine("gluonts_gp_forecaster", cardinality = 20) %>%
    fit(Weekly_Sales ~ Date + id, training(splits))

plot_walmart_model(model_fit_gp, title = "GP Forecaster")


# ACCURACY EVALUATION -----
accuracy_tbl <- get_accuracy_table(
    model_fit_deepar,
    model_fit_deepstate,
    model_fit_nbeats,
    model_fit_gp
)

accuracy_tbl %>% write_rds("examples/accuracy_results_walmart.rds")


accuracy_tbl %>%
    group_by(id) %>%
    slice_min(rmse)

# FORECAST VISUALIZATION -----

forecast_tbl <- modeltime_table(
    model_fit_deepar,
    model_fit_deepstate,
    model_fit_nbeats,
    model_fit_gp
) %>%
    modeltime_calibrate(
        new_data = testing(splits),
        id       = "id"
    ) %>%
    modeltime_forecast(
        new_data      = testing(splits),
        actual_data   = walmart_sales_weekly,
        conf_interval = 0.95,
        conf_by_id    = TRUE
    )

forecast_tbl %>% write_rds("examples/forecast_walmart.rds")

forecast_tbl %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol         = 2,
        .interactive        = FALSE,
        .conf_interval_show = FALSE,
        .title              = "Walmart | Forecast Results"
    )

# SUMMARY ----

read_rds("examples/accuracy_results_walmart.rds") %>%
    group_by(id) %>%
    slice_min(rmse)

read_rds("examples/forecast_walmart.rds") %>%
    group_by(id) %>%
    plot_modeltime_forecast(
        .facet_ncol         = 2,
        .interactive        = FALSE,
        .conf_interval_show = FALSE,
        .title              = "Walmart | Forecast Results"
    )
