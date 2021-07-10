# WALMART EXAMPLES ----

library(modeltime.gluonts)
library(tidymodels)
library(tidyverse)
library(timetk)

walmart_sales_weekly

splits <- walmart_sales_weekly %>%
    time_series_split(
        assess     = 26,
        cumulative = TRUE
    )

plot_walmart_model <- function(..., title = "Forecast Plot") {
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
            .title       = title
        )
}




# * DeepAR ----
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

plot_walmart_model(model_fit_deepar, title = "DeepAR")

# * N-BEATS ----

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
accuracy_tbl <- modeltime_table(
    model_fit_deepar,
    model_fit_nbeats,
    model_fit_gp
) %>%
    modeltime_calibrate(
        new_data = testing(splits),
        id       = "id"
    ) %>%
    modeltime_accuracy(acc_by_id = TRUE)

accuracy_tbl %>%
    group_by(id) %>%
    slice_min(rmse)

accuracy_tbl %>% write_rds("examples/accuracy_results_walmart.rds")