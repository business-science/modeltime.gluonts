
<!-- README.md is generated from README.Rmd. Please edit that file -->

# modeltime.gluonts <img src='man/figures/logo-modeltime-gluonts.png' align="right" height="138" />

<!-- badges: start -->

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/modeltime.gluonts)](https://cran.r-project.org/package=modeltime)
![](http://cranlogs.r-pkg.org/badges/modeltime.gluonts?color=brightgreen)
![](http://cranlogs.r-pkg.org/badges/grand-total/modeltime.gluonts?color=brightgreen)
[![Codecov test
coverage](https://codecov.io/gh/business-science/modeltime.gluonts/branch/master/graph/badge.svg)](https://codecov.io/gh/business-science/modeltime.gluonts?branch=master)
[![R-CMD-check](https://github.com/business-science/modeltime.gluonts/workflows/R-CMD-check/badge.svg)](https://github.com/business-science/modeltime.gluonts/actions)
<!-- badges: end -->

> GluonTS Deep Learning in R.

## GluonTS Deep Learning in R

Modeltime GluonTS integrates the **Python GluonTS Deep Learning
Library**, making it easy to develop forecasts using Deep Learning for
those that are comfortable with the [Modeltime Forecasting
Workflow](https://business-science.github.io/modeltime/).

<img src="man/figures/m4_hourly_forecast.jpg" width="90%" style="display: block; margin: auto;" />

## Installation Requirements

**Important: This package is being maintained on GitHub (not CRAN).
Please install the GitHub version, which is updated with the latest
features:**

    # Install GitHub Version 
    remotes::install_github("business-science/modeltime.gluonts")

    # Install Python Dependencies
    modeltime.gluonts::install_gluonts()

For more detailed installation instructions and troubleshooting
guidance, visit our [Installation
Guide](https://business-science.github.io/modeltime.gluonts/articles/managing-envs.html).

## Make Your First DeepAR Model

Make your first `deep_ar()` model, which connects to the GluonTS
`DeepAREstimator()`. For a more detailed walkthough, visit our [Getting
Started
Guide](https://business-science.github.io/modeltime.gluonts/articles/getting-started.html).

``` r
library(modeltime.gluonts)
library(tidymodels)
library(tidyverse)

# Fit a GluonTS DeepAR Model
model_fit_deepar <- deep_ar(
    id                    = "id",
    freq                  = "M",
    prediction_length     = 24,
    lookback_length       = 48,
    epochs                = 5
) %>%
    set_engine("gluonts_deepar") %>%
    fit(value ~ ., training(m750_splits))

# Forecast with 95% Confidence Interval
modeltime_table(
    model_fit_deepar
) %>%
    modeltime_calibrate(new_data = testing(m750_splits)) %>%
    modeltime_forecast(
        new_data      = testing(m750_splits),
        actual_data   = m750,
        conf_interval = 0.95
    ) %>%
    plot_modeltime_forecast(.interactive = FALSE)
```

<img src="man/figures/deepar_example_1.png" width="90%" style="display: block; margin: auto;" />

## Meet the modeltime ecosystem

> Learn a growing ecosystem of forecasting packages

<div class="figure" style="text-align: center">

<img src="man/figures/modeltime_ecosystem.jpg" alt="The modeltime ecosystem is growing" width="100%" />
<p class="caption">
The modeltime ecosystem is growing
</p>

</div>

Modeltime is part of a **growing ecosystem** of Modeltime forecasting
packages.

-   [Modeltime (Machine
    Learning)](https://business-science.github.io/modeltime/)

-   [Modeltime H2O
    (AutoML)](https://business-science.github.io/modeltime.h2o/)

-   [Modeltime GluonTS (Deep
    Learning)](https://business-science.github.io/modeltime.gluonts/)

-   [Modeltime Ensemble (Blending
    Forecasts)](https://business-science.github.io/modeltime.ensemble/)

-   [Modeltime Resample
    (Backtesting)](https://business-science.github.io/modeltime.resample/)

-   [Timetk (Feature Engineering, Data Wrangling, Time Series
    Visualization)](https://business-science.github.io/timetk/)

## Take the High-Performance Forecasting Course

> Become the forecasting expert for your organization

<a href="https://university.business-science.io/p/ds4b-203-r-high-performance-time-series-forecasting/" target="_blank"><img src="https://www.filepicker.io/api/file/bKyqVAi5Qi64sS05QYLk" alt="High-Performance Time Series Forecasting Course" width="100%" style="box-shadow: 0 0 5px 2px rgba(0, 0, 0, .5);"/></a>

[*High-Performance Time Series
Course*](https://university.business-science.io/p/ds4b-203-r-high-performance-time-series-forecasting/)

### Time Series is Changing

Time series is changing. **Businesses now need 10,000+ time series
forecasts every day.** This is what I call a *High-Performance Time
Series Forecasting System (HPTSF)* - Accurate, Robust, and Scalable
Forecasting.

**High-Performance Forecasting Systems will save companies by improving
accuracy and scalability.** Imagine what will happen to your career if
you can provide your organization a “High-Performance Time Series
Forecasting System” (HPTSF System).

### How to Learn High-Performance Time Series Forecasting

I teach how to build a HPTFS System in my [**High-Performance Time
Series Forecasting
Course**](https://university.business-science.io/p/ds4b-203-r-high-performance-time-series-forecasting).
You will learn:

-   **Time Series Machine Learning** (cutting-edge) with `Modeltime` -
    30+ Models (Prophet, ARIMA, XGBoost, Random Forest, & many more)
-   **Deep Learning** with `GluonTS` (Competition Winners)
-   **Time Series Preprocessing**, Noise Reduction, & Anomaly Detection
-   **Feature engineering** using lagged variables & external regressors
-   **Hyperparameter Tuning**
-   **Time series cross-validation**
-   **Ensembling** Multiple Machine Learning & Univariate Modeling
    Techniques (Competition Winner)
-   **Scalable Forecasting** - Forecast 1000+ time series in parallel
-   and more.

<p class="text-center" style="font-size:24px;">
Become the Time Series Expert for your organization.
</p>
<br>
<p class="text-center" style="font-size:30px;">
<a href="https://university.business-science.io/p/ds4b-203-r-high-performance-time-series-forecasting">Take
the High-Performance Time Series Forecasting Course</a>
</p>
