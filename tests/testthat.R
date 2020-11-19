library(testthat)

library(tidymodels)
library(modeltime.gluonts)
library(timetk)
library(reticulate)
library(tidyverse)
library(rlang)

# gluonts   <- import("gluonts", convert = FALSE)
# np        <- import("numpy", convert = FALSE)
# pd        <- import("pandas", convert = FALSE)

skip_if_no_gluonts <- function() {
    gluonts_available <- reticulate::py_module_available("gluonts")
    if (!gluonts_available) {
        skip("gluonts not available for testing")
    }
}


test_check("modeltime.gluonts")
