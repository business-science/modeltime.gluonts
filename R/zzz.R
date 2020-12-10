
# PACKAGE IMPORTS ----

#' @import modeltime
#' @importFrom reticulate py


# ONLOAD UTILITIES ----

msg_no_gluonts <- function() {

    cli::cli_h1("Python Dependency Check {.pkg modeltime.gluonts}")
    cli::cli_alert_danger('GluonTS Python Dependencies Not Found')

    cli::cli_alert_info("Available Options: ")
    cli::cli_ol(id = "gluonts_installation_options")
    cli::cli_li("{.strong [Recommended]} {.emph Use the Pre-Configured {.field 'r-gluonts'} Environment.} Use {.code install_gluonts()} to install GluonTS Python Dependencies into a conda environment named {.field 'r-gluonts'}.")
    cli::cli_li("{.strong [Advanced]} {.emph Use a Custom Python Environment.} Before running {.code library(modeltime.gluonts)}, use {.code Sys.setenv(GLUONTS_PYTHON = 'path/to/python')} to set the path of your python executable that is located in an environment that has 'gluonts', 'mxnet', 'numpy', 'pandas', and 'pathlib' available as dependencies.")
    cli::cli_end("gluonts_installation_options")


    cli::cli_text("Refer to {.code ?install_gluonts} for more details.")

    cli::cli_h1("End Python Dependency Check")


}




# PACKAGE ENVIRONMENT SETUP ----

pkg.env            <- new.env()
pkg.env$env_name   <- "r-gluonts"
pkg.env$activated  <- FALSE
pkg.env$conda_envs <- detect_default_gluonts_env()

# PYTHON DEPENDENCIES ----
# Move Python Imports to Package Environment
# - CRAN comment: Cannot use <<- to modify Global env
pkg.env$gluonts    <- NULL
pkg.env$pathlib    <- NULL
pkg.env$pd         <- NULL
pkg.env$np         <- NULL


# ONLOAD ----

.onLoad <- function(libname, pkgname) {

    activate_gluonts()

    if (is_gluonts_activated() && check_gluonts_dependencies()) {

        # LOAD PYTHON LIBRARIES ----
        pkg.env$gluonts <- reticulate::import("gluonts", delay_load = TRUE, convert = FALSE)
        pkg.env$pathlib <- reticulate::import("pathlib", delay_load = TRUE, convert = FALSE)
        pkg.env$np      <- reticulate::import("numpy", delay_load = TRUE, convert = FALSE)
        pkg.env$pd      <- reticulate::import("pandas", delay_load = TRUE, convert = FALSE)


    } else {
        # if (interactive()) {
        #     msg_no_gluonts()
        # }
        msg_no_gluonts()
    }

    # LOAD MODELS ----

    make_deep_ar()
    make_nbeats()

}




