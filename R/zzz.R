
# PACKAGE IMPORTS ----

#' @import modeltime
#' @importFrom reticulate py


# ONLOAD UTILITIES ----

msg_no_gluonts <- function() {
    cli::cli_h1("Python Dependency Check {.pkg modeltime.gluonts}")
    cli::cli_alert_danger('GluonTS Python Dependencies Not Found')
    cli::cli_alert_info("Available Options: ")
    cli::cli_ol(id = "gluonts_installation_options")
    cli::cli_li("{.strong [Option 1 - Use a Pre-Configured Environment]:} Use {.code install_gluonts()} to install GluonTS Python Dependencies into a conda environment named {.field r-gluonts}.")
    cli::cli_li("{.strong [Option 2 - Use a Custom Environment]:} Before running {.code library(modeltime.gluonts)}, use {.code Sys.setenv(GLUONTS_PYTHON = 'path/to/python')} to set the path of your python executable that is located in an environment that has 'gluonts', 'mxnet', 'numpy', 'pandas', and 'pathlib' available as dependencies.")
    cli::cli_end("gluonts_installation_options")
    cli::cli_text("Refer to 'Managing GluonTS Environments' {.url https://business-science.github.io/modeltime.gluonts/articles/managing-envs.html} for more details.")
    cli::cli_h1("End Python Dependency Check")
}

msg_error <- function(e) {
    cli::cli_h1("Error Loading Python Dependencies {.pkg modeltime.gluonts}")
    cli::cli_alert_danger("Python Dependency LoadError")
    cli::cli_text(e)
    cli::cli_h1("End Python Package Load Check")
}




# PACKAGE ENVIRONMENT SETUP ----

pkg.env            <- new.env()
pkg.env$env_name   <- "r-gluonts"
pkg.env$activated  <- FALSE

# PYTHON DEPENDENCIES ----
# Move Python Imports to Package Environment
# - CRAN comment: Cannot use <<- to modify Global env
pkg.env$gluonts    <- NULL
pkg.env$pathlib    <- NULL
pkg.env$pd         <- NULL
pkg.env$np         <- NULL


# ONLOAD ----

.onLoad <- function(libname, pkgname) {

    # ATTEMPT TO CONNECT TO A GLUONTS PYTHON ENV ----
    activate_gluonts()

    # ATTEMPT TO LOAD PYTHON LIBRARIES FROM GLUONTS ENV ----
    dependecies_ok <- check_gluonts_dependencies()
    if (dependecies_ok) {
        tryCatch({
            pkg.env$gluonts <- reticulate::import("gluonts", delay_load = TRUE, convert = FALSE)
            pkg.env$pathlib <- reticulate::import("pathlib", delay_load = TRUE, convert = FALSE)
            pkg.env$np      <- reticulate::import("numpy", delay_load = TRUE, convert = FALSE)
            pkg.env$pd      <- reticulate::import("pandas", delay_load = TRUE, convert = FALSE)
        }, error = function(e) {
            NULL
            dependecies_ok <- FALSE
            if (interactive()) msg_error(e)
        })
    }

    # LET USER KNOW IF DEPENDENCIES ARE NOT OK ----
    if (!dependecies_ok) {
        if (interactive()) msg_no_gluonts()
    }

    # LOAD MODELS ----
    make_deep_ar()
    make_nbeats()


}




