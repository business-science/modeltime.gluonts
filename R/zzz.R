
# PACKAGE IMPORTS ----

#' @import modeltime
#' @importFrom reticulate py

# PACKAGE ENVIRONMENT SETUP ----

detect_conda_env <- function() {

    ret <- NULL
    tryCatch({
        ret <- reticulate::conda_list() %>%
            dplyr::filter(stringr::str_detect(python, pkg.env$env_name))
    }, error = function(e) {
        ret <- NULL
    })

    return(ret)

}

pkg.env            <- new.env()
pkg.env$env_name   <- "r-gluonts"
pkg.env$activated  <- FALSE
pkg.env$conda_envs <- detect_conda_env()
# Move Python Imports to Package Environment
# - CRAN comment: Cannot use <<- to modify Global env
pkg.env$gluonts    <- NULL
pkg.env$pathlib    <- NULL
pkg.env$pd         <- NULL
pkg.env$np         <- NULL


# ONLOAD ----

.onLoad <- function(libname, pkgname) {

    activate_gluonts()

    if (pkg.env$activated && check_python_dependencies()) {

        # LOAD PYTHON LIBRARIES ----
        pkg.env$gluonts <- reticulate::import("gluonts", delay_load = TRUE, convert = FALSE)
        pkg.env$pathlib <- reticulate::import("pathlib", delay_load = TRUE, convert = FALSE)
        pkg.env$np      <- reticulate::import("numpy", delay_load = TRUE, convert = FALSE)
        pkg.env$pd      <- reticulate::import("pandas", delay_load = TRUE, convert = FALSE)

        # LOAD MODELS ----

        make_deep_ar()
        make_nbeats()


    } else {

        msg_no_gluonts()

    }

}

# UTILITIES ----

msg_no_gluonts <- function() {
    packageStartupMessage(
        "* GluonTS Python Dependencies Not Found *\n",
        "  Available options:\n",
        "    - [Recommended] Use Pre-Configured Python Environment: Use `install_gluonts()` to\n",
        "       install GluonTS Python Libraries into a conda environment named 'r-gluonts'.\n",
        "    - Use a Custom Python Environment: Before running `library(modeltime.gluonts)`, \n",
        "       use `Sys.setenv(GLUONTS_PYTHON = 'path/to/python')` to set the path of your \n",
        "       python executable in an environment that has 'gluonts', 'mxnet', 'numpy', 'pandas', \n",
        "       and 'pathlib' available as dependencies."
    )
}

activate_gluonts <- function() {

    gluonts_python <- Sys.getenv("GLUONTS_PYTHON", unset = NA)
    if (!is.na(gluonts_python)) {
        Sys.setenv('RETICULATE_PYTHON' = gluonts_python)
        pkg.env$activated <- TRUE
    }
    # gluonts_python <- NA

    if (is.na(gluonts_python)) {

        conda_envs_found <- nrow(pkg.env$conda_envs)

        if (is.null(conda_envs_found) || conda_envs_found == 0 ) {

            # Could not activate environment
            # message("GluonTS Please use 'install_gluonts()' to install the core GluonTS library.")
            pkg.env$activated <- FALSE

        } else if (conda_envs_found == 1) {

            # Option 2A: "r-gluonts" environment found
            reticulate::use_condaenv(pkg.env$conda_envs$name, required = TRUE)
            pkg.env$activated <- TRUE

        } else if (conda_envs_found > 1) {

            # Option 2B: More than one "r-gluonts"-like environments found
            # packageStartupMessage("Multiple 'r-gluonts' python environments found.")
            # print(pkg.env$conda_envs)
            #
            # message("\nUsing: ")
            pkg.env$conda_envs <- pkg.env$conda_envs %>% dplyr::slice(1)
            # print(pkg.env$conda_envs)

            reticulate::use_condaenv(pkg.env$conda_envs$name, required = TRUE)
            pkg.env$activated <- TRUE

        }
    }



}

check_python_dependencies <- function() {
    all(
        reticulate::py_module_available("numpy"),
        reticulate::py_module_available("pandas"),
        reticulate::py_module_available("gluonts"),
        reticulate::py_module_available("mxnet"),
        reticulate::py_module_available("pathlib")
    )
}



