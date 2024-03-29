#' GluonTS Environment Connection Utilities
#'
#' These functions are used for testing and establishing a python environment
#' connection with `modeltime.gluonts`.
#'
#' @details
#'
#' - `is_gluonts_activated()`: Determines if a GluonTS Environment has been activated
#'   during `library(modeltime.gluonts)`.
#'     - If `TRUE`, then you should be good to go.
#'     - If `FALSE`, then a connection between `modeltime.gluonts`
#'       and your GluonTS Python has _not_ been activated.
#'
#' - `activate_gluonts()`: Attempts to activate a connection between `modeltime.gluonts`
#'   and an associated GluonTS Python Environment using `reticulate::use_condaenv(required = TRUE)`.
#'     - It first looks for the system environment variable, 'GLUONTS_PYTHON', for a path to the python executable
#'     - It next looks for a Conda Environment named 'r-gluonts' (this is what most users will have)
#'
#' - `get_python_env()`: Returns the configuration for the python environment that is being discovered
#'   using `reticulate::py_discover_config()`.
#'
#' - `check_gluonts_dependencies()`: Checks whether GluonTS required python dependencies are present in the
#'   currently activated Python Environment.
#'
#' - `detect_default_gluonts_env()`: Detects if an 'r-gluonts' python environment is available.
#'     - Returns a `tibble` containing the
#'     - Returns `NULL` if an 'r-gluonts' environment is not detected
#'
#' @seealso
#' - [install_gluonts()] - Used to install the python environment needed to run `modeltime.gluonts`.
#'
#'
#' @examples
#' \donttest{
#' # Returns TRUE if GluonTS connection established on package load
#' is_gluonts_activated()
#'
#' #
#'
#' }
#'
#'
#'
#' @name gluonts-env

#' @export
#' @rdname gluonts-env
is_gluonts_activated <- function() {
    pkg.env$activated
}

#' @export
#' @rdname gluonts-env
activate_gluonts <- function() {

    # STEP 1 - CHECK FOR GLUONTS_PYTHON
    gluonts_python <- Sys.getenv("GLUONTS_PYTHON", unset = NA)
    custom_env_detected <- !is.na(gluonts_python)
    if (custom_env_detected) {

        # Sys.setenv('RETICULATE_PYTHON' = gluonts_python) # More forceful, generates warning and errors
        reticulate::use_python(python = gluonts_python, required = TRUE)
        pkg.env$activated <- TRUE

    }

    # STEP 2 - CHECK FOR DEFAULT r-gluonts ENV
    default_conda_env <- detect_default_gluonts_env()
    conda_envs_found  <- !is.null(default_conda_env)
    if (all(c(!pkg.env$activated, conda_envs_found))) {

        # Sys.setenv('RETICULATE_PYTHON' = default_conda_env$python[[1]])
        try({
            reticulate::use_python(python = default_conda_env$python[[1]], required = TRUE)
            pkg.env$activated <- TRUE
        }, silent = TRUE)

    }

}

#' @export
#' @rdname gluonts-env
get_python_env <- function() {
    reticulate::py_discover_config()
}

#' @export
#' @rdname gluonts-env
check_gluonts_dependencies <- function() {

    dependencies_ok <- FALSE
    try({
        dependencies_ok <- all(
            reticulate::py_module_available("numpy"),
            reticulate::py_module_available("pandas"),
            reticulate::py_module_available("gluonts"),
            reticulate::py_module_available("mxnet"),
            reticulate::py_module_available("pathlib")
        )
    }, silent = TRUE)

    return(dependencies_ok)
}

#' @export
#' @rdname gluonts-env
check_pytorch_dependencies <- function() {

    dependencies_ok <- FALSE
    try({
        dependencies_ok <- all(
            reticulate::py_module_available("torch"),
            reticulate::py_module_available("pytorch_lightning")
        )
    }, silent = TRUE)

    return(dependencies_ok)
}

#' @export
#' @rdname gluonts-env
detect_default_gluonts_env <- function() {

    ret <- NULL
    tryCatch({

        ret <- reticulate::conda_list() %>%
            tibble::as_tibble() %>%
            dplyr::filter(stringr::str_detect(python, pkg.env$env_name)) %>%
            dplyr::slice(1)

    }, error = function(e) {
        ret <- NULL
    })

    if (!is.null(ret)) {
        if (nrow(ret) == 0) {
            ret <- NULL
        }
    }

    return(ret)

}


# Check Version
check_gluonts_version <- function(min_required = "0.8.0") {

    gluonts <- reticulate::import("gluonts")

    installed_version_gluonts <- gluonts$`__version__`

    version_diff <- utils::compareVersion(installed_version_gluonts, min_required)

    if (version_diff < 0) {
        rlang::warn(
            stringr::str_glue("Detected gluonts version {installed_version_gluonts}. Upgrade required to gluonts {min_required}. Try running `install_gluonts()`.")
        )
    }
}
