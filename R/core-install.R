#' Install GluonTS
#'
#' @description
#' `install_gluonts()`: Installs `GluonTS` Probabilisitic Deep Learning Time Series Forecasting Software
#' using `reticulate::py_install()`.
#'
#' - A `Python` Environment will be created named `r-gluonts`.
#' - When loaded with `library(modeltime.gluonts)`, the `modeltime.gluonts` R package
#'   will connect to the `r-gluonts` Python environment by default. See "Details" for
#'   connecting to custom python environments.
#' - If `fresh_install`, will remove any prior installations of the "r-gluonts" python environment
#' - If `include_pytorch`, will install additional dependencies needed for the optional
#'   pytorch backend that is available in some algorithms.
#'
#' `uninstall_gluonts()`: Will remove the "r-gluonts" python environment and python packages
#'
#' @param include_pytorch If `TRUE`, will install `torch`. Needed for Torch implementation
#'   of `deep_ar()`. Default: `FALSE`.
#' @param fresh_install If `TRUE`, will remove prior installations of the `r-glounts`
#'   conda environment to setup for a fresh installation. This can be useful if
#'   errors appear during upgrades. Default: `FALSE`.
#'
#' @details
#'
#' __Options for Connecting to Python__
#'
#' - __Recommended__ _Use Pre-Configured Python Environment:_ Use `install_gluonts()` to
#'    install GluonTS Python Libraries into a conda environment named 'r-gluonts'.
#' - __Advanced__ _Use a Custom Python Environment:_ Before running `library(modeltime.gluonts)`,
#'    use `Sys.setenv(GLUONTS_PYTHON = 'path/to/python')` to set the path of your
#'    python executable in an environment that has 'gluonts', 'mxnet', 'numpy', 'pandas',
#'    and 'pathlib' available as dependencies.
#'
#' __Package Manager Support (Python Environment)__
#'
#' - __Conda Environments:__ Currently, `install_gluonts()` supports Conda and Miniconda Environments.
#'
#' - __Virtual Environments:__ are not currently supported with the default installation method, `install_gluonts()`.
#'    However, you can connect to virtual environment that you have created using
#'     `Sys.setenv(GLUONTS_PYTHON = 'path/to/python')` prior to running `library(modeltime.ensemble)`.
#'
#' @examples
#' \dontrun{
#' install_gluonts()
#' }
#'
#'
#' @export
install_gluonts <- function(
    fresh_install = FALSE,
    include_pytorch = FALSE
) {

    # Check for Anaconda
    if (!check_conda()) {
        return()
    }

    # REMOVE PREVIOUS ENV
    if (fresh_install) {
        cli::cli_alert_info("Removing conda env `r-gluonts` to setup for fresh install...")
        reticulate::conda_remove("r-gluonts")
    }


    # PYTHON SPEC
    python_version <- "3.7.1"

    # GLUONTS INSTALLATION

    message("\n")
    cli::cli_alert_info("Installing gluonts dependencies...")
    message("\n")

    default_pkgs <- c(
        "mxnet~=1.7",
        "gluonts==0.8.0",
        "numpy",
        "pandas==1.0.5",
        "pathlib==1.0.1",
        "ujson==4.0.2",
        "brotlipy"
    )


    reticulate::py_install(
        packages       = default_pkgs,
        envname        = "r-gluonts",
        method         = "conda",
        conda          = "auto",
        python_version = python_version,
        pip            = TRUE
    )


    # TORCH INSTALLATION
    if (include_pytorch) {

        message("\n")
        cli::cli_alert_info("Installing torch dependencies...")
        message("\n")

        torch_pkgs <- c(
            "torch~=1.6",
            "pytorch-lightning~=1.1"
        )

        reticulate::py_install(
            packages       = torch_pkgs,
            envname        = "r-gluonts",
            method         = "conda",
            conda          = "auto",
            python_version = "3.7.1",
            pip            = TRUE
        )

    }

    # PROCESS CHECKS
    env_exists      <- !is.null(detect_default_gluonts_env())
    gluonts_failure <- FALSE

    message("\n")
    if (env_exists) {
        cli::cli_alert_success("The {.field r-gluonts} conda environment has been created.")
    } else {
        cli::cli_alert_danger("The {.field r-gluonts} conda environment could not be created.")
        gluonts_failure <- TRUE
    }

    # default_pkgs_exist <- check_gluonts_dependencies()
    # if (default_pkgs_exist) {
    #     cli::cli_alert_success("Installing gluonts dependencies... ...Success.")
    # } else {
    #     cli::cli_alert_danger("Installing gluonts dependencies... ...Failed. One or more of the following packages are not available: gluonts, mxnet, numpy, pandas, pathlib")
    #     gluonts_failure <- TRUE
    # }
    #
    # pytorch_failure <- FALSE
    # if (include_pytorch) {
    #     pytorch_exists     <- check_pytorch_dependencies()
    #
    #     if (pytorch_exists) {
    #         cli::cli_alert_success("Installing torch dependencies... ...Success.")
    #     } else {
    #         cli::cli_alert_danger("Installing torch dependencies... ...Failed. One or more of the following packages are not available: torch, pytorch_lightning")
    #         pytorch_failure <- TRUE
    #     }
    #
    #
    # }

    if (env_exists) {
        cli::cli_alert_info("Please restart your R Session and run {.code library(modeltime.gluonts)} to activate the {.field r-gluonts} environment.")
    } else {
        cli::cli_alert_info("For installation failure reports, please copy the python error message(s). Search your error(s) using Google. If none are found, create new issues here: https://github.com/business-science/modeltime.gluonts/issues")
    }

}


#' @export
#' @rdname install_gluonts
uninstall_gluonts <- function() {
    cli::cli_alert_info("Removing conda env `r-gluonts`...")
    reticulate::conda_remove("r-gluonts")
    message("\n")
    cli::cli_alert_success("The `r-gluonts` env has been removed.")
}


check_conda <- function() {

    conda_list_nrow <- nrow(reticulate::conda_list())

    if (is.null(conda_list_nrow) || conda_list_nrow == 0L) {
        # No conda
        message("Could not detect Conda or Miniconda Package Managers, one of which is required for 'install_gluonts()'. \nAvailable options:\n",
                " - [Preferred] You can install Miniconda (light-weight) using 'reticulate::install_miniconda()'. \n",
                " - Or, you can install the full Aniconda distribution (1000+ packages) using 'reticulate::conda_install()'. \n\n",
                "Then use 'install_gluonts()' to set up the GluonTS python environment.")
        conda_found <- FALSE
    } else {
        conda_found <- TRUE
    }

    return(conda_found)
}

