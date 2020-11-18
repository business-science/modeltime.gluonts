#' Install GluonTS
#'
#' Installs `GluonTS` Probabilisitic Deep Learning Time Series Forecasting Software
#' using `reticulate::py_install()`.
#' - A `Python` Environment will be created
#' named `r-gluonts`.
#' - The Modletime GluonTS R package will connect to the `r-gluonts` Python environment to use `GluonTS`
#'
#' @examples
#' \dontrun{
#' install_gluonts()
#' }
#'
#'
#' @export
install_gluonts <- function() {

    method <- "conda"

    default_pkgs <- c(
        "mxnet==1.6",
        "gluonts==0.5.2",
        "numpy==1.16.6",
        "pandas==1.0.5",
        "scikit-learn==0.23.2",
        "matplotlib==3.3.2",
        "seaborn==0.11.0",
        "pathlib==1.0.1"
    )

    reticulate::py_install(
        packages       = default_pkgs,
        envname        = "r-gluonts",
        method         = method,
        conda          = "auto",
        python_version = "3.6",
        pip            = TRUE
    )

}
