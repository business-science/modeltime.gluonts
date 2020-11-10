
#' @import modeltime

gluonts <- NULL

.onLoad <- function(libname, pkgname) {

    reticulate::configure_environment(pkgname)

    # Python Imports
    gluonts  <<- reticulate::import(
        module     = "gluonts",
        convert    = FALSE,
        delay_load = list(
            priority    = 1,
            environment = "r-gluonts"
        )
    )

    pandas   <<- reticulate::import("pandas", delay_load = TRUE, convert = FALSE)
    main     <<- reticulate::import("__main__", delay_load = TRUE, convert = TRUE)

    # This defines the model database

    # N-BEATS

}
