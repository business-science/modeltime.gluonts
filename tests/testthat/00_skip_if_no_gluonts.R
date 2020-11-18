skip_if_no_gluonts <- function() {
    gluonts_available <- reticulate::py_module_available("gluonts")
    if (!gluonts_available) {
        skip("gluonts not available for testing")
    }
}
