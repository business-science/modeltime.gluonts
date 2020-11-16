


#' Saving and Loading GluonTS Models
#'
#' @param object A fitted model object
#' @param path A directory to store the GluonTS model files
#' @param overwrite Whether or not to allow overwriting a GluonTS model's directory. Default: FALSE.
#'
#' @export
save_gluonts_model <- function(object, path, overwrite = FALSE) {

    # Check Class
    is_acceptable_class <- c("workflow", "model_fit") %>%
        purrr::map_lgl(.f = function(cl) inherits(object, cl)) %>%
        any()
    if (!is_acceptable_class) {
        rlang::abort("'object' must be class 'workflow' or 'model_fit'.")
    }

    # Check Path
    path_extension <- fs::path_ext(path)
    if (path_extension != "") {
        msg <- glue::glue("'path' should be a directory only. Found extension: {path_extension}. No file extensions are permitted.")
        rlang::abort(msg)
    }

    # If directory exists, check if OK to overwrite
    if (!overwrite) {
        if (fs::dir_exists(path)) {
            msg <- glue::glue("A directory exists at path: {path}. Use 'overwrite = TRUE' to overwrite.")
            rlang::abort(msg)
        }
    }

    # If No Directory, create it
    if (!fs::dir_exists(path)) {
        fs::dir_create(path)
    }

    # SAVE PROCEDURE

    # 1. Save the modeltime model
    rds_path <- fs::path(path, "modeltime_model.rds")
    saveRDS(object, file = rds_path)

    # 2. Save (Serialize) the GluonTS model
    pathlib_path <- pathlib$Path(path)

    if (inherits(object, "workflow")) {
        # Is workflow
        object$fit$fit$fit$models$model_1$serialize(path = pathlib_path)
    } else {
        # Is parsnip model_fit
        object$fit$models$model_1$serialize(path = pathlib_path)
    }

    msg <- glue::glue("\n\nModel saved at path: {path}")
    message(msg)
}

#' @rdname save_gluonts_model
#' @export
load_gluonts_model <- function(path) {

    # Check Path
    path_extension <- fs::path_ext(path)
    if (path_extension != "") {
        msg <- glue::glue("'path' should be a directory only. Found extension: {path_extension}. No file extensions are permitted.")
        rlang::abort(msg)
    }

    # 1. Load the modeltime model
    rds_path <- fs::path(path, "modeltime_model.rds")
    object   <- readRDS(file = rds_path)

    # 2. Load (Deserialize) the GluonTS model
    pathlib_path <- pathlib$Path(path)
    model_gluon  <- gluonts$model$predictor$Predictor$deserialize(path = pathlib_path)

    # 3. Recombine the modeltime model and the gluon model
    if (inherits(object, "workflow")) {
        # Is workflow
        object$fit$fit$fit$models$model_1 <- model_gluon
    } else {
        # Is parsnip model_fit class
        object$fit$models$model_1 <- model_gluon
    }

    return(object)
}
