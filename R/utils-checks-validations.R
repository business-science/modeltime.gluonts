
# CHECKS ----

check_no_duplicated_timestamps_by_group <- function(data, date_var, group_var = NULL) {

    date_var_expr  <- rlang::enquo(date_var)
    group_var_expr <- rlang::enquo(group_var)

    if (rlang::quo_is_missing(date_var_expr)) rlang::abort("'date_var' is missing with no default.")

    # Data is NOT grouped: Add Group
    if (rlang::quo_is_null(group_var_expr)) {
        data <- data %>%
            dplyr::mutate(item_id = "_0")
        group_var_expr <- rlang::sym("item_id")
    }

    # Perform group-wise check
    data %>%
        dplyr::group_by(!! group_var_expr) %>%
        tidyr::nest() %>%
        dplyr::mutate(fail_check = purrr::map_lgl(data, .f = function(df) {
            df %>%
                dplyr::pull(!! date_var_expr) %>%
                duplicated_timestamps()
        })) %>%
        dplyr::ungroup()


}

# VALIDATIONS ----
validate_gluonts_required_args <- function(data, prediction_length, freq, id) {

    # PREDICTION LENGTH CHECKS
    msg <- "GluonTS requires a numeric 'prediction_length'."
    if (rlang::is_missing(prediction_length) || is.null(prediction_length)) rlang::abort(msg)
    if (!is.numeric(prediction_length)) rlang::abort(msg)
    if (prediction_length <= 0) rlang::abort("'prediction_length' must be positive.")

    # FREQ CHECKS
    msg <- "GluonTS requires a 'freq' in the format of a Pandas Timestamp Frequency. Try using a Pandas frequency like '5min' for 5-minute or 'D' for Daily."
    if (rlang::is_missing(freq) || is.null(freq)) rlang::abort(msg)
    if (!is.character(freq)) rlang::abort(msg)

    # ID CHECKS
    msg <- "Missing: 'id' parameter. Make sure your dataset and model specification includes a column with unique IDs for each time series. Then indicate the ID column using 'id' parameter in the model specification."
    if (rlang::is_missing(id) || is.null(id)) rlang::abort(msg)
    msg <- glue::glue("Column not found: id = '{id}'. Make sure your dataset includes a column with unique IDs for each time series. Then indicate the ID column using 'id'.")
    if (!id %in% names(data)) rlang::abort(msg)

}

# validate_gluonts_trainer_args <- function(epochs, batch_size, num_batches_per_epoch) {
#
#     # TODO
# }

validate_no_duplicated_timestamps_by_group <- function(data, date_var, group_var = NULL,
                                                       fail_msg = "Overlapping time stamps detected.") {

    result_tbl <- data %>%
        check_no_duplicated_timestamps_by_group(
            date_var  = !! rlang::enquo(date_var),
            group_var = !! rlang::enquo(group_var)
        ) %>%
        dplyr::filter(fail_check)

    if (nrow(result_tbl) > 0) {
        rlang::abort(fail_msg)
    }



}

# UTILITIES ----

duplicated_timestamps <- function(idx) {
    length(idx) != length(unique(idx))
}






