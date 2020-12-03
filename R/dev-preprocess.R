

# PREPROCESS HELPERS ----

# create_simple_scale_recipe <- function(data, prepare = TRUE, scale = TRUE) {
#
#     data_copy <- data
#
#     # Create recipe
#     recipe_spec <- recipes::recipe(~ ., data = data)
#
#     # Scale Numeric Columns
#     names_numeric <- data_copy %>%
#         dplyr::select_if(is.numeric)%>%
#         names()
#
#     if (length(names_numeric > 0)) {
#         recipe_spec <- recipe_spec %>%
#             recipes::step_normalize(recipes::all_numeric())
#     }
#
#     if (prepare) {
#         recipe_spec <- recipe_spec %>%
#             recipes::prep()
#     }
#
#     return(recipe_spec)
#
# }


transformer_scaler <- function(data, id, value) {

    history_tbl <- data %>%
        dplyr::group_by(!! enquo(id))

    scale_params_tbl <- history_tbl %>%
        dplyr::summarise(
            .mean = mean(!! enquo(value), na.rm = TRUE),
            .sd   = stats::sd(!! enquo(value), na.rm = TRUE)
        )

    history_trans_tbl <- history_tbl %>%
        dplyr::mutate(!! enquo(value) := timetk::standardize_vec(!! enquo(value), silent = TRUE)) %>%
        dplyr::ungroup()

    return(list(transformed = history_trans_tbl, params = scale_params_tbl))
}

inverter_scaler <- function(data, id, value, params) {

    data_inverted <- data %>%
        dplyr::group_by(!! rlang::enquo(id)) %>%
        tidyr::nest() %>%
        dplyr::left_join(params, by = rlang::quo_name(enquo(id))) %>%
        dplyr::mutate(.data_trans = purrr::pmap(.l = list(data, .mean, .sd),
                                                .f = function(df, m, s) {
            df %>%
                dplyr::mutate(!! rlang::enquo(value) := timetk::standardize_inv_vec(
                    x    = !! rlang::enquo(value),
                    mean = m,
                    sd   = s
                ))
        })) %>%
        dplyr::select(!! rlang::enquo(id), .data_trans) %>%
        tidyr::unnest(.data_trans) %>%
        dplyr::ungroup()

    return(data_inverted)

}
