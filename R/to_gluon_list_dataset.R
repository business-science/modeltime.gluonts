# DATA FRAME TO GLUON TS ----

#' Convert a data frame to a GluonTS ListDataset
#'
#' A ListDataset is the format required by `GluonTS`. This function
#' simplifies creating
#'
#' @param data A data.frame
#' @param date_var The date column (Timestamps)
#' @param value_var The value column (Target)
#' @param group_var The grouping column if Time Series Groups
#' @param freq the Pandas Timestamp Frequency.
#'
#' @examples
#' library(timetk)
#'
#' m4_daily %>%
#'     to_gluon_list_dataset(
#'         date_var  = date,
#'         value_var = value,
#'         group_var = id,
#'         freq      = "D"
#'     )
#'
#'
#' @export
to_gluon_list_dataset <- function(data, date_var, value_var, group_var = NULL, freq = "D") {

    data           <- dplyr::ungroup(data)

    date_var_expr  <- rlang::enquo(date_var)
    value_var_expr <- rlang::enquo(value_var)
    group_var_expr <- rlang::enquo(group_var)

    idx <- data %>% dplyr::pull(!! date_var_expr)
    if (length(idx) != length(unique(idx))) {
        if (rlang::quo_is_null(group_var_expr)) {
            rlang::abort("Overlapping time stamps detected, but no 'group_var'. A 'group_var' is required if the time series contains overlapping timestamps.")
        }
    }

    # Data is NOT grouped: Add Group
    if (rlang::quo_is_null(group_var_expr)) {
        data <- data %>%
            dplyr::mutate(.group_key = "_0")
        group_var_expr <- rlang::sym(".group_key")
    }

    # Create a GluonTS ListDataset
    ret <- data %>%

        # Split the groups
        dplyr::select(!! group_var_expr, !! date_var_expr, !! value_var_expr) %>%
        dplyr::mutate(!! group_var_expr := forcats::as_factor(as.character(!! group_var_expr))) %>%
        dplyr::group_by(!! group_var_expr) %>%
        dplyr::group_split() %>%

        # Convert to Dictionaries
        purrr::map(.f = function(df) {

            idx <- df %>% dplyr::pull(!! date_var_expr)
            val <- df %>% dplyr::pull(!! value_var_expr)
            grp <- df %>% dplyr::pull(!! group_var_expr)

            reticulate::dict(
                "start"   = as_pandas_datetime(idx[1], freq = freq),
                "target"  = np$array(val),
                "item_id" = as.character(grp[1])
            )
        }) %>%

        # Convert to ListDataset from a List of Dictionaries
        # Reference:
        # 1. https://ts.gluon.ai/examples/extended_forecasting_tutorial/extended_tutorial.html#1.2-Create-artificial-datasets
        gluonts$dataset$common$ListDataset(freq = freq)

    return(ret)

}
