# DATA FRAME TO GLUON TS ----

#' Convert a data frame to a GluonTS ListDataset
#'
#' A ListDataset is the format required by `GluonTS`. This function
#' simplifies creating a `GluonTS` ListDataset.
#'
#' @param data A data.frame
#' @param date_var The date column (Timestamps)
#' @param value_var The value column (Target)
#' @param id_var The Time Series ID column for tracking time series in GluonTS
#' @param freq the Pandas Timestamp Frequency.
#'
#' @examples
#' \donttest{
#' library(timetk)
#'
#' m4_daily %>%
#'     to_gluon_list_dataset(
#'         date_var  = date,
#'         value_var = value,
#'         id_var    = id,
#'         freq      = "D"
#'     )
#' }
#'
#'
#' @export
to_gluon_list_dataset <- function(data, date_var, value_var, id_var = NULL, freq = "D") {

    data           <- dplyr::ungroup(data)
    date_var_expr  <- rlang::enquo(date_var)
    value_var_expr <- rlang::enquo(value_var)
    id_var_expr    <- rlang::enquo(id_var)

    # Data has no ID, add ID
    if (rlang::quo_is_null(id_var_expr)) {
        message("Adding GluonTS ID Column:  item_id = '_0'")
        data <- data %>%
            dplyr::mutate(item_id = "_0")
        id_var_expr <- rlang::sym("item_id")
    }

    # Validate No Duplicated Timestamps By Group
    msg <- glue::glue(
        "Failed Validation:",
        "\n",
        "Overlapping time stamps detected."
    )
    validate_no_duplicated_timestamps_by_group(
        data,
        date_var       = !! date_var_expr,
        group_var      = !! id_var_expr,
        fail_msg       = msg
    )

    # Create a GluonTS ListDataset
    py_list_of_dict <- data %>%

        # TODO: CHANGE FOR MULTIVARIATE TIME SERIES
        dplyr::select(!! id_var_expr, !! date_var_expr, !! value_var_expr) %>%

        # Split the groups
        dplyr::mutate(!! id_var_expr := forcats::as_factor(as.character(!! id_var_expr))) %>%
        dplyr::group_by(!! id_var_expr) %>%
        dplyr::group_split() %>%

        # Convert to Dictionaries
        purrr::map(.f = function(df) {

            idx <- df %>% dplyr::pull(!! date_var_expr)
            val <- df %>% dplyr::pull(!! value_var_expr)
            grp <- df %>% dplyr::pull(!! id_var_expr)

            reticulate::dict(
                "start"   = as_pandas_timestamp(idx[1], freq = freq),
                "target"  = pkg.env$np$array(val),
                "item_id" = as.character(grp[1])
            )
        })

    # GLUONTS LISTDATASET CONVERSION
    # From a list of Dictionaries
    # Reference:
    # 1. https://ts.gluon.ai/examples/extended_forecasting_tutorial/extended_tutorial.html#1.2-Create-artificial-datasets
    pkg.env$gluonts$dataset$common$ListDataset(
        py_list_of_dict,
        freq = freq
    )

}
