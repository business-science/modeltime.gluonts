
#' Pandas Date Time Conversion Helpers
#'
#' @param x A Date or Date Time
#' @param ... Additional parameters passed to Pandas Timestamp()
#'
#' @export
as_pandas_datetime <- function(x, ...) {
    UseMethod("as_pandas_datetime")
}

#' @export
as_pandas_datetime.default <- function(x, ...) {
    stop("Unknown date type. Use Date or POSIXct only.")
}

#' @export
as_pandas_datetime.datetime.date <- function(x, ...) {
    x
}

#' @export
as_pandas_datetime.Date <- function(x, ...) {

    number_date <- as.numeric(x) * 86400 * 1e9
    pd$Timestamp(number_date, tz = "UTC", ...)

}

#' @export
as_pandas_datetime.POSIXct <- function(x, ...) {

    tz <- as.POSIXlt(x)$zone
    number_date <- as.numeric(x) * 1e9

    tryCatch({
        pd$Timestamp(number_date, tz = tz, ...)
    },
    error = function(e) {
        msg <- paste0("R time zone, ", tz, ", missing or does not exist in pytz. Defaulting to UTC.")
        warning(msg, call. = FALSE)
        pd$Timestamp(number_date, tz = "UTC", ...)
    })

}
