
#' Convert R Date or POSIXt to Pandas Timestamp
#'
#' @param x A Date or Date Time
#' @param ... Additional parameters passed to Pandas Timestamp
#' @param pass_time_zone Whether or not to include the time zone in the
#' conversion to Pandas. GluonTS does not work with Pandas
#' Time Zones. Default: FALSE.
#'
#' @examples
#' \donttest{
#' dt <- as.Date("2011-01-01")
#' as_pandas_timestamp(dt)
#'
#' dt_time <- as.POSIXct("2011-01-01 12:43:01", tz = "GMT")
#' as_pandas_timestamp(dt_time, pass_time_zone = TRUE)
#' }
#'
#' @export
as_pandas_timestamp <- function(x, ..., pass_time_zone = FALSE) {
    UseMethod("as_pandas_timestamp")
}

#' @export
as_pandas_timestamp.default <- function(x, ..., pass_time_zone = FALSE) {
    stop("Unknown date type. Use Date or POSIXct only.")
}

#' @export
as_pandas_timestamp.datetime.date <- function(x, ..., pass_time_zone = FALSE) {
    x
}

#' @export
as_pandas_timestamp.Date <- function(x, ..., pass_time_zone = FALSE) {

    number_date <- as.numeric(x) * 86400 * 1e9
    if (pass_time_zone) {
        pkg.env$pd$Timestamp(number_date, tz = "UTC", ...)
    } else {
        pkg.env$pd$Timestamp(number_date, ...)
    }

}

#' @export
as_pandas_timestamp.POSIXct <- function(x, ..., pass_time_zone = FALSE) {

    number_date <- as.numeric(x) * 1e9

    if (pass_time_zone) {
        tz <- as.POSIXlt(x)$zone
        tryCatch({
            pkg.env$pd$Timestamp(number_date, tz = tz, ...)
        },
        error = function(e) {
            msg <- paste0("R time zone, ", tz, ", missing or does not exist in pytz. Defaulting to UTC.")
            warning(msg, call. = FALSE)
            pkg.env$pd$Timestamp(number_date, tz = "UTC", ...)
        })
    } else {
        pkg.env$pd$Timestamp(number_date, ...)
    }

}
