as_py_datetime <- function(x) {
    UseMethod("as_py_datetime")
}

as_py_datetime.default <- function(x) {
    stop("Unknown date type. Use Date or POSIXct only.")
}

as_py_datetime.datetime.date <- function(x) {
    x
}

as_py_datetime.Date <- function(x) {

    number_date <- as.numeric(x) * 86400 * 1e9
    pandas$Timestamp(number_date, tz = "UTC")

}

as_py_datetime.POSIXct <- function(x) {

    tz <- as.POSIXlt(x)$zone
    number_date <- as.numeric(x) * 1e9

    tryCatch({
        pandas$Timestamp(number_date, tz = tz)
    },
    error = function(e) {
        msg <- paste0("R time zone, ", tz, ", missing or does not exist in pytz. Defaulting to UTC.")
        warning(msg, call. = FALSE)
        pandas$Timestamp(number_date, tz = "UTC")
    })

}
