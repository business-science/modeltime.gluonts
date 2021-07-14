#' Tuning Parameters for NBEATS Models
#'
#' @inheritParams dials::learn_rate
#' @param values A character string of possible values.
#'
#' @details
#' The main parameters for NBEATS models are:
#'
#' - `num_stacks`: The number of stacks the network should contain.
#'
#' @name nbeats_params


#' @export
#' @rdname nbeats_params
num_stacks <- function(range = c(2, 100), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(num_stacks = "Number of Stacks"),
        finalize  = NULL
    )
}


