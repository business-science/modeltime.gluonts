#' General Tuning Parameters for GluonTS Trainers (All Models)
#'
#' These parameters are part of the `gluonts.mx.trainer.Trainer()` function.
#'
#'
#' @inheritParams dials::learn_rate
#' @param values A character string of possible values.
#'
#' @details
#' The main parameters for GluonTS Model Training are:
#'
#' - `epochs`: The number of iterations of training. See `dials::epochs()`.
#' - `num_batches_per_epoch`: Number of batches at each epoch.
#' - `learn_rate`: The rate at which learning is changed. See `dials::learn_rate()`.
#' - `learn_rate_decay_factor`: Factor by which to decrease the learning rate.
#' - `penalty`: The weight decay (or L2 regularization) coefficient. See `dials::penalty()`.
#'
#' Additional useful parameters that have been added to Modeltime are:
#'
#' - `lookback_length`: Number of time units that condition the predictions.
#' - `scale_values`: Scales numeric data by `id' group using mean = 0, standard deviation = 1 transformation.
#'
#' @name trainer_params

#' @export
#' @rdname trainer_params
lookback_length <- function(range = c(dials::unknown(), dials::unknown()), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(lookback_length = "Number of time units that condition the predictions"),
        finalize  = NULL
    )
}

#' @export
#' @rdname trainer_params
num_batches_per_epoch <- function(range = c(2, 100), trans = NULL) {
    dials::new_quant_param(
        type      = "integer",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(num_batches_per_epoch = "Number of Batches at Each Epoch"),
        finalize  = NULL
    )
}

#' @export
#' @rdname trainer_params
learn_rate_decay_factor <- function(range = c(0.0, 1.0), trans = NULL) {
    dials::new_quant_param(
        type      = "double",
        range     = range,
        inclusive = c(TRUE, TRUE),
        trans     = trans,
        label     = c(learn_rate_decay_factor = "Learning Rate Decay Factor"),
        finalize  = NULL
    )
}

#' @export
#' @rdname trainer_params
scale_values <- function(values = c(TRUE, FALSE)) {
    dials::new_qual_param(
        type     = c("logical"),
        values   = values,
        default  = FALSE,
        label    = c(scale = "Scale Numeric Data"),
        finalize = NULL
    )
}
