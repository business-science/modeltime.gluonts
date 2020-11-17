


# FIT -----

#' GluonTS DeepAR Modeling Function (Bridge)
#'
#' @inheritParams deepar_fit_impl
#' @param meta_context_length The different ‘context_length’ (aslso known as ‘lookback period’) to use for training the models. The ‘context_length’ is the number of time units that condition the predictions. Default and recommended value: (multiplier * prediction_length for multiplier in range(2, 7))
#' @param meta_loss_function The different ‘loss_function’ (also known as metric) to use for training the models. Unlike other models in GluonTS this network does not use a distribution. Default and recommended value: (“sMAPE”, “MASE”, “MAPE”)
#' @param meta_bagging_size The number of models that share the parameter combination of ‘context_length’ and ‘loss_function’. Each of these models gets a different initialization random initialization. Default and recommended value: 10
#' @param num_stacks The number of stacks the network should contain. Default and recommended value for generic mode: 30 Recommended value for interpretable mode: 2
#' @param num_blocks The number of blocks per stack. A list of ints of length 1 or ‘num_stacks’. Default and recommended value for generic mode: 1. Recommended value for interpretable mode: 3.
#' @param block_layers Number of fully connected layers with ReLu activation per block. A list of ints of length 1 or ‘num_stacks’. Default and recommended value for generic mode: (4) Recommended value for interpretable mode: (4)
#' @param widths Widths of the fully connected layers with ReLu activation in the blocks. A list of ints of length 1 or ‘num_stacks’. Default and recommended value for generic mode: (512) Recommended value for interpretable mode: (256, 2048)
#' @param sharing Whether the weights are shared with the other blocks per stack. A list of ints of length 1 or ‘num_stacks’. Default and recommended value for generic mode: (False) Recommended value for interpretable mode: (True)
#' @param expansion_coefficient_lengths If the type is “G” (generic), then the length of the expansion coefficient. If type is “T” (trend), then it corresponds to the degree of the polynomial. If the type is “S” (seasonal) then its not used. A list of ints of length 1 or ‘num_stacks’. Default value for generic mode: (32) Recommended value for interpretable mode: (3)
#' @param stack_types One of the following values: “G” (generic), “S” (seasonal) or “T” (trend). A list of strings of length 1 or ‘num_stacks’. Default and recommended value for generic mode: (“G”) Recommended value for interpretable mode: (“T”,”S”)
#'
#' @export
nbeats_ensemble_fit_impl <- function(x, y, freq, prediction_length, id,

                                     # Trainer Args
                                     epochs = 5,
                                     batch_size = 32,
                                     num_batches_per_epoch = 50,
                                     learning_rate = 0.001,
                                     learning_rate_decay_factor = 0.5,
                                     patience = 10,
                                     minimum_learning_rate = 5e-5,
                                     clip_gradient = 10,
                                     weight_decay = 1e-8,
                                     init = "xavier",
                                     ctx = NULL,
                                     hybridize = TRUE,

                                     # Algo Args
                                     meta_context_length = NULL,
                                     meta_loss_function = NULL,
                                     meta_bagging_size = 10,
                                     num_stacks = 30,
                                     num_blocks = list(1),
                                     # block_layers = 4,
                                     widths = list(512),
                                     sharing = list(FALSE),
                                     expansion_coefficient_lengths = list(32),
                                     stack_types = list("G")

) {

    # ARG CHECKS ----
    validate_gluonts_required_args(x, prediction_length, freq, id)

    # Convert args
    if (is.null(meta_context_length)) meta_context_length <- reticulate::py_none()
    if (is.null(meta_loss_function)) meta_loss_function <- reticulate::py_none()
    if (is.null(ctx)) ctx <- reticulate::py_none()


    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x


    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(predictor)
    # period    <- modeltime::parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # ID COLUMN
    id_tbl <- x %>% dplyr::select(dplyr::all_of(id))

    # VALUE COLUMN
    value_tbl <- tibble::tibble(value = y)

    # CONSTRUCT GLUONTS LISTDATASET
    # Resources:
    # 1. Univariate: https://ts.gluon.ai/examples/extended_forecasting_tutorial/extended_tutorial.html
    # 2. Multivariate: https://github.com/awslabs/gluon-ts/issues/494
    # 3. NBEATS: https://github.com/Mcompetitions/M5-methods/blob/master/Code%20of%20Winning%20Methods/A2/M5_NBEATS_TopLevel.py

    constructed_tbl <- dplyr::bind_cols(id_tbl, index_tbl, value_tbl)

    gluon_listdataset <- constructed_tbl %>%
        to_gluon_list_dataset(
            date_var  = !! rlang::sym(idx_col),
            value_var = value,
            id_var    = !! rlang::sym(id),
            freq      = freq
        )

    # Construct GluonTS Trainer
    trainer    <- gluonts$trainer$Trainer(
        ctx                        = ctx,
        epochs                     = epochs,
        batch_size                 = batch_size,
        num_batches_per_epoch      = num_batches_per_epoch,
        learning_rate              = learning_rate,
        learning_rate_decay_factor = learning_rate_decay_factor,
        patience                   = patience,
        minimum_learning_rate      = minimum_learning_rate,
        clip_gradient              = clip_gradient,
        weight_decay               = weight_decay,
        init                       = init,
        hybridize                  = hybridize
    )

    # Construct GluonTS Model
    model_spec <- gluonts$model$n_beats$NBEATSEnsembleEstimator(
        freq                          = freq,
        prediction_length             = prediction_length,

        trainer                       = trainer,

        meta_context_length           = meta_context_length,
        meta_loss_function            = meta_loss_function,
        meta_bagging_size             = meta_bagging_size,
        num_stacks                    = num_stacks,
        num_blocks                    = num_blocks,
        # block_layers                  = block_layers,
        widths                        = widths,
        sharing                       = sharing,
        expansion_coefficient_lengths = expansion_coefficient_lengths,
        stack_types                   = stack_types
    )

    # Train the model
    model_fit  <- model_spec$train(training_data = gluon_listdataset)

    # GET FITTED
    # TODO - Not sure if this is possible. Return fitted values as NA for now

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "nbeats_ensemble_fit_impl"

    # Models - Insert model_1 and model_2 into a list
    models <- list(
        model_1 = model_fit
    )

    # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    data <- index_tbl %>%
        dplyr::mutate(
            .actual    =  y,
            .fitted    =  NA,
            .residuals = .actual - .fitted
        )

    # Extras - Pass on transformation recipe
    extras <- list(
        id              = id,
        idx_column      = idx_col,
        value_column    = "value",
        freq            = freq,
        grps            = constructed_tbl %>% dplyr::pull(!! rlang::sym(id)) %>% unique(),
        constructed_tbl = list(constructed_tbl)
    )

    # Model Description - Gets printed to describe the high-level model structure
    desc <- "NBEATS ENSEMBLE"

    # Create new model
    modeltime::new_modeltime_bridge(
        class  = class,
        models = models,
        data   = data,
        extras = extras,
        desc   = desc
    )

}


#' @export
print.nbeats_ensemble_fit_impl <- function(x, ...) {
    cat(x$desc)
    cat("\n")
    cat("--------")
    cat("\n")
    print(x$models$model_1$prediction_net)
    invisible(x)
}

# PREDICT ----

#' Bridge prediction Function for DeepAR Models
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @export
nbeats_ensemble_predict_impl <- function(object, new_data) {

    # PREPARE INPUTS
    model           <- object$models$model_1
    id              <- object$extras$id
    idx_col         <- object$extras$idx_col
    freq            <- object$extras$freq
    constructed_tbl <- object$extras$constructed_tbl[[1]]

    # RECONSTRUCT GLUON DATA
    gluon_listdataset <- constructed_tbl %>%
        to_gluon_list_dataset(
            date_var  = !! rlang::sym(idx_col),
            value_var = value,
            id_var    = !! rlang::sym(id),
            freq      = freq
        )

    # PREDICTIONS
    preds <- make_gluon_predictions(
        model             = model,
        gluon_listdataset = gluon_listdataset,
        new_data          = new_data,
        id_col            = id,
        idx_col           = idx_col
    )

    return(preds)

}

#' @export
predict.nbeats_ensemble_fit_impl <- function(object, new_data, ...) {
    nbeats_ensemble_predict_impl(object, new_data, ...)
}
