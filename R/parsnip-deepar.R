


#' Bridge GluonTS DeepAR Modeling Function
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param freq A `pandas` timeseries frequency.
#'  Refer to [Pandas Offset Aliases](https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html#offset-aliases).
#' @param prediction_length Length of the prediction horizon
#' @param epochs Number of epochs that the network will train (default: 100).
#' @param context_length Number of steps to unroll the RNN for before computing predictions
#'  (default: NULL, in which case context_length = prediction_length)
#' @param num_layers Number of RNN layers (default: 2)
#' @param num_cells Number of RNN cells for each layer (default: 40)
#' @param cell_type Type of recurrent cells to use (available: 'lstm' or 'gru'; default: 'lstm')
#' @param dropout_rate Dropout regularization parameter (default: 0.1)
#' @param use_feat_dynamic_real Whether to use the 'feat_dynamic_real' field from the data (default: FALSE)
#' @param use_feat_static_cat Whether to use the feat_static_cat field from the data (default: FALSE)
#' @param use_feat_static_real Whether to use the feat_static_real field from the data (default: FALSE)
#' @param cardinality – Number of values of each categorical feature.
#'  This must be set if `use_feat_static_cat` == TRUE (default: NULL)
#' @param embedding_dimension Dimension of the embeddings for categorical features (default: [min(50, (cat+1)//2) for cat in cardinality])
#' @param distr_output – Distribution to use to evaluate observations and sample predictions (default: StudentTOutput())
#' @param scaling Whether to automatically scale the target values (default: TRUE)
#' @param lags_seq Indices of the lagged target values to use as inputs of the RNN
#'  (default: NULL, in which case these are automatically determined based on freq)
#' @param time_features Time features to use as inputs of the RNN
#'  (default: None, in which case these are automatically determined based on freq)
#' @param num_parallel_samples Number of evaluation samples per time series to increase parallelism during inference.
#'  This is a model optimization that does not affect the accuracy (default: 100)
#' @param ctx The mxnet CPU/GPU context. Refer to using CPU/GPU in the mxnet documentation.
#'  (defualt: NULL, uses CPU)
#' @param batch_size Number of examples in each batch (default: 32).
#' @param num_batches_per_epoch Number of batches at each epoch (default: 50).
#' @param learning_rate Initial learning rate (default:  10−3 ).
#' @param learning_rate_decay_factor Factor (between 0 and 1) by which to decrease the learning rate (default: 0.5).
#' @param patience The patience to observe before reducing the learning rate, nonnegative integer (default: 10).
#' @param minimum_learning_rate Lower bound for the learning rate (default:  5⋅10−5 ).
#' @param clip_gradient Maximum value of gradient. The gradient is clipped if it is too large (default: 10).
#' @param weight_decay The weight decay (or L2 regularization) coefficient. Modifies objective by adding a penalty for having large weights (default  10−8 ).
#' @param init Initializer of the weights of the network (default: “xavier”).
#' @param hybridize Increases efficiency by using symbolic programming. (default: TRUE)
#'
#'
#'
#' @export
deepar_fit_impl <- function(x, y, freq, prediction_length, epochs = 100,

                            # Algo Args
                            context_length = NULL,
                            num_layers = 2,
                            num_cells = 40,
                            cell_type = "lstm",
                            dropout_rate = 0.1,
                            use_feat_dynamic_real = FALSE,
                            use_feat_static_cat = FALSE,
                            use_feat_static_real = FALSE,
                            cardinality = NULL,
                            embedding_dimension = NULL,
                            distr_output = "default",
                            scaling = TRUE,
                            lags_seq = NULL,
                            time_features = NULL,
                            num_parallel_samples = 100,

                            # Trainer Args
                            ctx = NULL,
                            batch_size = 32,
                            num_batches_per_epoch = 50,
                            learning_rate = 0.001,
                            learning_rate_decay_factor = 0.5,
                            patience = 10,
                            minimum_learning_rate = 5e-5,
                            clip_gradient = 10,
                            weight_decay = 10-8,
                            init = "xavier",
                            hybridize = TRUE
                            ) {

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

    # Convert args
    if (is.null(context_length)) context_length <- reticulate::py_none()
    if (is.null(ctx)) ctx <- reticulate::py_none()
    if (is.null(cardinality)) cardinality <- reticulate::py_none()
    if (is.null(embedding_dimension)) embedding_dimension <- reticulate::py_none()
    if (distr_output == "default") distr_output <- gluonts$distribution$student_t$StudentTOutput()
    if (is.null(lags_seq)) lags_seq <- reticulate::py_none()
    if (is.null(time_features)) time_features <- reticulate::py_none()
    # if (is.null(imputation_method)) imputation_method <- reticulate::py_none()

    HORIZON <- prediction_length
    FREQ    <- freq
    EPOCHS  <- epochs

    # Construct GluonTS dataset
    gluon_data <- py$prepare_data_univariate(
        index  = idx,
        values = y,
        freq   = freq
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
    model_spec <- gluonts$model$deepar$DeepAREstimator(
        freq                   = FREQ,
        prediction_length      = HORIZON,
        trainer                = trainer,

        context_length         = context_length,
        num_layers             = num_layers,
        num_cells              = num_cells,
        cell_type              = cell_type,
        dropout_rate           = dropout_rate,
        use_feat_dynamic_real  = use_feat_dynamic_real,
        use_feat_static_cat    = use_feat_static_cat,
        use_feat_static_real   = use_feat_static_real,
        cardinality            = cardinality,
        embedding_dimension    = embedding_dimension,
        distr_output           = distr_output,
        scaling                = scaling,
        lags_seq               = lags_seq,
        time_features          = time_features,
        num_parallel_samples   = num_parallel_samples
    )

    # Train the model
    model_fit  <- model_spec$train(training_data = gluon_data)

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "deepar_fit_impl"

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
    extras <- NULL

    # Model Description - Gets printed to describe the high-level model structure
    desc <- "DeepAR"

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
print.deepar_fit_impl <- function(x, ...) {
    cat(x$desc)
    cat("\n")
    cat("--------")
    cat("\n")
    print(x$models$model_1$prediction_net)
    invisible(x)
}

#' Bridge prediction Function for DeepAR Models
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @export
deepar_predict_impl <- function(object, new_data) {

    # PREPARE INPUTS
    deepar_model  <- object$models$model_1
    idx_train     <- object$data %>% timetk::tk_index()
    y             <- object$data %>% dplyr::pull(.actual)
    h_horizon     <- nrow(new_data)
    freq          <- deepar_model$freq

    # RECONSTRUCT GLUON DATA
    gluon_data <- py$prepare_data_univariate(
        index  = idx_train,
        values = y,
        freq   = freq
    )

    # PREDICTIONS
    prediction <- deepar_model$predict(gluon_data)
    res        <- reticulate::iter_next(prediction)
    preds      <- as.numeric(res$mean)

    # HANDLE DELTA BETWEEN NEW DATA & PREDICTION LENGTH
    if (length(preds) < h_horizon) {
        warning(stringr::str_glue("The number of rows in 'new_data' is greater than GluonTS model's 'prediction_length'. Reconciling by padding NA values. Consider using a 'prediction_length' = {h_horizon}."))
        preds  <- c(preds, rep(NA, h_horizon))
    } else if (length(preds) < h_horizon) {
        # warning("The GluonTS model's 'prediction_length' is greater than the number of rows in 'new_data'. Reconciling by trimming values.")
    }
    preds <- preds[1:h_horizon]

    return(preds)

}

#' @export
predict.deepar_fit_impl <- function(object, new_data, ...) {
    deepar_predict_impl(object, new_data, ...)
}
