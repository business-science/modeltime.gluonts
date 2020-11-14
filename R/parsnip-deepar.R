
# DEEP AR ----

#' GluonTS DeepAR Modeling Function (Bridge)
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param freq A `pandas` timeseries frequency such as "5min" for 5-minutes or "D" for daily.
#'  Refer to [Pandas Offset Aliases](https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html#offset-aliases).
#' @param prediction_length Numeric value indicating the length of the prediction horizon
#' @param id_column A quoted column name that tracks the GluonTS FieldName "item_id"
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
#' @param cardinality Number of values of each categorical feature.
#'  This must be set if `use_feat_static_cat` == TRUE (default: NULL)
#' @param embedding_dimension Dimension of the embeddings for categorical features (default: `min(50, (cat+1)//2)` for cat in cardinality)
#' @param distr_output Distribution to use to evaluate observations and sample predictions (default: StudentTOutput())
#' @param scaling Whether to automatically scale the target values (default: TRUE)
#' @param lags_seq Indices of the lagged target values to use as inputs of the RNN
#'  (default: NULL, in which case these are automatically determined based on freq)
#' @param time_features Time features to use as inputs of the RNN
#'  (default: None, in which case these are automatically determined based on freq)
#' @param num_parallel_samples Number of evaluation samples per time series to increase parallelism during inference.
#'  This is a model optimization that does not affect the accuracy (default: 100)
#' @param ctx The mxnet CPU/GPU context. Refer to using CPU/GPU in the mxnet documentation.
#'  (default: NULL, uses CPU)
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
deepar_fit_impl <- function(x, y, freq, prediction_length, id_column = "item_id",

                            # Trainer Args
                            epochs = 100,
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
                            num_parallel_samples = 100

                            ) {

    # ARG CHECKS ----
    validate_gluonts_required_args(x, prediction_length, freq, id_column)

    # Convert args
    if (is.null(context_length)) context_length <- reticulate::py_none()
    if (is.null(ctx)) ctx <- reticulate::py_none()
    if (is.null(cardinality)) cardinality <- reticulate::py_none()
    if (is.null(embedding_dimension)) embedding_dimension <- reticulate::py_none()
    if (distr_output == "default") distr_output <- gluonts$distribution$student_t$StudentTOutput()
    if (is.null(lags_seq)) lags_seq <- reticulate::py_none()
    if (is.null(time_features)) time_features <- reticulate::py_none()
    # if (is.null(imputation_method)) imputation_method <- reticulate::py_none()

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
    id_tbl <- x %>% dplyr::select(dplyr::all_of(id_column))

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
            id_var    = !! rlang::sym(id_column),
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
    model_spec <- gluonts$model$deepar$DeepAREstimator(
        freq                   = freq,
        prediction_length      = prediction_length,

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
    model_fit  <- model_spec$train(training_data = gluon_listdataset)

    # GET FITTED
    # TODO - Not sure if this is possible. Return fitted values as NA for now

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
    extras <- list(
        id_column       = id_column,
        idx_column      = idx_col,
        value_column    = "value",
        freq            = freq,
        constructed_tbl = list(constructed_tbl)
    )

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
    model           <- object$models$model_1
    id_column       <- object$extras$id_column
    idx_col         <- object$extras$idx_col
    freq            <- object$extras$freq
    constructed_tbl <- object$extras$constructed_tbl[[1]]

    # RECONSTRUCT GLUON DATA
    gluon_listdataset <- constructed_tbl %>%
        to_gluon_list_dataset(
            date_var  = !! rlang::sym(idx_col),
            value_var = value,
            id_var    = !! rlang::sym(id_column),
            freq      = freq
        )

    # PREDICTIONS
    prediction <- model$predict(gluon_listdataset)

    ids  <- list()
    vals <- list()
    dict <- reticulate::iter_next(prediction)
    i    <- 1
    while (!is.null(dict)) {

        ids[[i]]  <- dict$item_id %>% as.character()
        vals[[i]] <- as.numeric(dict$mean)

        i    <- i + 1
        dict <- reticulate::iter_next(prediction)
    }

    reconstructed <- purrr::map2(ids, vals, .f = function(x, y) {
        tibble::tibble(
            id    = x,
            value = y
        )
    }) %>%
        dplyr::bind_rows() %>%
        dplyr::group_by(id) %>%
        dplyr::mutate(seq = 1:length(id)) %>%
        dplyr::ungroup()

    new_data_merged <- new_data %>%

        tibble::rowid_to_column(var = ".row_id") %>%
        dplyr::mutate(id = !! rlang::sym(id_column)) %>%
        dplyr::select(.row_id, id, !! rlang::sym(idx_col)) %>%

        dplyr::group_by(id) %>%
        dplyr::arrange(!! rlang::sym(idx_col)) %>%
        dplyr::mutate(seq = 1:length(id)) %>%
        dplyr::ungroup() %>%

        dplyr::left_join(reconstructed, by = c("id" = "id", "seq" = "seq")) %>%
        dplyr::arrange(.row_id)

    preds <- new_data_merged$value

    return(preds)

}

#' @export
predict.deepar_fit_impl <- function(object, new_data, ...) {
    deepar_predict_impl(object, new_data, ...)
}
