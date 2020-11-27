
# DEEP AR ----

#' General Interface for DeepAR Time Series Models
#'
#' `deep_ar()` is a way to generate a _specification_ of a DeepAR model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `gluonts`.
#'
#' @inheritParams deepar_fit_impl
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param lookback_length Number of steps to unroll the RNN for before computing predictions
#'  (default: NULL, in which case context_length = prediction_length)
#' @param learn_rate Initial learning rate (default: 10-3).
#' @param learn_rate_decay_factor Factor (between 0 and 1) by which to decrease the learning rate (default: 0.5).
#' @param learn_rate_min Lower bound for the learning rate (default: 5x10-5 ).
#' @param dropout Dropout regularization parameter (default: 0.1)
#' @param penalty The weight decay (or L2 regularization) coefficient. Modifies objective by adding a penalty for having large weights (default 10-8 ).
#'
#' @details
#'
#' These arguments are converted to their specific names at the time that
#' the model is fit. Other options and arguments can be set using
#' `set_engine()`. If left to their defaults here (see above),
#' the values are taken from the underlying model functions.
#' If parameters need to be modified, `update()` can be used in lieu of recreating
#' the object from scratch.
#'
#' The model can be created using the fit() function using the following engines:
#'
#' - __GluonTS DeepAR:__ "gluonts_deepar" (the default)
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "modeltime", ~ "DeepAREstimator",
#'     "id", "NA",
#'     "freq", "freq",
#'     "prediction_length", "prediction_length",
#'     "lookback_length", "context_length (= prediction_length)",
#'     "epochs", "epochs (5)",
#'     "batch_size", "batch_size (32)",
#'     "num_batches_per_epoch", "num_batches_per_epoch (50)",
#'     "learn_rate", "learning_rate (0.001)",
#'     "learn_rate_decay_factor", "learning_rate_decay_factor (0.5)",
#'     "learn_rate_min", "minimum_learning_rate (5e-5)",
#'     "patience", "patience (10)",
#'     "clip_gradient", "clip_gradient (10)",
#'     "penalty", "weight_decay (1e-8)",
#'     "cell_type", "cell_type ('lstm')",
#'     "num_layers", "num_layers (2)",
#'     "num_cells", "num_cells (40)",
#'     "dropout", "dropout_rate (0.1)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#'
#' @section Engine: gluonts_deepar
#'
#' The engine uses `gluonts.model.deepar.DeepAREstimator()`.
#' Default values that have been changed to prevent long-running computations:
#'
#' - `epochs = 5`: GluonTS uses 100 by default.
#'
#' _Required Parameters_
#'
#' The `gluonts` implementation has several _Required Parameters_,
#' which are user-defined.
#'
#' _1. ID Variable (Required):_
#'
#' An important difference between other parsnip models is that
#' each time series (even single time series) must be uniquely identified
#' by an ID variable.
#'
#' - The ID feature must be of class `character` or `factor`.
#' - This ID feature is provided as a quoted expression
#' during the model specification process (e.g. `deep_ar(id = "ID")` assuming
#' you have a column in your data named "ID").
#'
#' _2. Frequency (Required):_
#'
#' The GluonTS models use a Pandas Timestamp Frequency `freq` to generate
#' features internally. Examples:
#'
#' - `freq = "5min"` for timestamps that are 5-minutes apart
#' - `freq = "D"` for Daily Timestamps
#'
#' The Pandas Timestamps are quite flexible.
#' Refer to [Pandas Offset Aliases](https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html#offset-aliases).
#'
#' _3. Prediction Length (Required):_
#'
#' Unlike other parsnip models, a `prediction_length` is required
#' during the model specification and fitting process.
#'
#'
#'
#' @section Fit Details:
#'
#' The following features are REQUIRED to be available in the incoming data for the
#' fitting process.
#'
#' - __Fit:__ `fit(y ~ date + id, data)`: Includes a target feature that is a
#' function of a "date" and "id" feature. The ID feature must be pre-specified
#' in the model_specification.
#' - __Predict:__ `predict(model, new_data)` where `new_data` contains both
#'  a column named "date" and "id".
#'
#' __ID Variable__
#'
#' An ID feature must be included in the recipe or formula fitting
#' process. This assists with cataloging the time series inside `GluonTS` ListDataset.
#' The column name must match the quoted feature name specified in the
#' `deep_ar(id = "id")` expects a column inside your data named "id".
#'
#' __Date and Date-Time Variable__
#'
#' It's a requirement to have a date or date-time variable as a predictor.
#' The `fit()` interface accepts date and date-time features and handles them internally.
#'
#'
#'
#'
#' @seealso [fit.model_spec()], [set_engine()]
#'
#' @examples
#' \donttest{
#' library(tidymodels)
#' library(tidyverse)
#' library(timetk)
#'
#'
#' # ---- MODEL SPEC ----
#' # - Important: Make sure *required* parameters are provided
#' model_spec <- deep_ar(
#'
#'     # User Defined (Required) Parameters
#'     id                    = "id",
#'     freq                  = "M",
#'     prediction_length     = 24,
#'
#'     # Hyper Parameters
#'     epochs                = 1,
#'     num_batches_per_epoch = 4
#' ) %>%
#'     set_engine("gluonts_deepar")
#'
#' model_spec
#'
#' # ---- TRAINING ----
#' # Important: Make sure the date and id features are included as regressors
#' #  and do NOT dummy the id feature.
#' model_fitted <- model_spec %>%
#'     fit(value ~ date + id, m750)
#'
#' model_fitted
#'
#' # ---- PREDICT ----
#' # - IMPORTANT: New Data must have id and date features
#' new_data <- tibble(
#'     id   = factor("M750"),
#'     date = as.Date("2015-07-01")
#' )
#'
#' predict(model_fitted, new_data)
#' }
#'
#' @export
deep_ar <- function(
    mode = "regression",

    # Required Args
    id,
    freq,
    prediction_length,

    # LSTM Args
    lookback_length = NULL,
    cell_type = NULL,
    num_layers = NULL,
    num_cells = NULL,
    dropout = NULL, # dropout_rate

    # Trainer Args
    epochs = NULL,
    batch_size = NULL,
    num_batches_per_epoch = NULL, # 50
    learn_rate = NULL, # learning_rate, 0.001
    learn_rate_decay_factor = NULL, # learning_rate_decay_factor
    learn_rate_min = NULL, #minimum_learning_rate
    patience = NULL,
    clip_gradient = NULL,
    penalty = NULL # weight_decay


) {

    args <- list(
        # Required Args
        id                      = rlang::enquo(id),
        freq                    = rlang::enquo(freq),
        prediction_length       = rlang::enquo(prediction_length),

        # LSTM Args
        lookback_length         = rlang::enquo(lookback_length), # context_length
        cell_type               = rlang::enquo(cell_type),
        num_layers              = rlang::enquo(num_layers),
        num_cells               = rlang::enquo(num_cells),
        dropout                 = rlang::enquo(dropout),

        # Trainer Args
        epochs                  = rlang::enquo(epochs),
        batch_size              = rlang::enquo(batch_size),
        num_batches_per_epoch   = rlang::enquo(num_batches_per_epoch),
        learn_rate              = rlang::enquo(learn_rate),
        learn_rate_decay_factor = rlang::enquo(learn_rate_decay_factor),
        learn_rate_min          = rlang::enquo(learn_rate_min),
        patience                = rlang::enquo(patience),
        clip_gradient           = rlang::enquo(clip_gradient),
        penalty                 = rlang::enquo(penalty) # weight_decay
    )

    parsnip::new_model_spec(
        "deep_ar",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.deep_ar <- function(x, ...) {
    cat("DeepAR Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.deep_ar <- function(object, parameters = NULL,
                           id                      = NULL,
                           freq                    = NULL,
                           prediction_length       = NULL,

                           # LSTM Args
                           lookback_length         = NULL,
                           cell_type               = NULL,
                           num_layers              = NULL,
                           num_cells               = NULL,
                           dropout                 = NULL,

                           # Trainer Args
                           epochs                  = NULL,
                           batch_size              = NULL,
                           num_batches_per_epoch   = NULL,
                           learn_rate              = NULL,
                           learn_rate_decay_factor = NULL,
                           learn_rate_min          = NULL,
                           patience                = NULL,
                           clip_gradient           = NULL,
                           penalty                 = NULL,

                           fresh = FALSE, ...) {

    parsnip::update_dot_check(...)

    if (!is.null(parameters)) {
        parameters <- parsnip::check_final_param(parameters)
    }

    args <- list(
        # Required Args
        id                      = rlang::enquo(id),
        freq                    = rlang::enquo(freq),
        prediction_length       = rlang::enquo(prediction_length),

        # LSTM Args
        lookback_length         = rlang::enquo(lookback_length),
        cell_type               = rlang::enquo(cell_type),
        num_layers              = rlang::enquo(num_layers),
        num_cells               = rlang::enquo(num_cells),
        dropout                 = rlang::enquo(dropout),

        # Trainer Args
        epochs                  = rlang::enquo(epochs),
        batch_size              = rlang::enquo(batch_size),
        num_batches_per_epoch   = rlang::enquo(num_batches_per_epoch),
        learn_rate              = rlang::enquo(learn_rate),
        learn_rate_decay_factor = rlang::enquo(learn_rate_decay_factor),
        learn_rate_min          = rlang::enquo(learn_rate_min),
        patience                = rlang::enquo(patience),
        clip_gradient           = rlang::enquo(clip_gradient),
        penalty                 = rlang::enquo(penalty)
    )

    args <- parsnip::update_main_parameters(args, parameters)

    if (fresh) {
        object$args <- args
    } else {
        null_args <- purrr::map_lgl(args, parsnip::null_value)
        if (any(null_args))
            args <- args[!null_args]
        if (length(args) > 0)
            object$args[names(args)] <- args
    }

    parsnip::new_model_spec(
        "deep_ar",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.deep_ar <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'gluonts'` for translation.")
        engine <- "gluonts_deepar"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}

# FIT -----

#' GluonTS DeepAR Modeling Function (Bridge)
#'
#' @param x A dataframe of xreg (exogenous regressors)
#' @param y A numeric vector of values to fit
#' @param freq A `pandas` timeseries frequency such as "5min" for 5-minutes or "D" for daily.
#'  Refer to [Pandas Offset Aliases](https://pandas.pydata.org/pandas-docs/stable/user_guide/timeseries.html#offset-aliases).
#' @param prediction_length Numeric value indicating the length of the prediction horizon
#' @param id A quoted column name that tracks the GluonTS FieldName "item_id"
#' @param epochs Number of epochs that the network will train (default: 5).
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
#' @param learning_rate Initial learning rate (default:  10-3 ).
#' @param learning_rate_decay_factor Factor (between 0 and 1) by which to decrease the learning rate (default: 0.5).
#' @param patience The patience to observe before reducing the learning rate, nonnegative integer (default: 10).
#' @param minimum_learning_rate Lower bound for the learning rate (default:  5x10-5 ).
#' @param clip_gradient Maximum value of gradient. The gradient is clipped if it is too large (default: 10).
#' @param weight_decay The weight decay (or L2 regularization) coefficient. Modifies objective by adding a penalty for having large weights (default  10-8 ).
#' @param init Initializer of the weights of the network (default: “xavier”).
#' @param hybridize Increases efficiency by using symbolic programming. (default: TRUE)
#'
#'
#'
#' @export
deepar_fit_impl <- function(x, y, freq, prediction_length, id,

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
    validate_gluonts_required_args(x, prediction_length, freq, id)

    # Convert args
    if (is.null(context_length)) context_length <- reticulate::py_none()
    if (is.null(ctx)) ctx <- reticulate::py_none()
    if (is.null(cardinality)) cardinality <- reticulate::py_none()
    if (is.null(embedding_dimension)) embedding_dimension <- reticulate::py_none()
    if (distr_output == "default") distr_output <- pkg.env$gluonts$distribution$student_t$StudentTOutput()
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
    trainer    <- pkg.env$gluonts$trainer$Trainer(
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
    model_spec <- pkg.env$gluonts$model$deepar$DeepAREstimator(
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
        id              = id,
        idx_column      = idx_col,
        value_column    = "value",
        freq            = freq,
        grps            = constructed_tbl %>% dplyr::pull(!! rlang::sym(id)) %>% unique(),
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
    cat("\nModel: ")
    print(x$models$model_1)
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
deepar_predict_impl <- function(object, new_data) {

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
predict.deepar_fit_impl <- function(object, new_data, ...) {
    deepar_predict_impl(object, new_data, ...)
}
