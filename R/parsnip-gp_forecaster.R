
# GP FORECASTER ----

#' General Interface for GP Forecaster Time Series Models
#'
#' `gp_forecaster()` is a way to generate a _specification_ of a Gaussian Process (GP) Forecaster model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `gluonts`.
#'
#' @inheritParams gp_forecaster_fit_impl
#' @param mode A single character string for the type of model.
#'  The only possible value for this model is "regression".
#' @param lookback_length Number of steps to unroll the RNN for before computing predictions
#'  (default: NULL, in which case context_length = prediction_length)
#' @param learn_rate Initial learning rate (default: 10-3).
#' @param learn_rate_decay_factor Factor (between 0 and 1) by which to decrease the learning rate (default: 0.5).
#' @param learn_rate_min Lower bound for the learning rate (default: 5x10-5 ).
#' @param penalty The weight decay (or L2 regularization) coefficient. Modifies objective by adding a penalty for having large weights (default 10-8 ).
#' @param scale Scales numeric data by `id` group using mean = 0, standard deviation = 1 transformation. (default: FALSE)
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
#' - __GluonTS GP Forecaster:__ "gluonts_gp_forecaster" (the default)
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "modeltime", ~ "GaussianProcessEstimator",
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
#'     "scale", "scale_by_id (FALSE)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#'
#' @section Engine: gluonts_gp_forecaster
#'
#' The engine uses `gluonts.model.gp_forecaster.GP ForecasterEstimator()`.
#' Default values that have been changed to prevent long-running computations:
#'
#' - `epochs = 5`: GluonTS uses 100 by default.
#' - `cardinality = 1`: GluonTS requires user to provide. You can change this via `set_engine()`
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
#' during the model specification process (e.g. `gp_forecaster(id = "ID")` assuming
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
#' `gp_forecaster(id = "id")` expects a column inside your data named "id".
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
#' model_spec <- gp_forecaster(
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
#'     set_engine("gluonts_gp_forecaster")
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
gp_forecaster <- function(
    mode = "regression",

    # Required Args
    id,
    freq,
    prediction_length,

    # Model Args
    lookback_length = NULL,


    # Trainer Args
    epochs = NULL,
    batch_size = NULL,
    num_batches_per_epoch = NULL, # 50
    learn_rate = NULL, # learning_rate, 0.001
    learn_rate_decay_factor = NULL, # learning_rate_decay_factor
    learn_rate_min = NULL, #minimum_learning_rate
    patience = NULL,
    clip_gradient = NULL,
    penalty = NULL, # weight_decay

    # Modeltime Args
    scale = NULL


) {

    args <- list(
        # Required Args
        id                      = rlang::enquo(id),
        freq                    = rlang::enquo(freq),
        prediction_length       = rlang::enquo(prediction_length),

        # Model Args
        lookback_length         = rlang::enquo(lookback_length), # context_length


        # Trainer Args
        epochs                  = rlang::enquo(epochs),
        batch_size              = rlang::enquo(batch_size),
        num_batches_per_epoch   = rlang::enquo(num_batches_per_epoch),
        learn_rate              = rlang::enquo(learn_rate),
        learn_rate_decay_factor = rlang::enquo(learn_rate_decay_factor),
        learn_rate_min          = rlang::enquo(learn_rate_min),
        patience                = rlang::enquo(patience),
        clip_gradient           = rlang::enquo(clip_gradient),
        penalty                 = rlang::enquo(penalty), # weight_decay

        # Modeltime Args
        scale                   = rlang::enquo(scale)
    )

    parsnip::new_model_spec(
        "gp_forecaster",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.gp_forecaster <- function(x, ...) {
    cat("GP Forecaster Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.gp_forecaster <- function(object, parameters = NULL,
                           id                      = NULL,
                           freq                    = NULL,
                           prediction_length       = NULL,

                           # Model Args
                           lookback_length         = NULL,

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

                           # Modeltime Args
                           scale                   = NULL,

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

        # GP Args
        lookback_length         = rlang::enquo(lookback_length), # context_length


        # Trainer Args
        epochs                  = rlang::enquo(epochs),
        batch_size              = rlang::enquo(batch_size),
        num_batches_per_epoch   = rlang::enquo(num_batches_per_epoch),
        learn_rate              = rlang::enquo(learn_rate),
        learn_rate_decay_factor = rlang::enquo(learn_rate_decay_factor),
        learn_rate_min          = rlang::enquo(learn_rate_min),
        patience                = rlang::enquo(patience),
        clip_gradient           = rlang::enquo(clip_gradient),
        penalty                 = rlang::enquo(penalty), # weight_decay

        # Modeltime Args
        scale                   = rlang::enquo(scale)
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
        "gp_forecaster",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.gp_forecaster <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'gluonts_gp_forecaster'` for translation.")
        engine <- "gluonts_gp_forecaster"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}

# FIT -----

#' GluonTS GP Forecaster Modeling Function (Bridge)
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
#' @param cardinality Number of time series. Default: 1
#' @param kernel_output KernelOutput instance to determine which kernel subclass to be instantiated (default: RBFKernelOutput()).
#' @param params_scaling Determines whether or not to scale the model parameters (default: True).
#' @param max_iter_jitter Maximum number of iterations for jitter to iteratively make the matrix positive definite (default: 10).
#' @param jitter_method Iteratively jitter method or use eigenvalue decomposition depending on problem size (default: "iter").
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
#' @param scale_by_id Scales numeric data by `id` group using mean = 0, standard deviation = 1 transformation. (default: FALSE)
#' @param ... Additional parameters passed to `gluonts.model.gp_forecaster.GP ForecasterEstimator()`
#'
#' @keywords internal
#'
#' @export
gp_forecaster_fit_impl <- function(x, y, freq, prediction_length, id,

                            # Trainer Args
                            epochs = 5,
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
                            cardinality = 1,
                            kernel_output = NULL,
                            params_scaling = TRUE,
                            max_iter_jitter = 10,
                            jitter_method = "iter",
                            time_features = NULL,
                            num_parallel_samples = 100,

                            batch_size = 32,

                            # Modeltime Args
                            scale_by_id = FALSE,

                            ...

                            ) {

    # ARG CHECKS ----
    validate_gluonts_required_args(x, prediction_length, freq, id)

    # Convert args
    if (is.null(context_length)) context_length <- reticulate::py_none()
    if (is.null(ctx)) ctx <- reticulate::py_none()
    if(is.null(kernel_output)) kernel_output <- pkg.env$gluonts$mx.kernels$`_rbf_kernel`$RBFKernelOutput()
    if (is.null(time_features)) time_features <- reticulate::py_none()

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- modeltime::parse_index_from_data(x)
    # period    <- modeltime::parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    # ID COLUMN
    id_tbl <- x %>% dplyr::select(dplyr::all_of(id))

    # VALUE COLUMN
    value_tbl <- tibble::tibble(value = y)

    # PREPROCESSING
    # - Critical to scale/center target by id
    scale_params <- NULL
    if (scale_by_id) {

        transform_results_list <- dplyr::bind_cols(id_tbl, value_tbl) %>%
            transformer_scaler(id = !! rlang::sym(id), value = value)

        value_tbl    <- transform_results_list$transformed %>% dplyr::select(value)
        scale_params <- transform_results_list$params

    }


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
    trainer    <- pkg.env$gluonts$mx$trainer$`_base`$Trainer(
        ctx                        = ctx,
        epochs                     = epochs,
        # batch_size                 = batch_size,
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
    model_spec <- pkg.env$gluonts$model$gp_forecaster$GaussianProcessEstimator(
        freq                   = freq,
        prediction_length      = prediction_length,
        trainer                = trainer,
        context_length         = context_length,

        cardinality            = cardinality,

        params_scaling         = params_scaling,
        max_iter_jitter        = max_iter_jitter,
        jitter_method          = jitter_method,
        time_features          = time_features,
        num_parallel_samples   = num_parallel_samples,

        # New Parameters:
        batch_size             = batch_size,

        ...
    )

    # Train the model
    model_fit  <- model_spec$train(training_data = gluon_listdataset)

    # GET FITTED
    # TODO - Not sure if this is possible. Return fitted values as NA for now

    # RETURN A NEW MODELTIME BRIDGE

    # Class - Add a class for the model
    class <- "gp_forecaster_fit_impl"

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
        id                = id,
        idx_column        = idx_col,
        value_column      = "value",
        freq              = freq,
        prediction_length = prediction_length,
        grps              = constructed_tbl %>% dplyr::pull(!! rlang::sym(id)) %>% unique(),
        constructed_tbl   = constructed_tbl,
        scale_params      = scale_params
    )

    # Model Description - Gets printed to describe the high-level model structure
    desc <- "GP Forecaster"

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
print.gp_forecaster_fit_impl <- function(x, ...) {
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

#' Bridge prediction Function for GP Forecaster Models
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @keywords internal
#'
#' @export
gp_forecaster_predict_impl <- function(object, new_data) {

    # PREPARE INPUTS
    model           <- object$models$model_1
    id              <- object$extras$id
    idx_col         <- object$extras$idx_col
    freq            <- object$extras$freq
    constructed_tbl <- object$extras$constructed_tbl
    scale_params    <- object$extras$scale_params

    # # COMBINE NEW DATA & TRAIN DATA
    # constructed_new_data <- new_data %>%
    #     dplyr::select(all_of(c(id, idx_col))) %>%
    #     dplyr::mutate(value = NaN)
    #
    # constructed_tbl <- constructed_tbl %>%
    #     bind_rows(constructed_new_data)

    # RECONSTRUCT GLUON DATA
    gluon_listdataset <- to_gluon_list_dataset(
            data              = constructed_tbl,
            date_var          = !! rlang::sym(idx_col),
            value_var         = value,
            id_var            = !! rlang::sym(id),
            freq              = freq
        )

    # PREDICTIONS
    preds_tbl <- make_gluon_predictions(
        model             = model,
        gluon_listdataset = gluon_listdataset,
        new_data          = new_data,
        id_col            = id,
        idx_col           = idx_col
    )

    # print(preds_tbl)
    # print(scale_params)

    # RE-TRANSFORM
    if (!is.null(scale_params)) {

       preds_tbl <- inverter_scaler(
            data   = preds_tbl,
            id     = id,
            value  = value,
            params = scale_params
        ) %>%
            dplyr::arrange(.row_id)

    }

    preds <- preds_tbl$value

    return(preds)

}

#' @export
predict.gp_forecaster_fit_impl <- function(object, new_data, ...) {
    gp_forecaster_predict_impl(object, new_data, ...)
}
