
# NBEATS & NBEATS ENSEMBLE -----

#' General Interface for N-BEATS Time Series Models
#'
#' `nbeats()` is a way to generate a _specification_ of a N-BEATS model
#'  before fitting and allows the model to be created using
#'  different packages. Currently the only package is `gluonts`.
#'  There are 2 N-Beats implementations: (1) Standard N-Beats, and (2) Ensemble N-Beats.
#'
#' @inheritParams deep_ar
#' @inheritParams nbeats_fit_impl
#' @param lookback_length Number of time units that condition the predictions Also known as 'lookback period'. Default is 2 * prediction_length.
#' @param bagging_size (Applicable to Ensemble N-Beats). The number of models that share the parameter combination of 'context_length' and 'loss_function'.
#'  Each of these models gets a different initialization random initialization. Default and recommended value: 10.
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
#' - __GluonTS N-BEATS:__ "gluonts_nbeats" (the default)
#' - __GluonTS N-BEATS Ensemble:__ "gluonts_nbeats_ensemble"
#'
#' @section Engine Details:
#'
#' The standardized parameter names in `modeltime` can be mapped to their original
#' names in each engine:
#'
#' ```{r echo = FALSE}
#' tibble::tribble(
#'     ~ "modeltime", ~ "NBEATSEstimator", ~ "NBEATSEnsembleEstimator",
#'     "id", "ListDataset('item_id')", "ListDataset('item_id')",
#'     "freq", "freq", "freq",
#'     "prediction_length", "prediction_length", "prediction_length",
#'     "lookback_length", "context_length (= 2 x prediction_length)", "meta_context_length (= prediction_length x c(2,4))",
#'     "bagging_size", "NA", "meta_bagging_size (3)",
#'     "loss_function", "loss_function ('sMAPE')", "meta_loss_function (list('sMAPE'))",
#'     "num_stacks", "num_stacks (30)", "num_stacks (30)",
#'     "num_blocks", "num_blocks (list(1))", "num_blocks (list(1))",
#'     "epochs", "epochs (5)", "epochs (5)",
#'     "batch_size", "batch_size (32)", "batch_size (32)",
#'     "num_batches_per_epoch", "num_batches_per_epoch (50)", "num_batches_per_epoch (50)",
#'     "learn_rate", "learning_rate (0.001)", "learning_rate (0.001)",
#'     "learn_rate_decay_factor", "learning_rate_decay_factor (0.5)", "learning_rate_decay_factor (0.5)",
#'     "learn_rate_min", "minimum_learning_rate (5e-5)", "minimum_learning_rate (5e-5)",
#'     "patience", "patience (10)", "patience (10)",
#'     "clip_gradient", "clip_gradient (10)", "clip_gradient (10)",
#'     "penalty", "weight_decay (1e-8)", "weight_decay (1e-8)"
#' ) %>% knitr::kable()
#' ```
#'
#' Other options can be set using `set_engine()`.
#'
#'
#' @section Engine: gluonts_nbeats
#'
#' The engine uses `gluonts.model.n_beats.NBEATSEstimator()`.
#' Default values that have been changed to prevent long-running computations:
#'
#' - `epochs = 5`: GluonTS uses 100 by default.
#' - `loss_function = 'sMAPE'`: GluonTS by default uses MAPE. MAPE can suffer from issues with small values.
#'
#' _Required Parameters_
#'
#' The `gluonts_nbeats` implementation has several _Required Parameters_,
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
#' during the model specification process (e.g. `nbeats(id = "ID")` assuming
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
#' @section Engine: gluonts_nbeats_ensemble
#'
#' The engine uses `gluonts.model.n_beats.NBEATSEnsembleEstimator()`.
#'
#' _Number of Models Created_
#'
#' This model is very good, but can be expensive (long-running) due to the number of models that are being created.
#' The number of models follows the formula:
#'
#' `length(lookback_length) x length(loss_function) x meta_bagging_size`
#'
#' The default values that have been changed from GluonTS implementation to prevent long-running computations:
#'
#' - `epochs = 5`: GluonTS uses 100 by default.
#' - `lookback_length = prediction_length * c(2, 4)`. GluonTS uses range of 2:7, which doubles the number of models created.
#' - `bagging_size = 3`: Averages 5 like models together. GluonTS uses 10, which doubles the number of models created.
#' - `loss_function = 'sMAPE'`: GluonTS uses 3 `meta_loss_function = list('sMAPE', 'MASE', 'MAPE')`,
#'  which 3X's (triples) the number of models created.
#'
#' The result is: 2 x 1 x 3 = __6 models.__ Each model will have 5 epochs by default.
#'
#' _Required Parameters_
#'
#' The `gluonts_nbeats_ensemble` implementation has several _Required Parameters_,
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
#' during the model specification process (e.g. `nbeats(id = "ID")` assuming
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
#' `nbeats(id = "id")` expects a column inside your data named "id".
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
#' model_spec <- nbeats(
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
#'     set_engine("gluonts_nbeats")
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
nbeats <- function(
    mode = "regression",

    # Required Args
    id,
    freq,
    prediction_length,

    # N-BEATS Args
    lookback_length = NULL, # context_length
    loss_function = NULL,
    bagging_size = NULL,
    num_stacks = NULL,
    num_blocks = NULL,

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

        # NBEATS Args
        lookback_length         = rlang::enquo(lookback_length),
        loss_function           = rlang::enquo(loss_function),
        bagging_size            = rlang::enquo(bagging_size),
        num_stacks              = rlang::enquo(num_stacks),
        num_blocks              = rlang::enquo(num_blocks),

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
        "nbeats",
        args     = args,
        eng_args = NULL,
        mode     = mode,
        method   = NULL,
        engine   = NULL
    )

}

#' @export
print.nbeats <- function(x, ...) {
    cat("N-BEATS Model Specification (", x$mode, ")\n\n", sep = "")
    parsnip::model_printer(x, ...)

    if(!is.null(x$method$fit$args)) {
        cat("Model fit template:\n")
        print(parsnip::show_call(x))
    }

    invisible(x)
}

#' @export
#' @importFrom stats update
update.nbeats <- function(object, parameters = NULL,
                          id                      = NULL,
                          freq                    = NULL,
                          prediction_length       = NULL,

                          # NBEATS Args
                          lookback_length         = NULL,
                          loss_function           = NULL,
                          bagging_size            = NULL,
                          num_stacks              = NULL,
                          num_blocks              = NULL,

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

        # NBEATS Args
        lookback_length         = rlang::enquo(lookback_length),
        loss_function           = rlang::enquo(loss_function),
        bagging_size            = rlang::enquo(bagging_size),
        num_stacks              = rlang::enquo(num_stacks),
        num_blocks              = rlang::enquo(num_blocks),

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
        "nbeats",
        args     = object$args,
        eng_args = object$eng_args,
        mode     = object$mode,
        method   = NULL,
        engine   = object$engine
    )
}


#' @export
#' @importFrom parsnip translate
translate.nbeats <- function(x, engine = x$engine, ...) {
    if (is.null(engine)) {
        message("Used `engine = 'gluonts_nbeats'` for translation.")
        engine <- "gluonts_nbeats"
    }
    x <- parsnip::translate.default(x, engine, ...)

    x
}

# NBEATS ----

# * FIT -----

#' GluonTS N-BEATS Modeling Function (Bridge)
#'
#' @inheritParams deepar_fit_impl
#' @inheritParams nbeats_ensemble_fit_impl
#' @param context_length Number of time units that condition the predictions Also known as 'lookback period'. Default is 2 * prediction_length
#' @param loss_function The loss function (also known as metric) to use for training the network. Unlike other models in GluonTS this network does not use a distribution. One of the following: "sMAPE", "MASE" or "MAPE". The default value is "MAPE".
#'
#' @export
nbeats_fit_impl <- function(x, y, freq, prediction_length, id,

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
                            loss_function = "sMAPE",
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
    if (length(context_length) > 1) {
        rlang::abort("Only one 'lookback_length' allowed. Did you mean to use 'gluonts_nbeats_ensemble'.")
    }

    # Convert args
    if (is.null(context_length)) context_length <- reticulate::py_none()
    if (is.null(ctx)) ctx <- reticulate::py_none()
    num_blocks <- as.list(num_blocks)
    widths <- as.list(widths)
    sharing <- as.list(sharing)
    expansion_coefficient_lengths <- as.list(expansion_coefficient_lengths)
    stack_types <- as.list(stack_types)


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
    model_spec <- pkg.env$gluonts$model$n_beats$NBEATSEstimator(
        freq                          = freq,
        prediction_length             = prediction_length,

        trainer                       = trainer,

        context_length                = context_length,
        loss_function                 = loss_function,
        # meta_bagging_size             = meta_bagging_size,
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
    class <- "nbeats_fit_impl"

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
    desc <- "NBEATS"

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
print.nbeats_fit_impl <- function(x, ...) {
    cat(x$desc)
    cat("\n")
    cat("--------")
    cat("\nModel: ")
    print(x$models$model_1)
    cat("\n")
    print(x$models$model_1$prediction_net)
    invisible(x)
}

# * PREDICT ----

#' Bridge prediction Function for N-BEATS Models
#'
#' @inheritParams parsnip::predict.model_fit
#'
#' @export
nbeats_predict_impl <- function(object, new_data) {

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
predict.nbeats_fit_impl <- function(object, new_data, ...) {
    nbeats_predict_impl(object, new_data, ...)
}

# NBEATS ENSEMBLE ----

# * FIT -----

#' GluonTS N-BEATS ENSEMBLE Modeling Function (Bridge)
#'
#'
#' @inheritParams deepar_fit_impl
#' @param meta_context_length The different 'context_length' (also known as 'lookback period') to use for training the models. The 'context_length' is the number of time units that condition the predictions. Default and recommended value: `list(multiplier * prediction_length for multiplier in range(2, 7))`
#' @param meta_loss_function The different 'loss_function' (also known as metric) to use for training the models. Unlike other models in GluonTS this network does not use a distribution. Default and recommended value: `list("sMAPE", "MASE", "MAPE")`
#' @param meta_bagging_size The number of models that share the parameter combination of 'context_length' and 'loss_function'. Each of these models gets a different initialization random initialization. Default (3). Recommended value: 10
#' @param num_stacks The number of stacks the network should contain. Default and recommended value for generic mode: 30 Recommended value for interpretable mode: 2
#' @param num_blocks The number of blocks per stack. A list of ints of length 1 or 'num_stacks'. Default and recommended value for generic mode: 1. Recommended value for interpretable mode: 3.
#' @param widths Widths of the fully connected layers with ReLu activation in the blocks. A list of ints of length 1 or 'num_stacks'. Default and recommended value for generic mode: `list(512)` Recommended value for interpretable mode: `list(256, 2048)`
#' @param sharing Whether the weights are shared with the other blocks per stack. A list of ints of length 1 or 'num_stacks'. Default and recommended value for generic mode: `list(FALSE)` Recommended value for interpretable mode: `list(TRUE)`
#' @param expansion_coefficient_lengths If the type is "G" (generic), then the length of the expansion coefficient. If type is "T" (trend), then it corresponds to the degree of the polynomial. If the type is "S" (seasonal) then its not used. A list of ints of length 1 or 'num_stacks'. Default value for generic mode: `list(32)` Recommended value for interpretable mode: `list(3)`
#' @param stack_types One of the following values: "G" (generic), "S" (seasonal) or "T" (trend). A list of strings of length 1 or 'num_stacks'. Default and recommended value for generic mode: `list("G")` Recommended value for interpretable mode: `list("T","S")`
#'
#' @details
#'
#' The total number of models used is:
#'
#' `meta_context_length x meta_loss_function x meta_bagging_size`
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
                                     meta_context_length = prediction_length * c(2, 4),
                                     meta_loss_function = list('sMAPE'),
                                     meta_bagging_size = 3,
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
    if (is.null(meta_context_length)) {
        meta_context_length <- reticulate::py_none()
    } else {
        meta_context_length <- as.list(meta_context_length)
    }
    if (is.null(meta_loss_function)) {
        meta_loss_function <- reticulate::py_none()
    } else {
        meta_loss_function <- as.list(meta_loss_function)
    }
    if (is.null(ctx)) ctx <- reticulate::py_none()
    stack_types <- as.list(stack_types)
    expansion_coefficient_lengths <- as.list(expansion_coefficient_lengths)
    widths <- as.list(widths)
    num_blocks <- as.list(num_blocks)


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
    model_spec <- pkg.env$gluonts$model$n_beats$NBEATSEnsembleEstimator(
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
    cat("\nModel: ")
    print(x$models$model_1)
    invisible(x)
}

# * PREDICT ----

#' Bridge prediction Function for N-BEATS ENSEMBLE Models
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



