


#' Bridge GluonTS DeepAR Modeling Function
#'
#' @export
deepar_fit_impl <- function(x, y, freq, horizon,

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
                            epochs = 100,
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

    HORIZON <- horizon
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
    extras <- list(
        x = x,
        y = y
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
    print(x$models$model_1$prediction_net)
    invisible(x)
}
