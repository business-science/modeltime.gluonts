# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

# DeepAR ----

make_deep_ar <- function() {

    # SETUP
    model <- "deep_ar"
    mode  <- "regression"
    eng   <- "gluonts_deepar"

    parsnip::set_new_model(model)
    parsnip::set_model_mode(model, mode)

    # deep_ar: regression ----

    # * Model ----
    parsnip::set_model_engine(model, mode = mode, eng = eng)
    parsnip::set_dependency(model, eng = eng, pkg = "reticulate")
    parsnip::set_dependency(model, eng = eng, pkg = "modeltime.gluonts")

    # * Args ----
    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "id",
        original     = "id",
        func         = list(pkg = "modeltime.gluonts", fun = "id"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "freq",
        original     = "freq",
        func         = list(pkg = "modeltime.gluonts", fun = "freq"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "prediction_length",
        original     = "prediction_length",
        func         = list(pkg = "modeltime.gluonts", fun = "prediction_length"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "lookback_length",
        original     = "context_length",
        func         = list(pkg = "modeltime.gluonts", fun = "lookback_length"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "epochs",
        original     = "epochs",
        func         = list(pkg = "dials", fun = "epochs"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "batch_size",
        original     = "batch_size",
        func         = list(pkg = "dials", fun = "batch_size"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "num_batches_per_epoch",
        original     = "num_batches_per_epoch",
        func         = list(pkg = "modeltime.gluonts", fun = "num_batches_per_epoch"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "learn_rate",
        original     = "learning_rate",
        func         = list(pkg = "dials", fun = "learn_rate"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "learn_rate_decay_factor",
        original     = "learning_rate_decay_factor",
        func         = list(pkg = "modeltime.gluonts", fun = "learn_rate_decay_factor"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "learn_rate_min",
        original     = "minimum_learning_rate",
        func         = list(pkg = "modeltime.gluonts", fun = "learn_rate_min"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "patience",
        original     = "patience",
        func         = list(pkg = "modeltime.gluonts", fun = "patience"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "clip_gradient",
        original     = "clip_gradient",
        func         = list(pkg = "modeltime.gluonts", fun = "clip_gradient"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "penalty",
        original     = "weight_decay",
        func         = list(pkg = "dials", fun = "penalty"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "cell_type",
        original     = "cell_type",
        func         = list(pkg = "modeltime.gluonts", fun = "cell_type"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "num_layers",
        original     = "num_layers",
        func         = list(pkg = "modeltime.gluonts", fun = "num_layers"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "num_cells",
        original     = "num_cells",
        func         = list(pkg = "modeltime.gluonts", fun = "num_cells"),
        has_submodel = FALSE
    )

    parsnip::set_model_arg(
        model        = model,
        eng          = eng,
        parsnip      = "dropout",
        original     = "dropout_rate",
        func         = list(pkg = "dials", fun = "dropout"),
        has_submodel = FALSE
    )

    # * Encoding ----
    parsnip::set_encoding(
        model   = model,
        eng     = eng,
        mode    = mode,
        options = list(
            predictor_indicators = "none",
            compute_intercept    = FALSE,
            remove_intercept     = FALSE,
            allow_sparse_x       = FALSE
        )
    )

    # * Fit ----
    parsnip::set_fit(
        model         = model,
        eng           = eng,
        mode          = mode,
        value         = list(
            interface = "data.frame",
            protect   = c("x", "y"),
            func      = c(fun = "deepar_fit_impl"),
            defaults  = list()
        )
    )

    # * Predict ----
    parsnip::set_pred(
        model         = model,
        eng           = eng,
        mode          = mode,
        type          = "numeric",
        value         = list(
            pre       = NULL,
            post      = NULL,
            func      = c(fun = "predict"),
            args      =
                list(
                    object   = rlang::expr(object$fit),
                    new_data = rlang::expr(new_data)
                )
        )
    )

}

# nocov end
