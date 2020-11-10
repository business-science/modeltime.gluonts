


#' Bridge GluonTS DeepAR Modeling Function
#'
#' @export
#' @importFrom stats frequency
gluonts_deepar_fit_impl <- function(x, y, freq, horizon, epochs) {

    # X & Y
    # Expect outcomes  = vector
    # Expect predictor = data.frame
    outcome    <- y
    predictor  <- x

    # INDEX & PERIOD
    # Determine Period, Index Col, and Index
    index_tbl <- parse_index_from_data(predictor)
    period    <- parse_period_from_index(index_tbl, period)
    idx_col   <- names(index_tbl)
    idx       <- timetk::tk_index(index_tbl)

    HORIZON <- horizon
    FREQ    <- freq
    EPOCHS  <- epochs

    # Make GluonTS dataset
    gluon_data <- py$prepare_data_univariate(index = m750$date, values = m750$value, freq = FREQ)

    trainer    <- gluonts$trainer$Trainer(epochs = EPOCHS)
    model_spec <- gluonts$model$deepar$DeepAREstimator(
        freq              = FREQ,
        prediction_length = HORIZON,
        trainer           = trainer
    )
    model_fit  <- model_spec$train(training_data = gluon_data)

    return(model_fit)

    # RETURN A NEW MODELTIME BRIDGE

    # # Class - Add a class for the model
    # class <- "gluonts_deepar_fit_impl"
    #
    # # Models - Insert model_1 and model_2 into a list
    # models <- list(
    #     model_fit
    # )
    #
    # # Data - Start with index tbl and add .actual, .fitted, and .residuals columns
    # data <- index_tbl %>%
    #     dplyr::mutate(
    #         .actual    =  y,
    #         .fitted    =  arima_fitted + xgboost_fitted,
    #         .residuals = .actual - .fitted
    #     )
    #
    # # Extras - Pass on transformation recipe
    # extras <- list(
    #     xreg_recipe = xreg_recipe
    # )
    #
    # # Model Description - Gets printed to describe the high-level model structure
    # desc <- paste0(get_arima_description(fit_arima),
    #                ifelse(is.null(fit_xgboost), "", " w/ XGBoost Errors"))
    #
    # # Create new model
    # modeltime::new_modeltime_bridge(
    #     class  = class,
    #     models = models,
    #     data   = data,
    #     extras = extras,
    #     desc   = desc
    # )

}
