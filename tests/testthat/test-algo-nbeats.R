# DEEP AR TEST ----
context("Test NBEATS")


# MODEL FITTING ----

test_that("nbeats: model fitting", {

    skip_if_no_gluonts()

    # ** MODEL FIT

    # Model Spec
    model_spec <<- nbeats(
        id                      = "id",
        freq                    = "M",
        prediction_length       = 24,
        epochs                  = 1,
        batch_size              = 2,
        num_batches_per_epoch   = 10,
        learn_rate              = 0.01,
        learn_rate_decay_factor = 0.25,
        learn_rate_min          = 2e-5,
        patience                = 100,
        clip_gradient           = 1,
        penalty                 = 0.2,

        lookback_length         = 12,
        loss_function           = "MAPE",
        num_stacks              = 10,
        num_blocks              = list(2)
    ) %>%
        set_engine("gluonts_nbeats")



    # Model Fit
    model_fit <- model_spec %>%
        fit(log(value) ~ date + id, data = training(m750_splits))

    # Test print
    expect_equal(print(model_fit), model_fit)

    # Structure

    testthat::expect_s3_class(model_fit$fit, "nbeats_fit_impl")

    testthat::expect_s3_class(model_fit$fit$data, "tbl_df")

    testthat::expect_equal(names(model_fit$fit$data)[1], "date")

    testthat::expect_equal(model_fit$fit$extras$id, "id")
    testthat::expect_equal(model_fit$fit$extras$idx_column, "date")
    testthat::expect_equal(model_fit$fit$extras$value_column, "value")
    testthat::expect_s3_class(model_fit$fit$extras$constructed_tbl[[1]], "data.frame")

    # $fit DEEPAR

    testthat::expect_s3_class(model_fit$fit$models$model_1, "gluonts.model.predictor.RepresentableBlockPredictor")

    testthat::expect_equal(model_fit$fit$models$model_1$batch_size %>% py_to_r(), 2)
    testthat::expect_equal(model_fit$fit$models$model_1$freq %>% py_to_r(), "M")

    model_fit$fit$models$model_1$prediction_net$num_stacks

    testthat::expect_equal(model_fit$fit$models$model_1$prediction_net$prediction_length %>% py_to_r(), 24)
    testthat::expect_equal(model_fit$fit$models$model_1$prediction_net$context_length %>% py_to_r(), 12)
    testthat::expect_equal(model_fit$fit$models$model_1$prediction_net$num_stacks %>% py_to_r(), 10)


    # $preproc

    testthat::expect_equal(model_fit$preproc$y_var, "value")


    # ** PREDICTIONS

    # Predictions
    predictions_tbl <- model_fit %>%
        modeltime_calibrate(testing(m750_splits)) %>%
        modeltime_forecast(new_data = testing(m750_splits))

    # Structure
    testthat::expect_identical(nrow(testing(m750_splits)), nrow(predictions_tbl))
    testthat::expect_identical(testing(m750_splits)$date, predictions_tbl$.index)



})

# UPDATE MODEL SPEC ----



testthat::test_that("nbeats: update model spec", {

    skip_if_no_gluonts()

    model_spec_updated <- model_spec %>%
        update(
            id                      = "id_2",
            freq                    = "D",
            prediction_length       = 36,
            epochs                  = 2,
            batch_size              = 4,
            num_batches_per_epoch   = 6,
            learn_rate              = 0.0001,
            learn_rate_decay_factor = 0.5,
            learn_rate_min          = 1e-5,
            patience                = 10,
            clip_gradient           = 10,

            loss_function           = "sMAPE"
        )

    expect_equal(eval_tidy(model_spec_updated$args$id), "id_2")
    expect_equal(eval_tidy(model_spec_updated$args$freq), "D")
    expect_equal(eval_tidy(model_spec_updated$args$prediction_length), 36)
    expect_equal(eval_tidy(model_spec_updated$args$epochs), 2)
    expect_equal(eval_tidy(model_spec_updated$args$batch_size), 4)
    expect_equal(eval_tidy(model_spec_updated$args$num_batches_per_epoch), 6)
    expect_equal(eval_tidy(model_spec_updated$args$learn_rate), 0.0001)
    expect_equal(eval_tidy(model_spec_updated$args$learn_rate_decay_factor), 0.5)
    expect_equal(eval_tidy(model_spec_updated$args$learn_rate_min), 1e-5)
    expect_equal(eval_tidy(model_spec_updated$args$patience), 10)
    expect_equal(eval_tidy(model_spec_updated$args$clip_gradient), 10)

    expect_equal(eval_tidy(model_spec_updated$args$loss_function), "sMAPE")
    expect_equal(eval_tidy(model_spec_updated$args$num_stacks), 10)
    expect_equal(eval_tidy(model_spec_updated$args$num_blocks), list(2))

})

# CHECKS / VALIDATIONS ----

testthat::test_that("nbeats: checks/validations", {

    skip_if_no_gluonts()

    # Missing prediction length
    expect_error({
        nbeats() %>%
            set_engine("gluonts_nbeats") %>%
            fit(value ~ date + id, training(m750_splits))
    })

    # Missing freq
    expect_error({
        nbeats(prediction_length = 24) %>%
            set_engine("gluonts_nbeats") %>%
            fit(value ~ date + id, training(m750_splits))
    })

    # Missing ID argument
    expect_error({
        nbeats(freq = "M", prediction_length = 24) %>%
            set_engine("gluonts_nbeats") %>%
            fit(value ~ date + id, training(m750_splits))
    })

    # ID column not provided
    expect_error({
        model_spec %>%
            fit(value ~ date, training(m750_splits))
    })

    # Date column not provided
    expect_error({
        model_spec %>%
            fit(value ~ id, training(m750_splits))
    })
})
