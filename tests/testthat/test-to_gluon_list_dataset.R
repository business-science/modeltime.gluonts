context("TESTING GLUON LIST DATASET")

# No Groups ----
test_that("NO GROUPS - Gluon ListDataset Created Properly", {

    original   <- m750
    gluon_list <- original %>% to_gluon_list_dataset(date, value, freq = "M")
    gluon_iter <- as_iterator(gluon_list$list_data)

    ids  <- list()
    vals <- list()
    dict <- iter_next(gluon_iter)
    i    <- 1
    while (!is.null(dict)) {

        vals[[i]] <- dict$target %>% as.numeric()

        i    <- i + 1
        dict <- iter_next(gluon_iter)
    }

    reconstructed <- map(vals, .f = function(y) {
        tibble(
            value = y
        )
    }) %>%
        bind_rows()

    original_sub <- original %>%
        select(-date, -id)

    expect_identical(original_sub, reconstructed)

})

# Groups ----
test_that("GROUPS - Gluon ListDataset Created Properly", {

    original   <- m4_daily
    gluon_list <- original %>% to_gluon_list_dataset(
        date_var  = date,
        value_var = value,
        group_var = id,
        freq      = "M"
    )
    gluon_iter <- as_iterator(gluon_list$list_data)

    ids  <- list()
    vals <- list()
    dict <- iter_next(gluon_iter)
    i    <- 1
    while (!is.null(dict)) {

        ids[[i]]  <- dict$item_id %>% as.character()
        vals[[i]] <- dict$target %>% as.numeric()

        i    <- i + 1
        dict <- iter_next(gluon_iter)
    }

    reconstructed <- map2(ids, vals, .f = function(x, y) {
        tibble(
            id    = x,
            value = y
        )
    }) %>%
        bind_rows()

    original_2 <- original %>%
        select(-date) %>%
        mutate(id = as.character(id))

    expect_identical(original_2, reconstructed)

})


