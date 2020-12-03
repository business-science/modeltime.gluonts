

make_gluon_predictions <- function(model, gluon_listdataset, new_data, id_col, idx_col) {

    # 1. Generate Predictions
    prediction <- model$predict(gluon_listdataset)

    # 2. Extract Predictions
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

    # 3. Reconstruct a Tibble from Lists
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

    # 4. Merge Predictions from Reconstructed Tibble with New Data
    # - New Values are filled in by group & date
    # - Order is then arranged based on the original order of New Data
    new_data_predictions_merged <- new_data %>%

        tibble::rowid_to_column(var = ".row_id") %>%
        dplyr::mutate(id = !! rlang::sym(id_col)) %>%
        dplyr::select(.row_id, id, !! rlang::sym(idx_col)) %>%

        dplyr::group_by(id) %>%
        dplyr::arrange(!! rlang::sym(idx_col)) %>%
        dplyr::mutate(seq = 1:length(id)) %>%
        dplyr::ungroup() %>%

        dplyr::left_join(reconstructed, by = c("id" = "id", "seq" = "seq")) %>%
        dplyr::arrange(.row_id)

    return(new_data_predictions_merged)
}
