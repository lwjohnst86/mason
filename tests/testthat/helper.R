
testdata <- data.frame(state.region, state.x77) %>%
    dplyr::mutate(Rich = as.numeric(Income > 4500)) %>%
    tibble::as_tibble() %>%
    dplyr::arrange(state.region) %>%
    dplyr::mutate(Populated = as.numeric(Population > 3000))

fix_order <- function(.tbl) {
    .tbl %>%
        dplyr::arrange(term, estimate)
}
