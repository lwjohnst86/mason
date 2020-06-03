
fix_order <- function(.tbl) {
    .tbl %>%
        dplyr::arrange(term, estimate)
}
