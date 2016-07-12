construction_base <- function(data, specs, tool, na.rm = FALSE) {
    data_prep(
        data = data,
        y = specs$vars$yvars,
        x = specs$vars$xvars,
        covars = specs$vars$covariates,
        int = specs$vars$interaction,
        id = specs$id,
        na.rm = na.rm
    ) %>%
        generate_results(tool, specs,
                         type = grep('bp', class(data), value = TRUE))
}

data_prep <- function(data, y, x, covars = NULL,
                      int = NULL, id = NULL, na.rm = TRUE) {

    prep <- data %>%
        dplyr::select_(.dots = c(id, y, x, covars, int)) %>%
        tidyr::gather_('Yterms', 'YtermValues', y) %>%
        tidyr::gather_('Xterms', 'XtermValues', x)

    if (!is.null(id))
        prep <- dplyr::rename_(prep, 'id' = id)

    prep <- prep %>%
        dplyr::group_by_('Yterms', 'Xterms')

    if (na.rm)
        prep <- stats::na.omit(prep)

    make_blueprint(data, prepared = prep)
}

regression_formula <- function(specs) {
    vars <- specs$vars
    if (length(vars$interaction) == 1) {
        int <- paste0('XtermValues:', vars$interaction)
    } else {
        int <- NULL
    }

    stats::reformulate(c('XtermValues',
                         vars$covariates, int),
                       response = 'YtermValues')
}

generate_results <- function(data, tool, specs, type) {

    results <- attr(data, 'specs')$prepared %>%
        dplyr::do_(.dots = tool) %>%
        dplyr::ungroup() %>%
        dplyr::tbl_df()

    # Remove in case prepared data already existed.
    attr(data, 'specs')$prepared <- NULL
    append_results(data, specs, results, type)
}

append_results <- function(data, specs, results, type) {
    if (!is.null(attr(data, 'specs')$results))
        results <- dplyr::bind_rows(attr(data, 'specs')$results, results)

    attr(data, 'specs')$results <- NULL
    make_blueprint(data, results = results, type = type)
}

