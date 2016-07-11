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

    data <- data %>%
        dplyr::select_(.dots = c(id, y, x, covars, int)) %>%
        tidyr::gather_('Yterms', 'YtermValues', y) %>%
        tidyr::gather_('Xterms', 'XtermValues', x)

    if (!is.null(id))
        data <- dplyr::rename_(data, 'id' = id)

    data <- data %>%
        dplyr::group_by_('Yterms', 'Xterms')

    if (na.rm)
        data <- na.omit(data)

    return(data)
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

    results <- data %>%
        dplyr::do_(.dots = tool) %>%
        dplyr::ungroup() %>%
        dplyr::tbl_df()

    data <- dplyr::ungroup(data)
    append_results(data, specs, results, type)
}

append_results <- function(data, specs, results, type) {
    if (!is.null(attr(data, 'results')))
        results <- dplyr::bind_rows(attr(data, 'results'), results)

    attr(data, 'specs') <- specs
    make_blueprint(data, results = results, type = type)
}

