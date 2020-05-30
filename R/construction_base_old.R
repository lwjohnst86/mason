construction_base_old <- function(data, specs, tool, na.rm = FALSE) {
    data_prep_old(
        data = data,
        y = specs$vars$yvars,
        x = specs$vars$xvars,
        covars = specs$vars$covariates,
        int = specs$vars$interaction,
        id = specs$id,
        na.rm = na.rm
    ) %>%
        generate_results_old(tool, specs,
                         type = grep('bp', class(data), value = TRUE))
}

data_prep_old <- function(data, y, x, covars = NULL,
                      int = NULL, id = NULL, na.rm = TRUE) {

    prep <- data %>%
        dplyr::select_(.dots = c(id, y, x, covars, int)) %>%
        tidyr::gather_('Yterms', 'YtermValues', y) %>%
        tidyr::gather_('Xterms', 'XtermValues', x)

    if (!is.null(id))
        prep <- dplyr::rename(prep, 'id' = "id")

    prep <- prep %>%
        dplyr::group_by_('Yterms', 'Xterms')

    if (na.rm)
        prep <- stats::na.omit(prep)

    make_blueprint(data, prepared = prep)
}

regression_formula_old <- function(specs) {
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

generate_results_old <- function(data, tool, specs, type) {

    results <- attr(data, 'specs')$prepared %>%
        dplyr::do_(.dots = tool) %>%
        dplyr::ungroup()

    # Remove in case prepared data already existed.
    attr(data, 'specs')$prepared <- NULL
    append_results(data, specs, results, type)
}

# append_results <- function(data, specs, results, type) {
#     if (!is.null(attr(data, 'specs')$results))
#         results <- dplyr::bind_rows(attr(data, 'specs')$results, results)
#
#     attr(data, 'specs')$results <- NULL
#     make_blueprint(data, results = results, type = type)
# }

construct_old <- function(data, ..., na.rm = TRUE) {
    UseMethod("construct_old", data)
}

construct_old.gee_bp <- function(data, na.rm = TRUE, ...) {

    if (!requireNamespace('geepack'))
        stop('geepack is needed for this analysis, please install it',
             call. = FALSE)

    specs <- attributes(data)$specs
    specs_integrity(data, specs)

    if (length(c(specs$vars$yvars, specs$vars$xvars)) >= 100)
        stop(
            'There are too many y and/or x variables to loop through, ',
            'please split the construction up (see vignette for more details).'
        )

    f <- function(data, specs, form) {
        # Have to add these because I think geeglm doesn't reference them
        # explicitly inside its function
        glm <- stats::glm
        model.frame <- stats::model.frame
        id <- data[['id']]

        mod <- geepack::geeglm(
            form,
            data = data,
            id = id,
            corstr = specs$corstr,
            family = specs$family
        )
        tidied <-
            broom::tidy(mod,
                        conf.int = specs$conf.int,
                        conf.level = specs$conf.level)
        nsize <- summary(mod)$clusz

        data.frame(
            tidied,
            sample.total = sum(nsize),
            sample.max = max(nsize),
            sample.min = min(nsize)
        )
    }
    form <- regression_formula_old(specs)
    tool <- lazyeval::interp(~f(., specs = specs, form = form),
                                          f = f,
                                          specs = specs,
                                          form = form)

    construction_base_old(data = data, specs = specs, tool = tool, na.rm = na.rm)
}

construct_old.glm_bp <- function(data, na.rm = TRUE, ...) {
    specs <- attributes(data)$specs
    specs_integrity(data, specs)

    f <- function(data, specs, form) {
        mod <- stats::glm(form,
                          data = data,
                          family = specs$family)
        tidied <-
            broom::tidy(mod,
                        conf.int = specs$conf.int,
                        conf.level = specs$conf.level)
        data.frame(tidied,
                   sample.size = nrow(mod$model))
    }
    form <- regression_formula_old(specs)
    tool <- lazyeval::interp(~f(., specs = specs, form = form),
                                          f = f,
                                          specs = specs,
                                          form = form)

    construction_base_old(data = data, specs = specs, tool = tool, na.rm = na.rm)
}
