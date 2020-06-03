construction_base <- function(data, specs, tool, formulas, na.rm = FALSE) {
    data_prep(
        data = data,
        y = specs$vars$yvars,
        x = specs$vars$xvars,
        covars = specs$vars$covariates,
        int = specs$vars$interaction,
        id = specs$id,
        na.rm = na.rm
    ) %>%
        generate_results(
            tool = tool,
            specs = specs,
            formulas = formulas,
            type = grep('bp', class(data), value = TRUE)
        )
}

data_prep <- function(data, y, x, covars = NULL,
                      int = NULL, id = NULL, na.rm = TRUE) {

    prep <- dplyr::select(data, tidyselect::all_of(c(id, y, x, covars, int)))

    if (!is.null(id))
        prep <- dplyr::rename(prep, 'id' = id)

    if (na.rm)
        prep <- stats::na.omit(prep)

    make_blueprint(data, prepared = prep)
}

regression_formula <- function(specs) {
    vars <- specs$vars

    variable_list <- expand.grid(
        y = vars$yvars,
        x = vars$xvars,
        stringsAsFactors = FALSE,
        KEEP.OUT.ATTRS = FALSE
    )


    formulas <-
        purrr::map2(variable_list$x,
                    variable_list$y,
                    ~ {
                        interactions <- NULL
                        if (length(vars$interaction))
                            interactions <- paste0(.x, ":", vars$interaction)

                        stats::reformulate(c(.x, vars$covariates, interactions),
                                           response = .y)
                    })

    list(variables = variable_list,
         formulas = formulas)
}


generate_results <- function(data, tool, specs, formulas, type) {

    results <- purrr::map_dfr(formulas, ~ tool(
        data = attr(data, 'specs')$prepared,
        specs = specs,
        form = .x
    ))

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

