.check_covars_in_xyvars <- function(blueprint) {
    if (!is.null(blueprint$covariates))
        if (any(blueprint$covariates %in% c(blueprint$yvars, blueprint$xvars)))
            stop('A covariate is also listed as a yvar or xvar. It should be in only one or the other.')
}

.check_int_in_covars <- function(blueprint) {
    if (!is.null(blueprint$int))
        if (!blueprint$int %in% blueprint$covariates)
            stop('Please include ', blueprint$int, ' in the covars as well.',
                 call. = FALSE)
}

.check_vars_in_data <-
    function(blueprint,
             parts = c('yvars', 'xvars', 'covariates', 'interaction')) {
        vars.want <- unlist(blueprint[parts])
        vars.have <- names(blueprint$data)
        index <- vars.want %in% vars.have
        if (!any(index)) {
            vars <- paste(vars.want[which(!vars.want %in% vars.have)], separate = ', ')
            stop(
                'The variables ',
                vars,
                ' do not exist in the dataset.',
                call. = FALSE
            )
        }
    }

.check_xyvars_same_type <-
    function(blueprint, vars = c('yvars', 'xvars')) {

        for (ind in vars) {
            var.names <- blueprint[[ind]]
            if (is.null(var.names))
                next
            data.types <- unique(sapply(blueprint$data[var.names], class))
            var.names <- ifelse(length(var.names) > 4,
                                paste0(paste(var.names[1:4], collapse = ', '), ', etc'),
                                paste(var.names, collapse = ', '))
            if (length(data.types) != 1)
                stop(
                    'The variables ',
                    var.names,
                    ' are a mix of data types (',
                    paste(data.types, collapse = ', '),
                    '). Use only one type in add_variables. ',
                    'Other types can be added after running construct_analysis',
                    ' (see the vignette for more information).',
                    call. = FALSE
                )
        }
    }
