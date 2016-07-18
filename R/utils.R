specs_integrity <- function(data, specs, stat = NULL) {
    vars <- specs$vars

    if (any(vars$xvars %in% vars$yvars))
        stop('Oops, you have one or more variables that are the same in',
             ' both xvars and yvars. Please have the xvars and yvars be completely',
             ' unique.', call. = FALSE)

    if (is.null(vars$xvars)) {
        if (is.null(vars$yvars) & stat == 'cor')
            stop('Please include at least x variables.', call. = FALSE)
        if (is.null(vars$xvars) | is.null(vars$yvars))
            stop('Please include y and x variables.', call. = FALSE)
    }

    if (!is.null(vars$covariates)) {
        if (any(vars$covariates %in% c(vars$yvars, vars$xvars))) {
            stop(
                'A covariate is also listed as a yvar or xvar. ',
                'It should be in only one or the other.',
                call. = FALSE
            )
        }
        if (!is.null(vars$interaction)) {
            if (length(vars$interaction) > 1)
                stop('Currently only one interaction can be added at a time.')
            if (!vars$interaction %in% vars$covariates)
                stop('Please include ',
                     vars$interaction,
                     ' in the covariates as well.',
                     call. = FALSE)
        }
    }
}

vars_exist <- function(data, vars) {
    vars.want <- vars
    vars.have <- names(data)
    index <- vars.want %in% vars.have
    if (!any(index)) {
        vars <-
            paste(vars.want[which(!vars.want %in% vars.have)], separate = ', ')
        stop('The variables ',
             vars,
             ' do not exist in the dataset.',
             call. = FALSE)
    }
}

#' @export
print.bp <- function(x, ...) {
    specs <- attributes(x)$specs
    if (is.null(specs$results)) {
        cat("# Analysis for", specs$stat, "is still under construction.",
            "\n# Showing data right now:\n")
        obj <- dplyr::tbl_df(unclass(x))
        print(obj, n = 6)
    } else if (!is.null(specs$results)) {
        cat(
            '# Analysis for', specs$stat, 'constructed but has not been scrubbed.',
            '\n# Here is a peek at the results:\n'
        )
        obj <- dplyr::tbl_df(attr(x, 'specs')$results)
        print(obj, n = 6)
    } else {
        warning('Nothing to show yet, is something wrong maybe?')
    }
    invisible(x)
}

#' @importFrom magrittr "%>%"
#' @export
magrittr::`%>%`