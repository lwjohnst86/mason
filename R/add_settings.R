#' Add analysis settings to the blueprint
#'
#' Most statistical techniques need to specify some settings for them to run.
#' This function sets those settings in the blueprint, before the statistical
#' method is used at the construction phase.
#'
#' @param blueprint The blueprint object
#' @param hclust.order Whether to order the correlation data based on the
#'   \code{\link[stats]{hclust}} algorithm.
#' @param cluster.id Variable that represents the cluster for GEE.
#' @inheritParams stats::glm
#' @inheritParams broom::tidy.geeglm
#' @inheritParams stats::cor
#' @inheritParams stats::t.test
#'
#' @return Settings for the analysis are added to the blueprint
#' @export
#'
#' @examples
#'
#' design(iris, 'gee') %>%
#'  add_settings('Species', family = binomial('logit'), conf.int = FALSE)
#'
#' ds <- design(iris, 'cor')
#' ds <- add_settings(ds, method = 'spearman')
#'
#' ds <- design(iris, 't.test')
#' add_settings(ds, paired = TRUE)
#' add_settings(ds)
#'
add_settings <-
    function(data, ...) {
        UseMethod('add_settings', data)
    }

#' @export
add_settings.gee_bp <-
    function(data,
             cluster.id,
             family,
             corstr = c('independence', 'exchangeable', 'ar1'),
             conf.int = TRUE,
             conf.level = 0.95, ...) {

        if (missing(family))
            family <- gaussian()

        if (missing(cluster.id)) {
            stop('Please supply an ID for the cluster.', call. = FALSE)
        } else {
            vars_exist(data, cluster.id)
        }

        make_blueprint(data,
            family = family,
            corstr = match.arg(corstr),
            id = cluster.id,
            conf.int = conf.int,
            conf.level = conf.level
        )
    }

#' @export
add_settings.cor_bp <-
    function(data,
             method = c('pearson', 'kendall', 'spearman'),
             use = c('complete.obs',
                     'all.obs',
                     'pairwise.complete.obs',
                     'everything',
                     'na.or.complete'),
             hclust.order = FALSE,
             ...) {

        if (hclust.order)
            message('Note: hclust.order arg is still experimental')

        make_blueprint(data,
            method = match.arg(method),
            obs.usage = match.arg(use),
            hclust.order = hclust.order
        )
    }

#' @export
add_settings.glm_bp <-
    function(data, family, conf.int = TRUE, conf.level = 0.95, ...) {

        if (missing(family))
            family <- gaussian()
        make_blueprint(data,
            family = family,
            conf.int = conf.int,
            conf.level = conf.level
        )
    }

#' @export
add_settings.pls_bp <-
    function(data,
             ncomp,
             scale = TRUE,
             validation = c('none', 'CV', 'LOO'),
             ...) {

    }

#' @export
add_settings.t.test_bp <-
    function(data, paired = FALSE, ...) {
        make_blueprint(data,
            paired = paired
        )
    }