#' Add analysis settings to the blueprint
#'
#' Most statistical techniques need to specify some settings for them to run.
#' This function sets those settings in the blueprint, before the statistical
#' method is used at the construction phase.
#'
#' @param blueprint The blueprint object
#' @inheritParams stats::cor
#' @inheritParams stats::glm
#' @inheritParams stats::t.test
#' @param cluster.id Variable that represents the cluster for GEE
#' @inheritParams broom::tidy.geeglm
#'
#' @return Settings for the analysis are added to the blueprint
#' @export
#'
#' @examples
#'
#' ds <- design(iris, 'gee')
#' ds <- add_settings(family = binomial('logit'), conf.int = FALSE)
#'
#' ds <- design(iris, 'cor')
#' ds <- add_settings(method = 'spearman')
#'
#' ds <- design(iris, 't.test')
#' add_settings(ds, paired = TRUE)
#' add_settings(ds)
#'
add_settings <-
    function(blueprint,
             family,
             corstr,
             cluster.id,
             conf.int,
             conf.level,
             method,
             paired,
             use) {
        UseMethod('add_settings', blueprint)
    }

#' @export
add_settings.gee_blueprint <-
    function(blueprint,
             cluster.id,
             family = gaussian('identity'),
             corstr = c('independence', 'exchangeable', 'ar1'),
             conf.int = TRUE,
             conf.level = 0.95) {
        .make_blueprint(
            family = family,
            corstr = match.arg(corstr),
            id = cluster.id,
            conf.int = conf.int,
            conf.level = conf.level,
            blueprint = blueprint
        )
    }

#' @export
add_settings.cor_blueprint <-
    function(blueprint,
             method = c('pearson', 'kendall', 'spearman'),
             use = c('complete.obs',
                     'all.obs',
                     'pairwise.complete.obs',
                     'everything',
                     'na.or.complete'),
             hclust.order = FALSE) {
        .make_blueprint(
            method = match.arg(method),
            obs.usage = match.arg(use),
            blueprint = blueprint,
            hclust.order = hclust.order
        )
    }

#' @export
add_settings.glm_blueprint <-
    function(blueprint,
             family = gaussian('identity'),
             conf.int = TRUE,
             conf.level = 0.95) {
        .make_blueprint(
            family = family,
            conf.int = conf.int,
            conf.level = conf.level,
            blueprint = blueprint
        )
    }

#' @export
add_settings.pls_blueprint <-
    function(blueprint,
             ncomp,
             scale = TRUE,
             validation = c('none', 'CV', 'LOO')) {

    }

#' @export
add_settings.t.test_blueprint <-
    function(blueprint,
             paired = FALSE) {
        .make_blueprint(
            blueprint = blueprint,
            paired = paired
        )
    }