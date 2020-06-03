#' @title
#' Build a (results from analyses) structure like a mason
#'
#' @description
#' Easily run common statistical analyses and build them into a form that can
#' easily be plotting or made into a table. Many parts of `mason` use
#' [dplyr::dplyr()] functions, which makes the analysis fast and
#' allows it to be put into a [magrittr::magrittr()] pipe chain.
#'
#' The final, [scrub()]'ed version of the analysis is in a 'tidy' format,
#' meaning it is already in a form to send to ggplot2 or created
#' into a table using the pander package or with [knitr::kable()]. It also allows
#' further processing with `dplyr` and `tidyr`.
#'
#' @details
#' One of the main goals of `mason` is to make it easy to implement other
#' analyses in a consistent syntax and structure. Like in architecture,
#' construction, and engineering, data analysis projects follow a similar
#' workflow, where there is a design phase, a construction phase, and a final
#' scrubbing/cleaning/polishing phase, with some back and forth as construction
#' continues. `mason` tries to emulate this pattern.
#'
#' @seealso For more documentation, see `vignette("mason", package = "mason")`.
#'
#' @name mason
#' @docType package
NULL

## usethis namespace: start
#' @importFrom tibble tibble
## usethis namespace: end
NULL
