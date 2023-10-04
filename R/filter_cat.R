#' Select variables based on categories
#'
#' All variables belonging to the same category are selected from a dataset
#' using the `%get%` infix operator. Categories are stored in the
#' "var_data_category" attribute, if it exists. Use `show_categories()` to see
#' all available variable categories.
#'
#' @name filter_cat
#' @param d dataset from data packages
#' @param var.cat variable category
#' @author Samuel Leung, Derek Chiu
#' @export
`%get%` <- function(d, var.cat) {
  vars.to.get <- vapply(d, function(x) {
    x.cat <- attr(x, ATTR_VAR_DATA_CAT)
    !is.null(x.cat) && x.cat %in% var.cat
  }, FUN.VALUE = logical(1))
  d[which(vars.to.get)]
}

#' @name filter_cat
#' @export
show_categories <- function(d) {
  unique(unlist(sapply(d, attr, ATTR_VAR_DATA_CAT)))
}
