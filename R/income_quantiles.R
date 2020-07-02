#' income_quantiles
#'
#' @description Household Income Quantiles
#'
#' @param data data.frame
#' @param quantile cuantiles: quintiles (5) o deciles (10)
#' @param weights ponderation variable
#' @param income Name of the income variables. Default: "ht11_per_capita_deflate"
#' @importFrom statar xtile
#' @importFrom dplyr mutate pull
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' \donttest{
#' income_quantiles()
#' }

income_quantiles <- function(data = ech::toy_ech_2017_income,
                             quantile = 5,
                             weights = "pesoano",
                             income = "ht11_per_capita_deflate") {

  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(weights %in% names(data))
  assertthat::assert_that(quantile %in% c(5, 10))
  assertthat::assert_that(income  %in% names(data), msg = "Sorry... :( \n Income parameter is not estimated, please use income_constant_prices() to obtain the variable.")

  weights = pull(data[,weights])

  if (quantile == 5) {
    ## quintiles
    data %<>% dplyr::mutate(quintil = statar::xtile(income, n = 5, wt = weights))
  }  else {
    ## deciles
    data %<>% dplyr::mutate(decil = statar::xtile(income, n = 10, wt = weights))
  }

}
