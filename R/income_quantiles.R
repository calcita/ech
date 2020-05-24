#' income_quantiles
#'
#' @description Household Income Quantiles
#'
#' @param data data.frame
#' @param quantile cuantiles: quintiles (5) o deciles (10)
#' @param weights ponderation variable
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

  weights = pull(data[,weights])

  if (quantile == 5) {
    ## quintiles
    data %<>% dplyr::mutate(quintil = statar::xtile(income, n = 5, wt = weights))
  }

  if (quantile == 10) {
    ## deciles
    data %<>% dplyr::mutate(decil = statar::xtile(income, n = 10, wt = weights))
  }

}
