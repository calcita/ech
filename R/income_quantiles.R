#' income_quantiles
#'
#' @description Household Income Quantiles
#'
#' @param data data.frame
#' @param quantile cuantiles: quintiles (5) o deciles (10)
#' @importFrom statar xtile
#' @importFrom dplyr mutate
#' @importFrom magrittr %<>%
#' @export
#' @examples
#' income_quantiles()
income_quantiles <- function(data = ech::toy_ech_2017_income,
                             quantile = 5) {

  weights = data$pesoano
  ht11_per_capita_deflate = data$ht11
  # Defino el disenio
  # d <- set_design()
  if (quantile == 5){
    ## quintiles
    # aux <- d %>% summarise(quintiles = srvyr::survey_quantile(ht11_per_capita_deflate, quantile = seq(0.2,1,0.2))) %>%
    #  select(1:5)
    # q <- as.numeric(c(aux))
    # df %<>% mutate(namecol = ifelse(ht11_svl_per_capita_deflate <= q[1], 1,
    #                                 ifelse(ht11_svl_per_capita_deflate <= q[2], 2,
    #                                        ifelse(ht11_svl_per_capita_deflate <= q[3], 3,
    #                                               ifelse(ht11_svl_per_capita_deflate <= q[4], 4,
    #                                                      ifelse(ht11_svl_per_capita_deflate <= q[5], 5, "error")))))
    data %<>% dplyr::mutate(quintil = statar::xtile(ht11_per_capita_deflate, n = 5, wt = weights))

  }

  if (quantile == 10){
    ## deciles
    data %<>% dplyr::mutate(decil = statar::xtile(ht11_per_capita_deflate, n = 10, wt = weights))
  }

}
