#' income_quintiles
#'
#' @description Household Income Quantiles
#'
#' @param data
#' @param ht11_per_capita_deflate
#' @param design
#' @param quantile
#' @param namecol
#' @importFrom statar xtile
#' @importFrom dplyr mutate
#' @importFrom magrittr %<>%
#' @return
#' @export
#'
#' @examples
income_quintiles <- function(data = df,
                             ht11_per_capita_deflate = ht11_per_capita_deflate,
                             design = d,
                             quantile = 5,
                             weights = pesoano) {

  # Defino el disenio
  # d <- set_design()
  if (quintil == 5){
    ## quintiles
    # aux <- d %>% summarise(quintiles = srvyr::survey_quantile(ht11_per_capita_deflate, quantile = seq(0.2,1,0.2))) %>%
    #  select(1:5)
    # q <- as.numeric(c(aux))
    # df %<>% mutate(namecol = ifelse(ht11_svl_per_capita_deflate <= q[1], 1,
    #                                 ifelse(ht11_svl_per_capita_deflate <= q[2], 2,
    #                                        ifelse(ht11_svl_per_capita_deflate <= q[3], 3,
    #                                               ifelse(ht11_svl_per_capita_deflate <= q[4], 4,
    #                                                      ifelse(ht11_svl_per_capita_deflate <= q[5], 5, "error")))))
    df %<>% dplyr::mutate(quantile = statar::xtile(ht11_per_capita_deflate, n = 5, wt = weights))

  }

  if (quintil == 10){
    ## deciles
    df %<>% dplyr::mutate(quantile = statar::xtile(ht11_per_capita_deflate, n = 10, wt = weights))
  }

}
