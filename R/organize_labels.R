#'Funcion para etiquetar las bases de la ECH con referencia al 2017.
#'
#'@param df data frame contains the ECH microdata
#'@param level (string) indicates whether the base to be labelled is of the type "household", "h", "individual", "i" or both, "hyp"
#'@param year numeric reference year of the df. Available from 2006 to 2019
#'@details
#'Disclaimer: El script no es un producto oficial de INE.
#'@examples
#'df <- organize_labels(df = toy_ech_2018, year = 2018, level = "h")
#'@export


organize_labels <- function(df, year, level = "individual"){
  assertthat::assert_that(is.data.frame(df))
  assertthat::assert_that(level %in% c("household", "h", "individual", "i", "hyp"))
  assertthat::assert_that(year %in% 2006:2018)
  n <- ech::dic
  if(level %in% "hyp"){
    nh <- n %>%
      dplyr::filter(!duplicated(var17)) %>%
      dplyr::select(paste0("var", c(substr(year,3,4), 17))) %>%
      dplyr::filter(!. == "")
    df <- df %>% dplyr::select(nh[,1])
    names(df) <- nh[,2]
  }
  if(level %in% c("hogar","h")){
    nh <- n %>%
      dplyr::filter(unidad == "H" & !duplicated(var17)) %>%
      dplyr::select(paste0("var", c(substr(year,3,4), 17))) %>%
      dplyr::filter(!. == "")
    df <- df %>% dplyr::select(nh[,1])
    names(df) <- nh[,2]
  }
  if(level %in% c("individual", "i")){
    nh <- n %>%
      dplyr::filter(unidad == "P" & !duplicated(var17)) %>%
      dplyr::select(paste0("var", c(substr(year,3,4), 17))) %>%
      dplyr::filter(!. == "")
    df <- df %>% dplyr::select(nh[,1])
    names(df) <- nh[,2]
  }
  df
}



# to_ascii <- function(x, upper = T ){
#   x <- x %>% as.character() %>%
#     toupper() %>%
#     str_replace_all("Ñ", "NI") %>%
#     str_replace_all("Ó", "O") %>%
#     str_replace_all("Á", "A") %>%
#     str_replace_all("É", "E") %>%
#     str_replace_all("Í", "I") %>%
#     str_replace_all("Ú", "U")
#   if (!upper == T) x <- tolower(x)
#   x
# }
#
# dic <- read.csv2("data/diccionario.csv") #%>%
# dic[] <- lapply(dic, to_ascii)
# save(dic, file = "data/dic.rda")
