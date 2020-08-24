#' This function allows you to organize the variables names of ECH with reference in 2017.
#' @family organize
#' @param data data.frame contains the ECH microdata
#' @param level (string) indicates whether the base to be labelled is of the type "household", "h", "individual", "i" or both, "hyp". Default "hyp"
#' @param year numeric reference year of the data. Available from 2011 to 2019
#' @export
#' @details
#' Disclaimer: This script is not an official INE product.
#' Aviso: El script no es un producto oficial de INE.
#' @examples
#' \donttest{
#' toy_ech_2018 <- organize_names(data = ech::toy_ech_2018, year = 2018, level = "h")
#' }

organize_names <- function(data, year, level = "hyp"){
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(level %in% c("household", "h", "individual", "i", "hyp"),
                          msg = "Check the level selected")
  assertthat::assert_that(year %in% 2011:2019,  msg = glue::glue("{year} not yet processed"))
  assertthat::assert_that(year != 2017,  msg = glue::glue("{year} is the reference year"))
  n <- ech::dic
  if(level %in% "hyp"){
    nh <- n %>%
      dplyr::filter(!duplicated(var17)) %>%
      dplyr::select(paste0("var", c(substr(year,3,4), 17))) %>%
      dplyr::filter(!. == "" & !.== " ")
    data <- data %>% dplyr::select(nh[,1])
    names(data) <- nh[,2]
  }
  if(level %in% c("hogar","h")){
    nh <- n %>%
      dplyr::filter((unidad == "H" | unidad == "G") & !duplicated(var17)) %>%
      dplyr::select(paste0("var", c(substr(year,3,4), 17))) %>%
      dplyr::filter(!. == "" & !.== " ")
    data <- data %>% dplyr::select(nh[,1])
    names(data) <- nh[,2]
  }
  if(level %in% c("individual", "i")){
    nh <- n %>%
      dplyr::filter((unidad == "P" | unidad == "G") & !duplicated(var17)) %>%
      dplyr::select(paste0("var", c(substr(year,3,4), 17))) %>%
      dplyr::filter(!. == "" & !.== " ")
    data <- data %>% dplyr::select(nh[,1])
    names(data) <- nh[,2]
  }
  return(data)
}


# #' check_spelling_dptouy
# #'
# #' @param x a column with names of "Departamentos" of Uruguay
# #' @param upper logic
# #'
# #' @return charcater vector with correct names of Departamentos
# #' @export
# #' @examples
# #' \donttest{
# #' d <- check_spelling_dptouy(ech::toy_ech_2018$dpto)
# #' }
# check_spelling_dptouy <- function(x, upper = T){
#   x <- stringr::str_to_title(x) %>%
#     gsub("Paysandu", "Paysandú", .) %>%
#     gsub("Rio Negro", "Río Negro", .) %>%
#     gsub("San Jose", "San José", .) %>%
#     gsub("Tacuarembo", "Tacuarembó", .) %>%
#     gsub("Treinta Y Tres", "Treinta y Tres", .)
#     if (upper == T) x <- upper(x)
#     x
# }

# #' to_ascii
# #'
# #' @param x a column
# #' @param upper logic
# #'
# #' @importFrom stringr str_replace_all
# #' @return
# #' @export
# #' @examples
# #' \donttest{
# #' d <- lapply(dic, to_ascii)
# #' }
# to_ascii <- function(x, upper = T ){
#   x <- x %>% as.character() %>%
#     toupper() %>%
#     stringr::str_replace_all("Ñ", "NI") %>%
#     stringr::str_replace_all("Ó", "O") %>%
#     stringr::str_replace_all("Á", "A") %>%
#     stringr::str_replace_all("É", "E") %>%
#     stringr::str_replace_all("Í", "I") %>%
#     stringr::str_replace_all("Ú", "U")
#   if (!upper == T) x <- tolower(x)
#   x
# }


# dic <- read.csv2("data/diccionario.csv") #%>%
# dic[] <- lapply(dic, to_ascii)
# save(dic, file = "data/dic.rda")

# ech <- get_microdata(2012)
# ech1 <- organize_names(ech,2012, "hyp")
# used_vars <- c("c2", "c3", "c4", "c5_1", "c5_2", "c5_3", "c5_4", "c5_5", "c5_6", "c5_7", "c5_8", "c5_9", "c5_10","c5_11", "c5_12",
#                "dpto", "d8_1", "d9", "d10", "d11", "d12", "d13", "d16", "d18", "d19","d14","d15","d21_1","d21_2","d21_3","d260",
#                "estred13",
#                "e26", "e27", "e30","e49", "e51_2","e51_3","e51_4","e51_5", "e51_6","e51_7","e51_7_1","e51_8","e51_9","e51_10","e51_11",
#                "e193", "e197", "e197_1", "e201", "e212", "e215", "e218", "e221", "e224", "f72_2", "f85",
#                "g126_1", "g126_2", "g126_3", "g126_4", "g126_5", "g126_6", "g126_7", "g126_8","g127_3","g128_1","g129_2", "g130_1",
#                "g131_1", "g133_1", "g133_2", "g134_1", "g134_2", "g134_3", "g134_4", "g134_5", "g134_6", "g134_7", "g134_8",
#                "g135_3", "g136_1", "g137_2", "g138_1","g139_1","g141_1","g141_2","g142","g144_1","g144_2_1","g144_2_3","g144_2_4","g144_2_5",
#                "numero", "mes", "ht11", "ht13", "ht19", "pobre06", "pobpcoac", "pesoano","pt4", "region_4")
#
# used_vars[!used_vars %in% names(ech1)]

