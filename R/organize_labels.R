#'Funcion para etiquetar las bases de la ECH con referencia al 2017.
#'
#'@param data data frame contains the ECH microdata
#'@param level (string) indicates whether the base to be labelled is of the type "household", "h", "individual", "i" or both, "hyp"
#'@param year numeric reference year of the data. Available from 2006 to 2019
#'@details
#'Disclaimer: El script no es un producto oficial de INE.
#'@examples
#'df <- organize_labels(data = ech::toy_ech_2018, year = 2018, level = "h")
#'@export


organize_labels <- function(data, year, level = "individual"){
  assertthat::assert_that(is.data.frame(data))
  assertthat::assert_that(level %in% c("household", "h", "individual", "i", "hyp"),
                          msg = "Check the level selected")
  assertthat::assert_that(year %in% 2006:2018, msg = "Your year is not already process")
  n <- ech::dic %>% filter(!var17 == "pt4")
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
  data
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
