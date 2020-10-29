## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 400
)

## ----setup--------------------------------------------------------------------
library(ech)

## ----eval = FALSE-------------------------------------------------------------
#  df <- age_groups(df)
#  df <- enrolled_school(df)
#  df <- years_of_schooling(df)
#  
#  df <- level_education(df)

## ----eval = FALSE-------------------------------------------------------------
#  get_estimation_mean(df, variable = "school_enrollment", domain = "age_groups == 3", level = "i", ids = "upm", estrato = "estrato")

## ----eval = FALSE-------------------------------------------------------------
#  get_estimation_mean(df, variable = "years_schooling", by.x = "e26", domain = "e27 > 24", level = "i", ids = "upm", estrato = "estrato")
#  

## ----eval = FALSE-------------------------------------------------------------
#  get_estimation_mean(df, variable = "level_education", domain = "e27 > 24", level = "i")

## ----eval = FALSE-------------------------------------------------------------
#  df <- level_completion(df, n = 2)
#  get_estimation_mean(df, variable = "primary_completion", domain = "e27 > 15", level = "i")
#  get_estimation_mean(df, variable = "lower_secondary_completion", domain = "e27 > 15", level = "i")
#  get_estimation_mean(df, variable = "upper_secondary_completion", domain = "e27 > 17", level = "i")
#  get_estimation_mean(df, variable = "tertiary_completion", domain = "e27 > 24", level = "i")
#  df <- level_completion(df, n = 4)
#  get_estimation_mean(df, variable = "tertiary_completion", domain = "e27 > 24", level = "i")

