## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(readODS)

## ----write_ods1---------------------------------------------------------------
write_ods(PlantGrowth, "plant.ods")

## ----read_ods1----------------------------------------------------------------
read_ods("plant.ods")

## ----write_ods_append---------------------------------------------------------
write_ods(mtcars, "plant.ods", sheet = "mtcars_ods", append = TRUE)

## ----read_ods_mtcars----------------------------------------------------------
read_ods("plant.ods", sheet = "mtcars_ods")

## ----read_ods_mtcars2---------------------------------------------------------
read_ods("plant.ods", sheet = 2)

## ----write_ods_update---------------------------------------------------------
write_ods(mtcars, "plant.ods", sheet = "mtcars_ods", update = TRUE, row_names = TRUE)

## ----read_ods_mtcars3---------------------------------------------------------
read_ods("plant.ods", sheet = "mtcars_ods")

## ----read_ods_mtcars_range----------------------------------------------------
read_ods("plant.ods", sheet = "mtcars_ods", range = "A1:C10")

## ----append_error, error = TRUE-----------------------------------------------
write_ods(iris, "plant.ods", sheet = "mtcars_ods", append = TRUE)

## ----update_error, error = TRUE-----------------------------------------------
write_ods(iris, "plant.ods", sheet = "iris", update = TRUE)

## ---- list_ods_sheets---------------------------------------------------------
list_ods_sheets("plant.ods")

## ---- echo = FALSE, message = FALSE-------------------------------------------
unlink("plant.ods")

