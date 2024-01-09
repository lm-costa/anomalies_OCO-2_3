# my functions
source('fun/functions.R')

#OCO-2
####
files_names <- list.files("data-raw/OCO-2/",
                          pattern = "nc",
                          full.names = TRUE)

#### Extracting

xco2 <- purrr::map_df(files_names,my_ncdf4_extractor) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time)
  ) |>
  dplyr::filter(
    quality_flag==0
  )

dplyr::glimpse(xco2)

readr::write_rds(xco2,'data/oco2_full.rds')



#OCO-3
####
files_names <- list.files("data-raw/OCO-3/",
                          pattern = "nc",
                          full.names = TRUE)

#### Extracting

xco2 <- purrr::map_df(files_names,my_ncdf4_extractor) |>
  dplyr::mutate(
    date = as.Date.POSIXct(time)
  ) |>
  dplyr::filter(
    quality_flag==0
  )

dplyr::glimpse(xco2)

readr::write_rds(xco2,'data/oco3_full.rds')
