source('fun/def_pol.R')


br <- geobr::read_country(showProgress = FALSE)
regiao <- geobr::read_region(showProgress = FALSE)

pol_br <- br$geom |> purrr::pluck(1) |> as.matrix()
pol_nordeste <- regiao$geom |> purrr::pluck(2) |> as.matrix()

# correcting poligions

pol_br <- pol_br[pol_br[,1]<=-34,]
pol_br <- pol_br[!((pol_br[,1]>=-38.8 & pol_br[,1]<=-38.6) &
                     (pol_br[,2]>= -19 & pol_br[,2]<= -16)),]

pol_nordeste <- pol_nordeste[pol_nordeste[,1]<=-34,]
pol_nordeste <- pol_nordeste[!((pol_nordeste[,1]>=-38.7 &
                                  pol_nordeste[,1]<=-38.6) &
                                 pol_nordeste[,2]<= -15),]
#### Filtering OCO-2
oco2 <- readr::read_rds('data/oco2_full.rds')

oco2 <- oco2 |>
  dplyr::mutate(
    flag_br = def_pol(lon, lat, pol_br),
    flag_nordeste = def_pol(lon, lat, pol_nordeste)
  )|>
  dplyr::filter(flag_br|flag_nordeste)

#### Filtering OCO-3

oco3 <- readr::read_rds('data/oco3_full.rds')

oco3 <- oco3 |>
  dplyr::mutate(
    flag_br = def_pol(lon, lat, pol_br),
    flag_nordeste = def_pol(lon, lat, pol_nordeste)
  )|>
  dplyr::filter(flag_br|flag_nordeste)


### export the data set for arc gis and add the biome column,
### than put the new table in 'data' folder
### You can Clean the environment 
### The data with columns added is in PROCESSED DATA folder
