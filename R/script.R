
#### Download data from Url

filename <- dir('url/Mission_name', pattern = 'Mission_name')
urls <- read.table(paste0('url/Mission_name',filename))
n_urls <- nrow(urls)
n_split <- length(stringr::str_split(urls[1,1],"/",simplify = TRUE))


files_nc <- stringr::str_split_fixed(urls[,1],"/",n=Inf)[,n_split]


for (i in 1:n_urls){
  repeat{
    dw <- try(download.file(urls[i,1],
                            paste0('data-raw/Mission_name',files_nc[i]),
                            method = 'wget',
                            extra = c('--user=xxxxx --password xxxxx')
    )
    )
    if (!(inherits(dw,"try-error")))
      break
  }
}



###### data extraction

file_names <- list.files('data-raw/Mission_name', pattern = 'nc')

for(i in 1:length(file_names)){
  if(i==1){
    df <- ncdf4::nc_open(paste0('data-raw/Mission_name',file_names[i]))
    if (df$ndims == 0){

    }else{
      xco2 <- data.frame(
        'lon' = ncdf4::ncvar_get(df,varid='longitude'),
        'lat' = ncdf4::ncvar_get(df,varid='latitude'),
        'time' = ncdf4::ncvar_get(df,varid='time'),
        'xco2' = ncdf4::ncvar_get(df,varid='xco2'),
        'uncertanty' = ncdf4::ncvar_get(df,varid='xco2_uncertainty'),
        'quality_flag' = ncdf4::ncvar_get(df,varid='xco2_quality_flag')
      ) |>
        dplyr::filter(lon < -35 & lat < 5) |> # brazil region
        dplyr::filter(quality_flag <= 'value 1 or 0')
    }
    ncdf4::nc_close(df)
  }else{
    df_a <- ncdf4::nc_open(paste0('data-raw/Mission_name',file_names[i]))
    if (df_a$ndims == 0){
      }else{
        xco2_a <- data.frame(
          'lon' = ncdf4::ncvar_get(df_a,varid='longitude'),
          'lat' = ncdf4::ncvar_get(df_a,varid='latitude'),
          'time' = ncdf4::ncvar_get(df_a,varid='time'),
          'xco2' = ncdf4::ncvar_get(df_a,varid='xco2'),
          'uncertanty' = ncdf4::ncvar_get(df_a,varid='xco2_uncertainty'),
          'quality_flag' = ncdf4::ncvar_get(df_a,varid='xco2_quality_flag')
        )|>
          dplyr::filter(lon < -35 & lat < 5)|> # brazil region
          dplyr::filter(quality_flag <= 'value 1 or 0')}
    ncdf4::nc_close(df_a)
    xco2 <- rbind(xco2,xco2_a)
  }
}

xco2 <- xco2 |>
  dplyr::mutate(
    date = as.Date.POSIXct(time)) |>
  dplyr::filter(
    lubridate::year(date)>2019 & lubridate::year(date)<2022
  )

##### filters
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


xco2 <- xco2 |>
  dplyr::mutate(
    flag_br = def_pol(lon, lat, pol_br),
    flag_nordeste = def_pol(lon, lat, pol_nordeste)
  )|>
  dplyr::filter(flag_br|flag_nordeste)


### export the data set for arc gis and add the biome column,
### than put the new table in 'data' folder
### You can Clean the environment

source('fun/mapa.R')
source('fun/out.R')

oco2 <- read.csv('data/mission/data_bioma.csv') |>
  dplyr::mutate(
    date=lubridate::as_datetime(time_), ### use this if date column have been altered
    date=lubridate::as_date(date)
  ) |>
  dplyr::filter(lubridate::year(date)>2019 & lubridate::year(date)<2022) |>
  dplyr::select(-date_)

oco3 <- read.csv('data/mission/data_bioma.csv') |>
  dplyr::mutate(
    date=lubridate::as_datetime(time_), ### use this if date column have been altered
    date=lubridate::as_date(date)
  ) |>
  dplyr::filter(lubridate::year(date)>2019 & lubridate::year(date)<2022)

##### Coverage

br |>
  ggplot2::ggplot()+
  ggplot2::geom_sf(fill="white", color="black",
                   size=.15, show.legend = FALSE)+
  ggplot2::geom_point(data = oco2 |>
                        dplyr::mutate(
                          mes=lubridate::month(date),
                          year=lubridate::year(date)
                        ) |>
                        dplyr::filter(mes==3 & year == 2020),
                      ggplot2::aes(x=lon,y=lat,color='OCO-2'),
                      alpha=.2)+
  ggplot2::geom_point(data = oco3 |>
                        dplyr::mutate(
                          mes=lubridate::month(date),
                          year=lubridate::year(date)
                        ) |>
                        dplyr::filter(mes==3 & year == 2020),
                      ggplot2::aes(x=lon,y=lat,color='OCO-3'),
                      alpha=.2)+
  ggplot2::scale_color_manual(name='Mission',
                              breaks=c('OCO-2','OCO-3'),
                              values=c('OCO-2'='red', 'OCO-3'='dark blue'))+
  ggplot2::theme(axis.text = ggplot2::element_text(color='black'))+
  tema_mapa()



### Analyzing uncertanty

oco2 |>
  dplyr::group_by(date) |>
  dplyr::summarise(uncertanty=mean(uncertanty)) |>
  ggplot2::ggplot(ggplot2::aes(x=date,y=uncertanty))+
  ggplot2::geom_area(ggplot2::aes(color='OCO-2', fill='OCO-2'),alpha=.2)+
  ggplot2::geom_area(data=oco3 |>
                       dplyr::group_by(date) |>
                       dplyr::summarise(uncertanty=mean(uncertanty)),
                     ggplot2::aes(color='OCO-3',fill='OCO-3'),
                     fill='dark blue',alpha=.2)+
  ggplot2::ylab(expression('Xco'[2]~' Uncertanty (ppm)'))+
  ggplot2::ylim(0,1.5)+
  ggplot2::scale_color_manual(name='Mission',
                              breaks=c('OCO-2','OCO-3'),
                              values=c('OCO-2'='red', 'OCO-3'='dark blue'))+
  ggplot2::scale_fill_manual(name='Mission',
                             breaks=c('OCO-2','OCO-3'),
                             values=c('OCO-2'='red', 'OCO-3'='dark blue'))+
  ggplot2::theme(axis.text = ggplot2::element_text(color='black'))


#### Combining OCO-2 and OCO-3

xco2 <- rbind(oco2,oco3)

xco2 <- xco2 |>
  dplyr::filter(
    xco2 > remove_outlier(xco2)[1]&
      xco2<remove_outlier(xco2)[2]
  ) |>
  dplyr::group_by(date) |>
  dplyr::mutate(
    Anomaly = xco2-median(xco2)
  )

xco2 <- xco2 |>
  dplyr::mutate(
    year=lubridate::year(date),
    Biome = dplyr::case_when(
      RASTERVALU==3~'AMZ',
      RASTERVALU==6~'AF',
      RASTERVALU==7~'CERR',
      RASTERVALU==8~'CAAT',
      RASTERVALU==9~'PNT',
      RASTERVALU==10~'PMP'
    ),
    mes=lubridate::month(date),
    Period = dplyr::case_when(
      mes==1~'Wet' ,
      mes==2~ 'Wet',
      mes==3~'Wet' ,
      mes==4~ 'Dry',
      mes==5~ 'Dry',
      mes==6~ 'Dry',
      mes==7~ 'Dry',
      mes==8~ 'Dry',
      mes==9~ 'Dry',
      mes==10~ 'Wet',
      mes==11~ 'Wet',
      mes==12~'Wet'
    ),

  ) |>
  na.omit()


xco2 |>
  dplyr::mutate(
    Season=stringr::str_c(
      lubridate::year(date),Period,sep='-'
    )
  ) |>
  ggplot2::ggplot(ggplot2::aes(x=Anomaly, y=Biome,col=Biome, fill=Biome))+
  ggridges::geom_density_ridges(alpha=.4)+
  ggplot2::facet_wrap(~Season,scales = 'fixed')+
  ggplot2::geom_vline(xintercept = 0, linetype='dashed')+
  ggplot2::scale_color_viridis_d()+
  ggplot2::scale_fill_viridis_d()


##### extract fire data from .rar inside 'data' folder

frp <- read.csv('data/frp.csv') |>
  dplyr::mutate(
    lon=round(lon,2),
    lat=round(lat,2),
    year=lubridate::year(date)
  ) |>
  dplyr::select(lon,lat,year,Period,Biome,frp) |>
  dplyr::filter(
    frp>remove_outlier(frp)[1]&
      frp<remove_outlier(frp)[2]
  )

#####



df <- xco2 |>
  dplyr::mutate(
    lon=round(lon,2),
    lat=round(lat,2)
  ) |>
  dplyr::select(lon,lat,year,Period,Biome, Anomaly) |>
  dplyr::left_join(frp) |>
  na.omit()

df |>
  dplyr::mutate(
    lon=round(lon,1),
    lat=round(lat,1),
    Season=stringr::str_c(
      year,Period,sep='-'
    )) |>
  dplyr::group_by(lon,lat,Season,Biome) |>
  dplyr::summarise(
    ff=dplyr::n(),
    Anomaly=max(Anomaly)
  ) |>
  dplyr::filter(Biome != 'PMP') |>
  ggplot2::ggplot(ggplot2::aes(x=ff,y=Anomaly,col=Season))+
  ggplot2::geom_point()+
  ggplot2::geom_smooth(se=FALSE,method='lm')+
  ggplot2::facet_wrap(~Biome,scales = 'fixed')+
  ggpubr::stat_cor(label.y.npc = 'bottom',label.x =70)+
  ggpubr::stat_regline_equation(label.y.npc = 'bottom',label.x = 30)+
  ggplot2::xlab(
    'Fire Foci count'
  )+
  ggplot2::ylab('Anomaly (ppm)')+
  ggplot2::theme_bw()+
  ggplot2::theme(axis.text = ggplot2::element_text(color='black'))
