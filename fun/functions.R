my_ncdf4_extractor <- function(ncdf4_file){
  df <- ncdf4::nc_open(ncdf4_file)
  if(df$ndims!=0){
    dft <- data.frame(
      'lon' = ncdf4::ncvar_get(df,varid='longitude'),
      'lat' = ncdf4::ncvar_get(df,varid='latitude'),
      'time' = ncdf4::ncvar_get(df,varid='time'),
      'xco2' = ncdf4::ncvar_get(df,varid='xco2'),
      'uncertanty' = ncdf4::ncvar_get(df,varid='xco2_uncertainty'),
      'quality_flag' = ncdf4::ncvar_get(df,varid='xco2_quality_flag')
    ) |>
      dplyr::filter(lon < -35 & lon >-75 & lat < 5 & lat >-35)|>
      tibble::as_tibble()
  }
  ncdf4::nc_close(df)
  return(dft)
}


my_ncdf4_download <- function(url_unique,
                              user="input your user",
                              password="input your password",
                              mission='OCO-2'){
  if(is.character(user)==TRUE & is.character(password)==TRUE){
    n_split <- length(
      stringr::str_split(url_unique,
                         "/",
                         simplify=TRUE))
    filenames_nc <- stringr::str_split(url_unique,
                                       "/",
                                       simplify = TRUE)[,n_split]
    repeat{
      dw <- try(download.file(url_unique,
                              paste0("data-raw/",mission,"/",filenames_nc),
                              method="wget",
                              extra= c(paste0("--user=", user,
                                              " --password ",
                                              password))
      ))
      if(!(inherits(dw,"try-error")))
        break
    }
  }else{
    print("input a string")
  }
}
