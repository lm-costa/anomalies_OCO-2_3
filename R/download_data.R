source('fun/functions.r')


#OCO-2
url_filename <- list.files("url/OCO-2/",
                            pattern = ".txt",
                            full.names = TRUE)

urls <- read.table(url_filename) |>
  dplyr::filter(!stringr::str_detect(V1,".pdf"))
n_urls <- nrow(urls)

###
tictoc::tic()
furrr::future_pmap(list(urls[,1],"input your user","your password","OCO-2"),my_ncdf4_download)
tictoc::toc()


#OCO-3
url_filename <- list.files("url/OCO-3/",
                           pattern = ".txt",
                           full.names = TRUE)

urls <- read.table(url_filename) |>
  dplyr::filter(!stringr::str_detect(V1,".pdf"))
n_urls <- nrow(urls)

###
tictoc::tic()
furrr::future_pmap(list(urls[,1],"input your user","your password",'OCO-3'),my_ncdf4_download)
tictoc::toc()
