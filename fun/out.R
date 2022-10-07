remove_outlier <- function(x,coefi=1.5){
  med = median(x)
  li = med - coefi*IQR(x)
  ls = med + coefi*IQR(x)
  c(Lim_Inf = li, Lim_Sup = ls)
}