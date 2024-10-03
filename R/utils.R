pt_time_extract<-function(string){
  string |>
    stringr::str_extract("\\d{2}/\\d{2}/\\d+") |>
    lubridate::dmy()
}

sub<- function(string,start,end,value){
  stringr::str_sub(string,start,end) <- value
  string
}
