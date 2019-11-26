as_numeric_ignore_commas <- function(x){
  as.numeric(gsub("\\,", "", x))
}
