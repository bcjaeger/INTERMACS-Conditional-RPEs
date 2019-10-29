
.read_csv <- function(file, ...){
  .dots <- list(...)
  .dots$guess_max = 20000
  .dots$file <- file
  do.call(readr::read_csv, .dots)
}