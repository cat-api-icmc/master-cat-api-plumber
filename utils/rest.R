library(jsonlite)

response <- function(data, status = 200) {
  return(list(
    response = jsonlite::unbox(data),
    status = jsonlite::unbox(status)
  ))
}
