library(httr)

API_URL <- "http://127.0.0.1:8000/api"

USER_TOKEN <- "d3544ba5-41c5-480f-8fe5-37ec99c77882"

ASSESSMENT_UUID <- "9e79179d-998d-473e-8898-baa73dc06517"


start_assessment_api <- function() {
  
  response <- POST(
    url = paste0(API_URL, "/user-assessment/"),
    add_headers(Authorization = paste("Token", USER_TOKEN)),
    body = list(assessment = ASSESSMENT_UUID),
    encode = "form",
    timeout(3000)
  )
  
  content <- httr::content(response, "parsed")
  
  return(content)
}

get_next_item_api <- function(assessment,answer) {
  
  cat("Answering:", assessment$next_question$id, "\n")
  
  answer <- ifelse(answer == 1, "Correta", "Errada")
  
  answer_order <- which(sapply(assessment$next_question$alternatives, function(i) i$text) == answer)
  alt_id <- assessment$next_question$alternatives[[answer_order]]$id
  
  response <- PUT(
    url = paste0(API_URL, "/user-assessment/", assessment$user_assessment, "/"),
    add_headers(Authorization = paste("Token", USER_TOKEN)),
    body = list(alternative = alt_id),
    encode = "form",
    timeout(10000)
  )

  content <- httr::content(response, "parsed")

  return(content)
}


simulate_assessment <- function(){
  
  assessment <- start_assessment_api()
  cat("Started assessment\n")
  
  n_itens <- 0
  while(assessment$in_progress) {
    n_itens <- n_itens + 1
    cat(n_itens, "status:", assessment$status, "\n")
    assessment <- get_next_item_api(
      assessment,
      1
    )
    print(assessment)
  }
  
  return(assessment)
}


test <- simulate_assessment()






test <- start_assessment_api()
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)
test <- get_next_item_api(test, 1)


