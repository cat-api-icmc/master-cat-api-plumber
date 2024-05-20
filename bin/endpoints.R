
#* @get /hc
function() {
  return(list(status = jsonlite::unbox(
    sprintf(
      "Healthy! %s | plumber %s | api %s", 
      r_version, plumber_version, app_version
    )
  )))
}

#* @post /start-assessment
function(req) {
  questions <- req$body$questions
  pattern_theta <- req$body$pattern_theta

  # create mirt object
  irt_params <- build_irt_parameters(
    discrimination_list = questions$discrimination,
    difficulty_list = questions$difficulty,
    guessing_list = questions$guess
  )

  mo <- create_mirt_object(
    parameters = irt_params,
    latent_covariance = matrix(2)
  )

  # start assessment
  cat_design <- create_cat_design(mo, pattern_theta = pattern_theta)
  next_index <- mirtCAT::findNextItem(cat_design)

  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    design = jsonlite::unbox(serialize_design(cat_design))
  ))
}

#* @post /next-item
function(req) {
  e_design <- req$body$design
  answer <- req$body$answer
  prev_item <- req$body$previous_index

  # deserialize and update design
  cat_design <- mirtCAT::updateDesign(
    deserialize_design(e_design),
    new_item = prev_item,
    new_response = answer,
    updateTheta = TRUE
  )

  # get next item
  next_index <- ifelse(
    !cat_design$design@stop_now, 
    next_index,
    0
  )

  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    design = jsonlite::unbox(serialize_design(cat_design))
  ))
}


#* @post /get-design-data
function(req) {
  e_design <- req$body$design
  cat_design <- deserialize_design(e_design)

  item_history <- cat_design$person$items_answered
  response_history <- cat_design$person$responses
  theta_history <- lapply(
    cat_design$person$thetas_history, 
    function(x) jsonlite::unbox(x)
  )
  standard_error_history <- lapply(
    cat_design$person$thetas_SE_history, 
    function(x) jsonlite::unbox(x)
  )

  return(list(
    "item_history" = item_history,
    "response_history" = response_history,
    "theta_history" = theta_history,
    "standard_error_history" = standard_error_history
  ))
}

