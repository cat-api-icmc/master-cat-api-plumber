
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

#* @get /test/serialize_design
function() {
  a <- c(1, 1.2, 0.9, 0.8, 1.1, 1.2, 0.8, 0.7, 0.5, 1)
  d <- c(-1, 1.5, 0, 0.5, -0.5, -1, 0, 0.1, 1.1, -0.2)
  g <- rep(0.2, 10)
  pars <- data.frame(a1 = a, d = d, g = g)
  lc <- matrix(2)

  mo <- create_mirt_object(
    parameters = pars,
    latent_covariance = lc,
  )
  design <- create_design(mo, pattern_theta = 0.1)

  e <- serialize_design(design)
  return(response(e))
}

#* @post /test/serialize_design
function(req) {
  e <- req$body$encoded
  design <- deserialize_design(e)

  print(design)
  print(class(design))
  print(typeof(design))

  return("DONE!")
}

