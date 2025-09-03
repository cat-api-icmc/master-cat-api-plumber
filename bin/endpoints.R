
#* @get /hc
function() {
  return(list(status = jsonlite::unbox(
    sprintf(
      "Healthy! %s | plumber %s | api %s", 
      r_version, plumber_version, app_version
    )
  )))
}

#* @post /irt/start-assessment
function(req) {
  # - questions: list of questions with parameters
  # - config: elements which goes into the design of the assessment
  #     - model: model type (3PL, 2PL, 1PL) item_type input
  #     - start_item: index of the first item to be presented
  #     - criteria: next item selection criteria
  #     - min_sem: minimum standard error of measurement
  #     - delta_thetas: change in theta for stopping criteria
  #     - thetas_start: initial theta values
  #     - pattern_theta: pattern of theta values for multidimensional assessments
  #     - min_items: minimum number of items to be administered
  #     - max_items: maximum number of items to be administered
  #     - max_time: maximum time allowed for the assessment

  questions <- req$body$questions
  config <- req$body$config
  
  model <- config$model # 3PL
  start_item <- config$start_item
  criteria <- config$criteria

  thetas_start <- config$thetas_start
  pattern_theta <- config$pattern_theta

  # stoping criteria
  min_sem <- config$design$min_sem
  delta_thetas <- config$design$delta_thetas
  min_items <- config$design$min_items
  max_items <- config$design$max_items
  max_time <- ifelse(
    !is.null(config$design$max_time),
    config$max_time,
    Inf
  )

  design <- list(
    min_SEM = min_sem ,
    delta_thetas = delta_thetas,
    thetas.start = thetas_start,
    min_items = min_items,
    max_items = max_items,
    max_time = max_time
  )

  # create mirt object
  irt_params <- build_irt_parameters(
    discrimination_list = questions$discrimination,
    difficulty_list = questions$difficulty,
    guessing_list = questions$guess
  )
  
  mo <- create_mirt_object(
    item_type = ifelse(
      model %in% list("3PL", "2PL", "1PL"),
      model, 
      "3PL"
    ),
    parameters = irt_params,
    latent_covariance = matrix(2)
  )
  
  # start assessment
  cat_design <- create_cat_design(
    mo, 
    pattern_theta = pattern_theta, 
    criteria = criteria, 
    start_item = start_item,
    design = design 
  )

  cat_design$item_time_history <- list()
  cat_design$last_answer_time <- Sys.time()
  
  next_index <- mirtCAT::findNextItem(cat_design)
  
  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    design = jsonlite::unbox(serialize_design(cat_design))
  ))
}


#* @post /cdm/start-assessment
function(req) {
  # req contains the request object with elements:
  #   - questions: list of questions with parameters
  #   - q_matrix: Q-matrix for CDM
  #   - config: elements which goes into the design of the assessment
  #       - model: model type (DINA, DINO, GDINA) item_type input
  #       - start_item: index of the first item to be presented
  #       - criteria: next item selection criteria
  #       - method: estimation method
  #       - min_sem: minimum standard error of measurement
  #       - delta_thetas: change in theta for stopping criteria
  #       - thetas_start: initial theta values
  #       - pattern_theta: pattern of theta values for multidimensional assessments
  #       - min_items: minimum number of items to be administered
  #       - max_items: maximum number of items to be administered
  #       - max_time: maximum time allowed for the assessment

  # request arguments
  questions <- req$body$questions
  q_matrix <- req$body$q_matrix
  config <- req$body$config
  
  # assessment arguments that goes into the design
  model <- config$model
  start_item <- config$start_item
  method <- config$method # estimation method
  thetas_start <- config$thetas_start
  pattern_theta <- config$pattern_theta
  
  # stoping criteria
  min_sem <- config$design$min_sem
  delta_thetas <- config$design$delta_thetas
  min_items <- config$design$min_items
  max_items <- config$design$max_items
  max_time <- ifelse(
    !is.null(config$design$max_time),
    config$max_time,
    Inf
  )
  
  design <- list(
    min_SEM = min_sem,
    delta_thetas = delta_thetas,
    thetas.start = thetas_start,
    min_items = min_items,
    max_items = max_items,
    max_time = max_time,
    customUpdateThetas = customUpdateThetas,
    customNextItem = customNextItem # validar se eh usado internamente
  )

  # create mirt object
  source("mirtCAT.R") # edit some mirtCAT objects
  params <- generate_fake_mirt_pars(q_matrix)
  trait_cov <- diag(ncol(q_matrix))
  cdm_parameters <- questions
  
  mo <- create_mirt_object(
    item_type = "3PL", #model,
    parameters = params,
    latent_covariance = trait_cov  # Multidimensional element (validate importance)
  )
  
  # start assessment
  cat_design <- create_cat_design(
    mo, 
    pattern_theta = pattern_theta, 
    criteria = "custom",
    method = method,
    start_item = start_item,
    design = design
  )
  
  cat_design$item_time_history <- list()
  cat_design$last_answer_time <- Sys.time()
  
  next_index <- cat_design$design@start_item
  
  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    model = jsonlite::unbox(serialize_design(model)),
    questions = jsonlite::unbox(serialize_design(questions)),
    q_matrix = jsonlite::unbox(serialize_design(q_matrix)),
    design = jsonlite::unbox(serialize_design(cat_design)),
    criteria = jsonlite::unbox(config$criteria)
  ))
}


#* @post /irt/next-item
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

  now <- Sys.time()
  cat_design$item_time_history <- append(
    cat_design$item_time_history,
    as.numeric(difftime(
      now, cat_design$last_answer_time, units = "secs"
    ))
  )
  cat_design$last_answer_time <- now

  # get next item
  next_index <- ifelse(
    !cat_design$design@stop_now,
    mirtCAT::findNextItem(cat_design), # confimar no CDM
    0
  )
  
  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    design = jsonlite::unbox(serialize_design(cat_design))
  ))
}


#* @post /cdm/next-item
function(req) {
  
  # request arguments
  e_design <- deserialize_design(req$body$design)
  model <- deserialize_design(req$body$model)
  questions <- deserialize_design(req$body$questions)
  q_matrix <- deserialize_design(req$body$q_matrix)
  criteria <- req$body$criteria
  answer <- req$body$answer
  prev_item <- req$body$previous_index
  
  # set CDM variables to global environment
  model <<- model
  criteria <<- criteria
  cdm_parameters <<- questions
  q_matrix <<- q_matrix
  
  # deserialize and update design
  cat_design <- mirtCAT::updateDesign(
    e_design,
    new_item = prev_item,
    new_response = answer,
    updateTheta = TRUE
  )
  
  
  now <- Sys.time()
  cat_design$item_time_history <- append(
    cat_design$item_time_history,
    as.numeric(difftime(
      now, cat_design$last_answer_time, units = "secs"
    ))
  )
  cat_design$last_answer_time <- now

  if(cat_design$design@stop_now){
    next_index <- 0
  } else {
    next_index <- customNextItem( # funcao usada diretamente
      person = cat_design$person,
      design = cat_design$design,
      test = cat_design$test
    )
  }
  
  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    criteria = jsonlite::unbox(criteria),
    model = jsonlite::unbox(serialize_design(model)),
    questions = jsonlite::unbox(serialize_design(questions)),
    q_matrix = jsonlite::unbox(serialize_design(q_matrix)),
    design = jsonlite::unbox(serialize_design(cat_design))
  ))
}


#* @post /get-design-data
function(req) {
  e_design <- req$body$design
  cat_design <- deserialize_design(e_design)

  item_history <- cat_design$person$items_answered
  response_history <- cat_design$person$responses
  last_answer_time <- jsonlite::unbox(cat_design$last_answer_time)

  item_time_history <- lapply(
    cat_design$item_time_history,
    function(x) jsonlite::unbox(x)
  )

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
    "item_time_history" = item_time_history,
    "last_answer_time" = last_answer_time,
    "theta_history" = theta_history,
    "standard_error_history" = standard_error_history
  ))
}
