
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
  # req contains the request object with elements:
  #   - questions: list of questions with parameters
  #   - q_matrix: Q-matrix for CDM (if applicable)
  #   - assessment_type: type of assessment (TRI or MDC)
  #   - model: model type (IRT(3PL, 2PL, 1PL) or CDM(DINA, DINO, GDINA))
  #   - config: elements which goes into the design of the assessment
  #       - start_item: index of the first item to be presented
  #       - type: model type (IRT or CDM) item_type input
  #       - criteria: next item selection criteria
  #       - method: estimation method
  #       - min_sem: minimum standard error of measurement
  #       - delta_thetas: change in theta for stopping criteria
  #       - thetas_start: initial theta values
  #       - pattern_theta: pattern of theta values for multidimensional assessments
  #       - min_items: minimum number of items to be administered
  #       - max_items: maximum number of items to be administered
  #       - max_time: maximum time allowed for the assessment

  print("Starting assessment...")
  # request arguments
  questions <- req$body$questions
  config <- req$body$config

  assessment_type <- req$body$assessment_type # IRT or CDM
  model <- req$body$model # model type IRT(3PL, 2PL, 1PL) or CDM(DINA, DINO, GDINA)
  q_matrix <- req$body$q_matrix # Q-matrix for CDM

  # assessment arguments that goes into the design
  item_type <- config$item_type # model type IRT(3PL, 2PL, 1PL) or CDM(DINA, DINO, GDINA)
  start_item <- config$start_item
  criteria <- config$criteria # next item selection criteria
  method <- config$method # estimation method
  min_sem <- config$min_sem
  delta_thetas <- config$delta_thetas
  thetas_start <- config$thetas_start
  pattern_theta <- config$pattern_theta
  
  min_items <- config$min_items
  max_items <- config$max_items
  max_time <- ifelse(
    !is.null(config$max_time),
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
    customUpdateThetas = if(assessment_type == "MDC") customUpdateThetas else NULL, # CDM
    customNextItem = if(assessment_type == "MDC") customNextItem else NULL # CDM
  )

  # create mirt object
  if(assessment_type == "TRI"){
    params <- build_irt_parameters(
      discrimination_list = questions$discrimination,
      difficulty_list = questions$difficulty,
      guessing_list = questions$guess
    )
    trait_cov <- matrix(2) # check
  }else if(assessment_type == "MDC") {
    source("mirtCAT.R") # edit some mirtCAT objects
    params <- generate_fake_mirt_pars(q_matrix)
    trait_cov <- diag(ncol(q_matrix))
    cdm_parameters <- questions
    
  }else{ # CAT com TRI e MDC
  }
    
  mo <- create_mirt_object(
    item_type = ifelse( # remove this when implementing CDM
      item_type %in% list("3PL", "2PL", "1PL"),
      item_type, 
      "3PL"
    ),
    parameters = params,
    latent_covariance = trait_cov  # Multidimensional element (validate importance)
  )
  


  # start assessment
  cat_design <- create_cat_design(
    mo, 
    pattern_theta = pattern_theta, 
    criteria = if(assessment_type == "MDC") "custom" else criteria,
    method = method,
    start_item = start_item,
    design = design #,q_matrix = q_matrix #  CDM TEST 
  )

  cat_design$item_time_history <- list()
  cat_design$last_answer_time <- Sys.time()
  
  # next_index <- mirtCAT::findNextItem(cat_design)
  next_index <- cat_design$design@start_item

  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    assessment_type = jsonlite::unbox(assessment_type), # NEW
    criteria = jsonlite::unbox(criteria), # NEW
    model = jsonlite::unbox(serialize_design(model)), # NEW
    questions = jsonlite::unbox(serialize_design(questions)), # NEW
    q_matrix = jsonlite::unbox(serialize_design(q_matrix)), # NEW
    design = jsonlite::unbox(serialize_design(cat_design))
  ))
}

#* @post /next-item
function(req) {
  
  # request arguments
  e_design <- deserialize_design(req$body$design)
  model <- deserialize_design(req$body$model)
  questions <- deserialize_design(req$body$questions)
  q_matrix <- deserialize_design(req$body$q_matrix)
  assessment_type <- req$body$assessment_type
  criteria <- req$body$criteria
  answer <- req$body$answer
  prev_item <- req$body$previous_index
  
  # set CDM variables to global environment
  if(assessment_type == "MDC"){
    model <<- model
    criteria <<- criteria
    cdm_parameters <<- questions
    q_matrix <<- q_matrix
  }
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

  # get next item
  
#   next_index <- ifelse(
#     !cat_design$design@stop_now,
#     mirtCAT::findNextItem(cat_design,  objective = objective),
#     0
#   )
  if(cat_design$design@stop_now){
    next_index <- 0
  }else if(assessment_type == "TRI"){
    next_index <- mirtCAT::findNextItem(cat_design)
  }else if(assessment_type == "MDC"){
    next_index <- customNextItem(person = cat_design$person, design = cat_design$design, test = cat_design$test)
  }
  

  return(list(
    next_index = jsonlite::unbox(next_index),
    stop = jsonlite::unbox(cat_design$design@stop_now),
    assessment_type = jsonlite::unbox(assessment_type), # NEW
    criteria = jsonlite::unbox(criteria), # NEW
    model = jsonlite::unbox(serialize_design(model)), # NEW
    questions = jsonlite::unbox(serialize_design(questions)), # NEW
    q_matrix = jsonlite::unbox(serialize_design(q_matrix)), # NEW
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