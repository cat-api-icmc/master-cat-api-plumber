CDM_DOMAIN <- list(
  models = c("DINA","DINO","GDINA"),
  criteria = list(
    non_adaptive = c("seq","random"),
    information  = c("KL","PWKL","MPWKL"),
    entropy      = c("SHE")
  ),
  methods = c("MLE","MAP","EAP")
)

CDM_ALL_CRITERIA <- unlist(CDM_DOMAIN$criteria)

validate_cdm_prior <- function(prior, K) {
  
  # --------------------------------------------------
  # 1️⃣ Validação básica
  # --------------------------------------------------
  if (is.null(prior))
    return(NULL)
  
  if (!is.list(prior))
    stop("CDM prior must be a named list.")
  
  if (is.null(names(prior)))
    stop("CDM prior must have named latent patterns.")
  
  # --------------------------------------------------
  # 2️⃣ Construir padrões esperados (ordem mirt-style)
  # --------------------------------------------------
  skill_patterns_matrix <- expand.grid(
    replicate(K, 0:1, simplify = FALSE)
  )
  
  skill_patterns <- apply(
    skill_patterns_matrix,
    1,
    paste0,
    collapse = ""
  )
  
  expected_n <- 2^K
  
  if (length(prior) != expected_n)
    stop(
      sprintf(
        "CDM prior must have exactly %d patterns (2^K).",
        expected_n
      )
    )
  
  # --------------------------------------------------
  # 3️⃣ Checar padrões faltantes/extras
  # --------------------------------------------------
  missing_patterns <- setdiff(skill_patterns, names(prior))
  extra_patterns   <- setdiff(names(prior), skill_patterns)
  
  if (length(missing_patterns) > 0)
    stop(
      paste(
        "Missing prior probabilities for patterns:",
        paste(missing_patterns, collapse = ", ")
      )
    )
  
  if (length(extra_patterns) > 0)
    stop(
      paste(
        "Invalid prior patterns:",
        paste(extra_patterns, collapse = ", ")
      )
    )
  
  # --------------------------------------------------
  # 4️⃣ Reordenar corretamente
  # --------------------------------------------------
  prior_vec <- numeric(expected_n)
  
  for (i in seq_along(skill_patterns)) {
    prior_vec[i] <- as.numeric(prior[[skill_patterns[i]]])
  }
  
  # --------------------------------------------------
  # 5️⃣ Validação numérica
  # --------------------------------------------------
  if (any(is.na(prior_vec)))
    stop("CDM prior contains NA values.")
  
  if (any(prior_vec < 0))
    stop("CDM prior must contain non-negative values.")
  
  return(prior_vec)
}

parse_cdm_request <- function(req) {

  # --------------------------------------------------
  # 1️⃣ Parse JSON bruto (evita simplificação automática)
  # --------------------------------------------------
  if (is.null(req$body))
    abort_bad_request("Request body is missing.")

  body <- req$body

  # --------------------------------------------------
  # 2️⃣ Required fields
  # --------------------------------------------------
  if (is.null(body$questions))
    abort_bad_request("Field 'questions' is required.")

  if (is.null(body$config))
    abort_bad_request("Field 'config' is required.")

  config <- body$config

  # ==================================================
  # 3️⃣ PRIOR (CDM specific)
  # ==================================================
  prior_raw <- config$prior %||% NULL

  prior <- NULL

  if (!is.null(prior_raw)) {
    K <- max(unlist(body$questions$params$cdm_qmatrix))
    prior <- validate_cdm_prior(prior_raw, K)
  }

  # ==================================================
  # 4️⃣ Threshold
  # ==================================================
  threshold <- config$threshold %||% NULL

  if (!is.null(threshold)) {
    threshold <- as.numeric(unlist(threshold))
  }

  # ==================================================
  # 5️⃣ Content balancing (optional)
  # ==================================================
  content <- NULL
  content_prop <- NULL

  if (!is.null(config$content_balancing)) {

    content <- config$content_balancing$content %||% NULL
    content_prop_raw <- config$content_balancing$content_prop %||% NULL

    if (!is.null(content))
      content <- unlist(content)

    if (!is.null(content_prop_raw)) {
      content_prop <- as.numeric(unlist(content_prop_raw))
      names(content_prop) <- names(content_prop_raw)
    }
  }

  # ==================================================
  # 6️⃣ Exposure (optional)
  # ==================================================
  exposure <- config$exposure %||% NULL

  if (!is.null(exposure))
    exposure <- as.numeric(unlist(exposure))

  # ==================================================
  # 7️⃣ Other numeric fields
  # ==================================================
  thetas_start <- config$thetas_start %||% NULL
  if (!is.null(thetas_start))
    thetas_start <- as.numeric(unlist(thetas_start))

  min_items <- config$min_items %||% NULL
  if (!is.null(min_items))
    min_items <- as.numeric(unlist(min_items))

  max_items <- config$max_items %||% NULL
  if (!is.null(max_items))
    max_items <- as.numeric(unlist(max_items))

  max_time <- config$max_time %||% Inf
  if (!is.null(max_time))
    max_time <- as.numeric(unlist(max_time))

  # start_item pode ser numeric ou character
  start_item <- config$start_item %||% NULL
  if (!is.null(start_item) && !is.character(start_item))
    start_item <- as.numeric(unlist(start_item))

  # ==================================================
  # 8️⃣ Return structured object
  # ==================================================
  list(
    questions         = body$questions,
    model             = config$model_type,
    criteria          = config$criteria,
    method            = config$method,
    start_item        = start_item,
    thetas_start      = thetas_start,
    threshold         = threshold,
    min_items         = min_items,
    max_items         = max_items,
    max_time          = max_time,
    content           = content,
    content_prop      = content_prop,
    exposure          = exposure,
    constr_fun_string = config$constr_fun,
    prior             = prior
  )
}

validate_cdm_request <- function(p) {

  # -------------------------------------------------
  # 1️⃣ Model validation
  # -------------------------------------------------
  if (!p$model %in% CDM_DOMAIN$models)
    abort_unprocessable(
      paste("Unsupported CDM model. Allowed:",
            paste(CDM_DOMAIN$models, collapse = ", "))
    )

  # -------------------------------------------------
  # 2️⃣ Criteria validation
  # -------------------------------------------------
  if (!p$criteria %in% CDM_ALL_CRITERIA)
    abort_unprocessable(
      paste("Unsupported CDM criteria. Allowed:",
            paste(CDM_ALL_CRITERIA, collapse = ", "))
    )

  # -------------------------------------------------
  # 3️⃣ Method validation
  # -------------------------------------------------
  if (!p$method %in% CDM_DOMAIN$methods)
    abort_unprocessable(
      paste("Unsupported estimation method. Allowed:",
            paste(CDM_DOMAIN$methods, collapse = ", "))
    )

  # -------------------------------------------------
  # 4️⃣ Threshold validation
  # -------------------------------------------------
  if (is.null(p$threshold))
    abort_bad_request("threshold is required for CDM.")

  if (!is.numeric(p$threshold))
    abort_unprocessable("threshold must be numeric.")

  if (!(length(p$threshold) %in% c(1, 2)))
    abort_unprocessable(
      "threshold must have length 1 (max rule) or 2 (dual rule)."
    )

  if (any(p$threshold <= 0 | p$threshold >= 1))
    abort_unprocessable(
      "threshold values must be in (0,1)."
    )

  # -------------------------------------------------
  # 5️⃣ start_item validation
  # -------------------------------------------------
  if (!is.null(p$start_item)) {

    if (is.numeric(p$start_item)) {

      if (length(p$start_item) != 1 || p$start_item < 1)
        abort_unprocessable(
          "start_item numeric must be positive scalar."
        )

    } else if (is.character(p$start_item)) {

      if (!p$start_item %in% CDM_ALL_CRITERIA)
        abort_unprocessable(
          paste("start_item criterion must be one of:",
                paste(CDM_ALL_CRITERIA, collapse = ", "))
        )

    } else {
      abort_unprocessable(
        "start_item must be numeric or character."
      )
    }
  }

  # -------------------------------------------------
  # 6️⃣ Min/Max items validation
  # -------------------------------------------------
  if (!is.null(p$min_items) && (!is.numeric(p$min_items) || p$min_items < 1))
    abort_unprocessable("min_items must be positive integer.")

  if (!is.null(p$max_items) && (!is.numeric(p$max_items) || p$max_items < 1))
    abort_unprocessable("max_items must be positive integer.")

  if (!is.null(p$min_items) &&
      !is.null(p$max_items) &&
      p$min_items > p$max_items)
    abort_unprocessable("min_items cannot be greater than max_items.")

  # -------------------------------------------------
  # 7️⃣ Questions validation
  # -------------------------------------------------
  if (is.null(p$questions$params))
    abort_bad_request("questions$params is required for CDM.")


  TRUE
}


build_cdm_domain_objects <- function(p) {
  
  q_values <- build_cdm_parameters(
    questions = p$questions,
    model     = p$model
  )
  
  list(
    parameters = q_values$parameters,
    q_matrix   = q_values$qmatrix,
    n_skills   = q_values$n_skills
  )
}

parse_cdm_next_request <- function(req) {

  if (is.null(req$body))
    abort_bad_request("Request body is missing.")

  body <- req$body

  required_fields <- c("design", "answer", "previous_index")

  for (field in required_fields) {
    if (is.null(body[[field]]))
      abort_bad_request(paste0("Field '", field, "' is required."))
  }

  list(
    design_serialized = body$design,
    answer = body$answer,
    previous_index = body$previous_index
  )
}

validate_cdm_next_request <- function(p) {

  if (!is.numeric(p$answer))
    abort_bad_request("answer must be numeric.")

  if (!is.numeric(p$previous_index) ||
      length(p$previous_index) != 1)
    abort_bad_request("previous_index must be numeric scalar.")

  TRUE
}


# --- Endpoint functions
cdm_start_assessment <- function(req, res, verbose = FALSE, validate = TRUE) {
  
  tryCatch({

    # ======================================
    # 1 PARSE
    # ======================================
    parsed <- parse_cdm_request(req)
    
    # ======================================
    # 2 VALIDATION
    # ======================================
    if (validate) {
      validate_cdm_request(parsed)
    }
    
    # ======================================
    # 3 BUILD DOMAIN OBJECTS
    # ======================================
    cdm_objects <- build_cdm_domain_objects(parsed)
    
    constr_fun <- build_constr_fun(parsed$constr_fun_string)

    # --------------------------------------
    # CAT DESIGN (only allowed fields)
    # --------------------------------------
    design <- list(
      thetas.start = parsed$thetas_start,
      min_items    = parsed$min_items,
      max_items    = parsed$max_items,
      max_time     = parsed$max_time,
      content      = parsed$content,
      content_prop = parsed$content_prop,
      exposure     = parsed$exposure,
      constr_fun   = constr_fun,
      customUpdateThetas = customUpdateSkills,
      customStop   = customStopCDM,
      customNextItem = customNextItemCDM
    )

    # ======================================
    # 4 CREATE PLACEHOLDER MIRT OBJECT
    # ======================================
    params <- generate_fake_mirt_pars(cdm_objects$q_matrix)
    trait_cov <- diag(ncol(cdm_objects$q_matrix))
    
    mo <- create_mirt_object(
      parameters = params,
      item_type = "3PL",
      latent_covariance = trait_cov
    )
    
    # ======================================
    # 5 CREATE CAT DESIGN
    # ======================================
    cat_design <- create_cat_design(
        mo,
        method = parsed$method,
        design = design
      )
    
    cat_design$item_time_history <- list()
    cat_design$last_answer_time <- Sys.time()

    # ======================================
    # 6 STORE CDM METADATA (SAFE PLACE)
    # ======================================
    cat_design$person$clientData$cdm <- list(
      config = list(
        model = parsed$model,
        criteria = parsed$criteria,
        method = parsed$method,
        threshold = parsed$threshold,
        start_item = parsed$start_item,
        prior = parsed$prior
      ),
      data = list(
        q_matrix = cdm_objects$q_matrix,
        parameters = cdm_objects$parameters,
        n_skills = cdm_objects$n_skills
      )
    )

    cat_design$person$clientData$cdm$start_time <- Sys.time()
    
    # ======================================
    # 7 FIRST ITEM SELECTION
    # ======================================
    next_index <- customNextItemCDM(
        person = cat_design$person,
        design = cat_design$design,
        test   = cat_design$test
        )

    if (verbose) {
      print_item_selection(next_index, stage = "START")
    }
    
    # ======================================
    # 8 RESPONSE
    # ======================================
    res$status <- 200
    
    return(list(
      success = TRUE,
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      design = jsonlite::unbox(serialize_design(cat_design))
    ))
    
  }, error = function(e) {
    print(e)
    build_error_response(e, res)
    
  })
}

cdm_next_item <- function(req, res, verbose = FALSE, validate = TRUE) {

  tryCatch({

    # ======================================
    # 1 PARSE
    # ======================================
    parsed <- parse_cdm_next_request(req)

    # ======================================
    # 2 VALIDATION
    # ======================================
    if (validate) {
      validate_cdm_next_request(parsed)
    }

    # ======================================
    # 3 DESERIALIZE DESIGN
    # ======================================
    cat_design <- deserialize_design(parsed$design_serialized)

    if (is.null(cat_design$person$clientData$cdm))
      abort_bad_request("Invalid or corrupted CDM design state.")

    # ======================================
    # 4 UPDATE DESIGN (response update)
    # ======================================
    cat_design <- mirtCAT::updateDesign(
      cat_design,
      new_item = parsed$previous_index,
      new_response = parsed$answer,
      updateTheta = TRUE
    )

    # tempo
    now <- Sys.time()

    cat_design$item_time_history <- append(
      cat_design$item_time_history,
      as.numeric(difftime(
        now,
        cat_design$last_answer_time,
        units = "secs"
      ))
    )

    cat_design$last_answer_time <- now


    # ======================================
    # 5 SELECT NEXT ITEM
    # ======================================
    if (cat_design$design@stop_now) {

      next_index <- 0

    } else {

      next_index <- customNextItemCDM(
        person = cat_design$person,
        design = cat_design$design,
        test   = cat_design$test
        )
    }

    if (is.na(next_index)) {
      cat_design$design@stop_now <- TRUE
      next_index <- 0
    }

    if (verbose) {
      print_item_selection(next_index, stage = "NEXT")
    }

    # ======================================
    # 6 SUCCESS RESPONSE
    # ======================================
    res$status <- 200

    return(list(
      success = TRUE,
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      design = jsonlite::unbox(serialize_design(cat_design))
    ))

  }, error = function(e) {
    print(e)
    build_error_response(e, res)

  })
}


