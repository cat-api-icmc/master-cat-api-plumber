library(mirtCAT)

IRT_DOMAIN <- list(

  models = list(
    unidimensional = c("1PL","2PL","3PL","4PL"),
    multidimensional = c("M1PL","M2PL","M3PL","M4PL")
  ),

  criteria = list(
    non_adaptive = c(
      "seq",
      "random"
    ),
    unidimensional = c(
      "MI",
      "MEPV",
      "MLWI",
      "MPWI",
      "MEI",
      "IKL",
      "IKLP",
      "IKLn",
      "IKLPn"
    ),
    multidimensional = c(
      "Drule",
      "Trule",
      "Arule",
      "Erule",
      "Wrule",
      "DPrule",
      "TPrule",
      "APrule",
      "EPrule",
      "WPrule"
    ),
    common = c(
      "KL",
      "KLn"
    )
  ),

  methods = c("MLE","MAP","EAP")
)

IRT_ALL_MODELS <- unname(unlist(IRT_DOMAIN$models))
IRT_ALL_CRITERIA <- unname(unlist(IRT_DOMAIN$criteria))

build_irt_parameters <- function(
  discrimination = NULL,
  difficulty,
  guessing = NULL,
  upper = NULL,
  model_type = "3PL"
) {

  # -------------------------------------------------
  # 1️⃣ Model validation
  # -------------------------------------------------
  if (!model_type %in% IRT_ALL_MODELS)
    abort_unprocessable("Unsupported model_type.")

  # -------------------------------------------------
  # 2️⃣ Discrimination
  # -------------------------------------------------
  if (is.vector(discrimination)) {
    discrimination <- matrix(discrimination, ncol = 1)
  }

  if (!is.matrix(discrimination))
    abort_bad_request("discrimination must be vector or matrix.")

  if (!is.numeric(discrimination))
    abort_unprocessable("discrimination must be numeric.")

  if (any(is.na(discrimination)))
    abort_unprocessable("discrimination contains NA.")

  n_items <- nrow(discrimination)
  n_dim   <- ncol(discrimination)

  # -------------------------------------------------
  # 3️⃣ Difficulty
  # -------------------------------------------------
  if (!is.numeric(difficulty))
    abort_unprocessable("difficulty must be numeric.")

  if (length(difficulty) != n_items)
    abort_unprocessable(
      "difficulty length must equal number of items."
    )

  if (any(is.na(difficulty)))
    abort_unprocessable("difficulty contains NA.")

  # -------------------------------------------------
  # 4️⃣ Dimensional consistency
  # -------------------------------------------------
  is_multidim_model <- model_type %in% IRT_DOMAIN$models$multidimensional

  if (is_multidim_model && n_dim < 2)
    abort_unprocessable(
      paste0("Model ", model_type, " requires multidimensional discrimination.")
    )

  if (!is_multidim_model && n_dim != 1)
    abort_unprocessable(
      paste0("Model ", model_type, " requires unidimensional discrimination.")
    )

  # -------------------------------------------------
  # 5️⃣ Intercept calculation
  # -------------------------------------------------
  if (is_multidim_model) {

    a_norm <- sqrt(rowSums(discrimination^2))
    d <- - difficulty * a_norm

  } else {

    d <- - discrimination[,1] * difficulty
  }

  # -------------------------------------------------
  # 6️⃣ Build parameter dataframe
  # -------------------------------------------------
  param_df <- as.data.frame(discrimination)
  colnames(param_df) <- paste0("a", seq_len(n_dim))

  param_df$d <- d

  # -------------------------------------------------
  # 7️⃣ Guessing (3PL / 4PL)
  # -------------------------------------------------
  if (grepl("3PL|4PL", model_type)) {

    if (is.null(guessing))
      abort_bad_request("guessing parameter required for 3PL/4PL models.")

    if (!is.numeric(guessing))
      abort_unprocessable("guessing must be numeric.")

    if (length(guessing) != n_items)
      abort_unprocessable("guessing length mismatch.")

    if (any(guessing <= 0 | guessing >= 1))
      abort_unprocessable("guessing must be in (0,1).")

    param_df$g <- guessing
  }

  # -------------------------------------------------
  # 8️⃣ Upper asymptote (4PL / M4PL)
  # -------------------------------------------------
  if (grepl("4PL", model_type)) {

    if (is.null(upper))
      abort_bad_request("upper parameter required for 4PL models.")

    if (!is.numeric(upper))
      abort_unprocessable("upper must be numeric.")

    if (length(upper) != n_items)
      abort_unprocessable("upper length mismatch.")

    if (any(upper <= 0 | upper > 1))
      abort_unprocessable("upper must be in (0,1].")

    if (!is.null(guessing)) {
      if (any(upper <= guessing))
        abort_unprocessable("upper must be greater than guessing.")
    }

    param_df$u <- upper
  }

  # -------------------------------------------------
  # 9️⃣ Final sanity check
  # -------------------------------------------------
  if (any(!is.finite(as.matrix(param_df))))
    abort_unprocessable("Non-finite values detected in parameters.")

  param_df
}

customNextItemIRT <- function(person, design, test, criteria){

  constr_fun <- design@constr_fun

  # Detect empty function
  is_empty <- identical(body(constr_fun), quote(NULL)) ||
              length(body(constr_fun)) == 1

  if (is_empty) {

    best_item <- findNextItem(
      person = person,
      design = design,
      test = test,
      criteria = criteria
    )

  } else {

    obj <- computeCriteria(
      person = person,
      design = design,
      test = test,
      criteria = criteria
    )

    best_item <- findNextItem(
      person = person,
      design = design,
      test = test,
      objective = obj
    )
  }

  best_item
}

parse_irt_request <- function(req) {

  if (is.null(req$body))
    abort_bad_request("Request body is missing.")

  body <- req$body

  if (is.null(body$questions))
    abort_bad_request("Field 'questions' is required.")

  if (is.null(body$config))
    abort_bad_request("Field 'config' is required.")

  config <- body$config

  # -----------------------------
  # PRIOR (TRI)
  # -----------------------------
  prior <- config$prior %||% list()

  latent_means <- prior$latent_means
  latent_covariance <- prior$latent_covariance

  # -----------------------------
  # DESIGN EXTRAS (TRI)
  # -----------------------------
  quadpts <- config$quadpts
  theta_range <- config$theta_range
  weights <- config$weights
  KL_delta <- config$KL_delta

  # -----------------------------
  # Content balancing
  # -----------------------------
  content <- NULL
  content_prop <- NULL

  if (!is.null(config$content_balancing)) {
    content <- unlist(config$content_balancing$content)
    content_prop <- unlist(config$content_balancing$content_prop)
  }

  exposure <- if (!is.null(config$exposure)) {
    unlist(config$exposure)
  } else {
    NULL
  }

  list(
    questions = body$questions,
    model = config$model_type %||% "3PL",
    method = config$method %||% "EAP",
    start_item = config$start_item,
    criteria = config$criteria,

    # PRIOR
    latent_means = latent_means,
    latent_covariance = latent_covariance,

    # DESIGN EXTRAS
    quadpts = quadpts,
    theta_range = theta_range,
    weights = weights,
    KL_delta = KL_delta,

    # STOPPING
    thetas_start = config$thetas_start,
    min_sem = config$min_sem,
    delta_thetas = config$delta_thetas,
    min_items = config$min_items,
    max_items = config$max_items,
    max_time = config$max_time %||% Inf,

    content = content,
    content_prop = content_prop,
    exposure = exposure,
    constr_fun_string = config$constr_fun
  )
}

validate_irt_request <- function(p) {

  # -------------------------------------------------
  # 1️⃣ Model validation
  # -------------------------------------------------
  if (!p$model %in% IRT_ALL_MODELS)
    abort_unprocessable(
      paste("model_type must be one of:",
            paste(IRT_ALL_MODELS, collapse = ", "))
    )

  is_multidim_model <- grepl("^M", p$model)
  base_model <- gsub("^M", "", p$model)  # remove M prefix


  # -------------------------------------------------
  # 2️⃣ Method validation
  # -------------------------------------------------
  if (!p$method %in% IRT_DOMAIN$methods)
    abort_unprocessable(
      paste("method must be one of:",
            paste(IRT_DOMAIN$methods, collapse = ", "))
    )


  # -------------------------------------------------
  # 3️⃣ Criteria validation
  # -------------------------------------------------
  if (!p$criteria %in% IRT_ALL_CRITERIA)
    abort_unprocessable("Invalid criteria.")

  crit <- p$criteria

  if (crit %in% IRT_DOMAIN$criteria$multidimensional &&
      !is_multidim_model)
    abort_unprocessable(
      paste0("Criteria '", crit,
             "' requires a multidimensional model.")
    )


  # -------------------------------------------------
  # 4️⃣ Question parameters existence
  # -------------------------------------------------
  if (is.null(p$questions$params))
    abort_bad_request("questions$params is required.")

  params <- p$questions$params

  if (is.null(params$irt_difficulty))
    abort_bad_request("irt_difficulty is required.")

  diff <- params$irt_difficulty

  if (!is.numeric(diff))
    abort_unprocessable("irt_difficulty must be numeric.")

  n_items <- length(diff)


  # -------------------------------------------------
  # 5️⃣ Discrimination validation
  # -------------------------------------------------
  disc <- params$irt_discrimination

  if (base_model == "1PL") {

    # Rasch: discrimination should NOT be provided
    if (!is.null(disc))
      abort_unprocessable(
        "irt_discrimination must not be provided for 1PL/M1PL."
      )

    n_dim <- if(is_multidim_model) 2 else 1

  } else {

    if (is.null(disc))
      abort_bad_request(
        paste0("irt_discrimination is required for ", p$model)
      )

    if (is.vector(disc))
      disc <- matrix(disc, ncol = 1)

    if (!is.matrix(disc))
      abort_unprocessable(
        "irt_discrimination must be vector or matrix."
      )

    if (nrow(disc) != n_items)
      abort_unprocessable(
        "irt_discrimination must have one row per item."
      )

    n_dim <- ncol(disc)

    if (is_multidim_model && n_dim < 2)
      abort_unprocessable(
        "Multidimensional models require ≥2 discrimination columns."
      )

    if (!is_multidim_model && n_dim != 1)
      abort_unprocessable(
        "Unidimensional models require single-column discrimination."
      )
  }


  # -------------------------------------------------
  # 6️⃣ Guess parameter validation
  # -------------------------------------------------
  guess <- params$irt_guess

  if (base_model %in% c("3PL", "4PL")) {

    if (is.null(guess))
      abort_unprocessable(
        paste0("irt_guess is required for ", p$model)
      )

    if (!is.numeric(guess) || length(guess) != n_items)
      abort_unprocessable(
        "irt_guess must be numeric vector with one value per item."
      )

  } else {

    if (!is.null(guess))
      abort_unprocessable(
        "irt_guess is not allowed for 1PL/2PL models."
      )
  }


  # -------------------------------------------------
  # 7️⃣ Upper parameter validation
  # -------------------------------------------------
  upper <- params$irt_upper_asymptote

  if (base_model == "4PL") {

    if (is.null(upper))
      abort_unprocessable(
        paste0("irt_upper is required for ", p$model)
      )

    if (!is.numeric(upper) || length(upper) != n_items)
      abort_unprocessable(
        "irt_upper must be numeric vector with one value per item."
      )

  } else {

    if (!is.null(upper))
      abort_unprocessable(
        "irt_upper is only allowed for 4PL/M4PL."
      )
  }


  # -------------------------------------------------
  # 8️⃣ Prior validation
  # -------------------------------------------------
  if (!is.null(p$latent_means)) {

    if (!is.numeric(p$latent_means) ||
        length(p$latent_means) != n_dim)
      abort_unprocessable(
        "latent_means dimension mismatch."
      )
  }

  if (!is.null(p$latent_covariance)) {

    lc <- as.matrix(p$latent_covariance)

    if (!is.numeric(lc) ||
        nrow(lc) != ncol(lc) ||
        nrow(lc) != n_dim)
      abort_unprocessable(
        "latent_covariance must be square matrix with dimension equal to number of traits."
      )
  }


  # -------------------------------------------------
  # 9️⃣ weights (multidimensional rules)
  # -------------------------------------------------
  weighted_rules <- c(
    "Trule","Arule","Wrule",
    "TPrule","APrule","WPrule"
  )

  if (crit %in% weighted_rules) {

    if (is.null(p$weights))
      abort_unprocessable(
        paste0("Criteria '", crit,
               "' requires weights parameter.")
      )

    if (!is.numeric(p$weights) ||
        length(p$weights) != n_dim)
      abort_unprocessable(
        "weights must be numeric vector with length equal to number of dimensions."
      )
  }


  # -------------------------------------------------
  # 🔟 KL_delta
  # -------------------------------------------------
  kl_rules <- c("KL","KLn","IKL","IKLP","IKLn","IKLPn")

  if (crit %in% kl_rules) {
    if (!is.null(p$KL_delta) &&
        (!is.numeric(p$KL_delta) ||
         length(p$KL_delta) != 1 ||
         p$KL_delta <= 0))
      abort_unprocessable(
        "KL_delta must be positive scalar."
      )
  }


  TRUE
}

parse_irt_next_request <- function(req) {

  if (is.null(req$body))
    abort_bad_request("Request body is missing.")

  body <- req$body

  if (is.null(body$design))
    abort_bad_request("Field 'design' is required.")

  if (is.null(body$answer))
    abort_bad_request("Field 'answer' is required.")

  if (is.null(body$previous_index))
    abort_bad_request("Field 'previous_index' is required.")

  list(
    design_serialized = body$design,
    answer = body$answer,
    previous_index = body$previous_index
  )
}

validate_irt_next_request <- function(p) {

  if (!is.numeric(p$answer))
    abort_bad_request("answer must be numeric.")

  if (!is.numeric(p$previous_index) || length(p$previous_index) != 1)
    abort_bad_request("previous_index must be a single numeric value.")

  TRUE
}

update_cat_design <- function(design_serialized, previous_index, answer) {

  cat_design <- mirtCAT::updateDesign(
    deserialize_design(design_serialized),
    new_item = previous_index,
    new_response = answer,
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

  cat_design
}

irt_select_next_item <- function(cat_design) {
  
  if (cat_design$design@stop_now)
    return(0)

  criteria <- cat_design$design@criteria
  
  next_index <- customNextItemIRT(
    person = cat_design$person,
    design = cat_design$design,
    test = cat_design$test,
    criteria = criteria
  )

  if (is.na(next_index)) {
    cat_design$design@stop_now <- TRUE
    return(0)
  }

  next_index
}

normalize_irt_model <- function(model_type) {
  
  if (!model_type %in% IRT_ALL_MODELS)
    abort_unprocessable("Unsupported model_type.")
  
  # Remove prefix M se existir
  sub("^M", "", model_type)
}

print_irt_configuration <- function(
  parsed,
  latent_means,
  latent_covariance,
  irt_params,
  design
) {
  
  cat("\n==============================\n")
  cat("IRT TEST CONFIGURATION\n")
  cat("==============================\n")
  
  # -------------------------------------------------
  # Modelo
  # -------------------------------------------------
  cat("Model (API): ", parsed$model, "\n")
  cat("Model (mirt): ", normalize_irt_model(parsed$model), "\n")
  
  # -------------------------------------------------
  # Dimensionalidade
  # -------------------------------------------------
  a_cols <- grep("^a[0-9]+$", names(irt_params), value = TRUE)
  
  if (length(a_cols) == 0) {
    cat("Dimensions: Unable to detect\n")
  } else {
    cat("Dimensions: ", length(a_cols), "\n")
  }
  
  cat("Items: ", nrow(irt_params), "\n\n")
  
  # -------------------------------------------------
  # Estimation
  # -------------------------------------------------
  cat("Estimation Method: ", parsed$method, "\n")
  cat("Selection Criteria: ", parsed$criteria, "\n")
  
  if (!is.null(parsed$start_item))
    cat("Start Item: ", parsed$start_item, "\n")
  
  cat("\n")
  
  # -------------------------------------------------
  # Prior
  # -------------------------------------------------
  if (!is.null(latent_means))
    cat("Latent Means: ", paste(latent_means, collapse = ", "), "\n")
  
  if (!is.null(latent_covariance)) {
    cat("Latent Covariance:\n")
    print(latent_covariance)
  }
  
  # -------------------------------------------------
  # Design parameters
  # -------------------------------------------------
  cat("\n--- Design ---\n")
  
  if (!is.null(design$min_SEM))
    cat("min_SEM: ", design$min_SEM, "\n")
  
  if (!is.null(design$delta_thetas))
    cat("delta_thetas: ", design$delta_thetas, "\n")
  
  if (!is.null(design$min_items))
    cat("min_items: ", design$min_items, "\n")
  
  if (!is.null(design$max_items))
    cat("max_items: ", design$max_items, "\n")
  
  if (!is.null(design$max_time))
    cat("max_time: ", design$max_time, "\n")
  
  if (!is.null(design$quadpts))
    cat("quadpts: ", design$quadpts, "\n")
  
  if (!is.null(design$theta_range))
    cat("theta_range: ", paste(design$theta_range, collapse = ", "), "\n")
  
  if (!is.null(design$weights))
    cat("weights: ", paste(design$weights, collapse = ", "), "\n")
  
  if (!is.null(design$KL_delta))
    cat("KL_delta: ", design$KL_delta, "\n")
  
  if (!is.null(design$content))
    cat("Content balancing active\n")
  
  if (!is.null(design$exposure))
    cat("Exposure control active\n")
  
  if (!is.null(design$constr_fun) &&
      length(body(design$constr_fun)) > 1)
    cat("Shadow CAT active\n")
  
  cat("==============================\n\n")
}


# --- Endpoint functions

irt_start_assessment <- function(req, res) {

  tryCatch({

    # ===============================
    # 1 PARSE
    # ===============================
    parsed <- parse_irt_request(req)
    cat('upper: ', parsed$questions$params$irt_upper_asymptote, '\n')

    # ===============================
    # 2 VALIDATION
    # ===============================
    validate_irt_request(parsed)

    # ===============================
    # 3 BUILD DOMAIN OBJECTS
    # ===============================
    constr_fun <- build_constr_fun(parsed$constr_fun_string)

    design <- list(
      min_SEM = parsed$min_sem,
      delta_thetas = parsed$delta_thetas,
      thetas.start = parsed$thetas_start,
      min_items = parsed$min_items,
      max_items = parsed$max_items,
      max_time = parsed$max_time,
      content = parsed$content,
      content_prop = parsed$content_prop,
      exposure = parsed$exposure,
      constr_fun = constr_fun,

      # NOVOS
      quadpts = parsed$quadpts,
      theta_range = parsed$theta_range,
      weights = parsed$weights,
      KL_delta = parsed$KL_delta
    )

    # ===============================
    # 4 BUILD IRT PARAMETERS
    # ===============================
    
    irt_params <- build_irt_parameters( 
      discrimination = parsed$questions$params$irt_discrimination,
      difficulty = parsed$questions$params$irt_difficulty,
      guessing = parsed$questions$params$irt_guess,
      upper = parsed$questions$params$irt_upper_asymptote,
      model_type = parsed$model
    )

    # Detect dimensionality
    is_multidim <- parsed$model %in% IRT_DOMAIN$models$multidimensional

    a_cols <- grep("^a[0-9]+$", names(irt_params), value = TRUE)
    n_dim <- length(a_cols)

    # Default latent means
    latent_means <- parsed$latent_means %||% rep(0, n_dim)

    # Default covariance
    if (!is.null(parsed$latent_covariance)) {
      latent_covariance <- as.matrix(parsed$latent_covariance)
    } else {
      latent_covariance <- if (is_multidim) diag(n_dim) else matrix(1)
    }

    mo <- create_mirt_object(
      item_type = normalize_irt_model(parsed$model),
      parameters = irt_params,
      latent_means = latent_means,
      latent_covariance = latent_covariance
    )

    # ===============================
    # 5 CREATE CAT DESIGN
    # ===============================
    cat_design <- create_cat_design(
      mo,
      start_item = parsed$start_item,
      criteria = parsed$criteria,
      method = parsed$method,
      design = design
    )

    cat_design$item_time_history <- list()
    cat_design$last_answer_time <- Sys.time()

    print_irt_configuration(
      parsed,
      latent_means,
      latent_covariance,
      irt_params,
      design
    )

    # ===============================
    # 6 FIRST ITEM SELECTION
    # ===============================
    next_index <- customNextItemIRT(
      person = cat_design$person,
      design = cat_design$design,
      test = cat_design$test,
      criteria = parsed$criteria
    )

    print_item_selection(next_index, stage = "START")

    # ===============================
    # 7 SUCCESS RESPONSE
    # ===============================
    res$status <- 200

    return(list(
      success = TRUE,
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      design = jsonlite::unbox(serialize_design(cat_design))
    ))

  }, error = function(e) {

    build_error_response(e, res)

  })
}

irt_next_item <- function(req, res) {

  tryCatch({

    # ===============================
    # 1 PARSE
    # ===============================
    parsed <- parse_irt_next_request(req)
    
    # ===============================
    # 2 VALIDATION
    # ===============================
    validate_irt_next_request(parsed)
    
    # ===============================
    # 3 UPDATE DESIGN
    # ===============================
    cat_design <- update_cat_design(
      design_serialized = parsed$design_serialized,
      previous_index = parsed$previous_index,
      answer = parsed$answer
    )
    
    # ===============================
    # 4 SELECT NEXT ITEM
    # ===============================
    next_index <- irt_select_next_item(cat_design)

    print_item_selection(next_index, stage = "NEXT")
    
    # ===============================
    # 5 SUCCESS RESPONSE
    # ===============================
    res$status <- 200

    return(list(
      success = TRUE,
      next_index = jsonlite::unbox(next_index),
      stop = jsonlite::unbox(cat_design$design@stop_now),
      design = jsonlite::unbox(serialize_design(cat_design))
    ))

  }, error = function(e) {

    build_error_response(e, res)

  })
}

