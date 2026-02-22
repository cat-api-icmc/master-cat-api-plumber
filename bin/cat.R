# ===============================
# API ERROR SYSTEM
# ===============================

new_api_error <- function(
  message,
  status = 400,
  type = "validation_error",
  details = NULL
) {
  
  structure(
    list(
      message   = as.character(message),
      status    = as.integer(status),
      type      = as.character(type),
      details   = details,
      call      = sys.call(-1),
      timestamp = Sys.time()
    ),
    class = c("api_error", "error", "condition")
  )
}

abort_bad_request <- function(msg, details = NULL) {
  stop(new_api_error(msg, 400, "bad_request", details), call. = FALSE)
}

abort_unprocessable <- function(msg, details = NULL) {
  stop(new_api_error(msg, 422, "unprocessable_entity", details), call. = FALSE)
}

build_error_response <- function(e, res) {
  
  tb <- paste(rev(sapply(sys.calls(), deparse)), collapse = "\n")
  
  if (inherits(e, "api_error")) {
    
    res$status <- e$status
    
    return(list(
      success = FALSE,
      error = list(
        type      = e$type,
        message   = e$message,
        details   = e$details,
        timestamp = as.character(e$timestamp),
        traceback = tb
      )
    ))
  }
  
  res$status <- 500
  
  return(list(
    success = FALSE,
    error = list(
      type      = "internal_error",
      message   = e$message,
      traceback = tb
    )
  ))
}


`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) b else a
}


parse_get_assessment_request <- function(req) {

  if (is.null(req$body))
    abort_bad_request("Request body is missing.")

  body <- req$body

  if (is.null(body$design))
    abort_bad_request("Field 'design' is required.")

  list(
    design_serialized = body$design
  )
}

extract_assessment_state <- function(design_serialized) {
  
  cat_design <- deserialize_design(design_serialized)
  
  person <- cat_design$person
  
  list(
    item_history = person$items_answered %||% NULL,
    response_history = person$responses %||% NULL,
    item_time_history = cat_design$item_time_history %||% NULL,
    last_answer_time = cat_design$last_answer_time %||% NULL,
    
    theta_history = person$thetas_history %||% NULL,
    standard_error_history = person$thetas_SE_history %||% NULL
  )
}
build_constr_fun <- function(constr_fun_string){

  if (is.null(constr_fun_string))
    return(function(){})

  if (!is.character(constr_fun_string))
    abort_bad_request("constr_fun must be a string.")

  if (!grepl("^function\\s*\\(", constr_fun_string))
    abort_unprocessable(
      "constr_fun must be a valid function definition."
    )

  tryCatch({
      eval(
        parse(text = constr_fun_string),
        envir = new.env(parent = baseenv())
      )
    },
    error = function(e)
      abort_unprocessable(
        "Invalid function syntax in constr_fun."
      )
  )
}

create_cat_design <- function(
    mirt_object,
    design,
    dataframe = NULL,
    method = "MAP",
    criteria = "seq",
    start_item = 1,
    ...
) {

  if (is.null(design))
    abort_bad_request("design must be provided.")

  mirtCAT(
    mo = mirt_object,
    dataframe = dataframe,
    start_item = start_item,
    criteria = criteria,
    design_elements = TRUE,
    method = method,
    design = design,
    ...
  )
}

print_item_selection <- function(next_index, stage = "NEXT") {
  
  cat("\n--- ITEM SELECTION (", stage, ") ---\n", sep = "")
  
  if (is.null(next_index)) {
    cat("Next item: NULL\n")
  } else if (identical(next_index, 0)) {
    cat("Test stopped (next_index = 0)\n")
  } else {
    cat("Next item selected:", next_index, "\n")
  }
  
  cat("-----------------------------------\n")
}

create_mirt_object <- function(
    parameters,
    item_type,
    latent_means = NULL,
    latent_covariance = NULL,
    key = NULL,
    min_category = 0
) {

  mirtCAT::generate.mirt_object(
    parameters = parameters,
    itemtype = item_type,
    latent_means = latent_means,
    latent_covariance = latent_covariance,
    key = key,
    min_category = min_category
  )
}

# --- Endpoint functions

get_assessment_data <- function(req, res) {
  
  tryCatch({
    
    # ===============================
    # 1 PARSE
    # ===============================
    parsed <- parse_get_assessment_request(req)
    
    # ===============================
    # 2 DESERIALIZE
    # ===============================
    cat_design <- deserialize_design(parsed$design_serialized)
    
    person <- cat_design$person
    
    # ===============================
    # 3 BUILD RESPONSE (MESMA ESTRUTURA ANTIGA)
    # ===============================
    item_history <- person$items_answered
    response_history <- person$responses
    
    last_answer_time <- jsonlite::unbox(cat_design$last_answer_time)
    
    item_time_history <- lapply(
      cat_design$item_time_history,
      function(x) jsonlite::unbox(x)
    )
    
    theta_history <- person$thetas_history
    standard_error_history <- person$thetas_SE_history
    
    res$status <- 200
    
    return(list(
      item_history = item_history,
      response_history = response_history,
      item_time_history = item_time_history,
      last_answer_time = last_answer_time,
      theta_history = theta_history,
      standard_error_history = standard_error_history
    ))
    
  }, error = function(e) {
    
    build_error_response(e, res)
    
  })
}

